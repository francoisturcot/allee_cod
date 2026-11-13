library(tidyverse)
library(segmented)

source("R/1 - get data.R")

# calculate lrp as 40% max biomass
# calculate level of depletion as minimum biomass / lrp

df_depletion <- df %>%
  group_by(stock) %>%
  summarise(
    max_biomass = max(biomass, na.rm = TRUE),
    lrp         = 0.4 * max_biomass,
    min_biomass = min(biomass, na.rm = TRUE),
    depletion   = min_biomass / lrp,
    .groups = "drop"
  )


str(df)

df <- df %>%
  group_by(stock) %>%
  mutate(biomass_scaled = biomass / max(biomass, na.rm = TRUE)) %>%
  ungroup()

df$biomass = df$biomass_scaled

# fit segmented + lm per stock
fits <- map(sort(unique(df$stock)), function(s) {
  temp <- df %>% filter(stock == s)
  m    <- lm(prod.rate ~ biomass, data = temp)
  sm   <- try(segmented(m, seg.Z = ~ biomass), silent = TRUE)
  if (inherits(sm, "try-error") || is.null(sm$psi)) return(NULL)
  b1 <- as.numeric(coef(sm)[2])
  list(stock  = s, data = temp, mod = sm, psi = as.numeric(sm$psi[2]),
       slope1 = b1, slope2 = b1 + as.numeric(coef(sm)[3]),
       lm_AIC = AIC(m), seg_AIC = AIC(sm))
}) %>% compact()

# keep depensatory stocks (+ before, - after) where segmented beats lm
sel <- fits %>% keep(~ .x$slope1 > 0 & .x$slope2 < 0 & .x$seg_AIC < .x$lm_AIC)

# observations
df_plot <- map_dfr(sel, function(f) {
  f$data %>% mutate(psi = f$psi,
                    segment = if_else(biomass < f$psi, "before_break", "after_break"))
})

# continuous segmented fit, split at psi for colouring
df_seg <- map_dfr(sel, function(f) {
  b1 <- c(seq(min(f$data$biomass), f$psi, length.out = 100))
  b2 <- c(seq(f$psi, max(f$data$biomass), length.out = 100))
  bind_rows(tibble(biomass = b1, segment = "before_break"),
            tibble(biomass = b2, segment = "after_break")) %>%
    mutate(stock = f$stock,
           prod.rate = predict(f$mod, data.frame(biomass = biomass)))
})

# post-break slope extrapolated below psi
df_pred_lm_before <- map_dfr(sel, function(f) {
  b <- seq(min(f$data$biomass), f$psi, length.out = 100)
  tibble(stock = f$stock, biomass = b,
         pred_lm_after = predict(f$mod, data.frame(biomass = f$psi)) + f$slope2 * (b - f$psi))
})

p <- ggplot(df_plot, aes(x = biomass, y = prod.rate)) +
  
  geom_point(size = 1, alpha = 0.7) +
  geom_line(alpha = 0.4) +
  
  geom_vline(
    aes(xintercept = psi),
    colour = "grey",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  geom_line(
    data = df_seg,
    aes(colour = segment),
    linewidth = 1
  ) +
  
  geom_line(
    data = df_pred_lm_before,
    aes(x = biomass, y = pred_lm_after),
    linetype = "dashed",
    linewidth = 1,
    colour = "coral"
  ) +
  
  geom_hline(
    aes(yintercept = 0),
    linetype = "dashed",
    colour = "grey",
    linewidth = 0.8
  ) +
  
  facet_wrap(~ stock, ncol = 4, scales = "free_y" ) +
  
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(0, 1),
    labels = c("0", "0.25", "0.50", "0.75", "1")
  ) +
  
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    x = "Biomass (proportion of maximum)",
    y = "Relative surplus production"
  )


p
# 
# ggsave(
#   "figures/segmented_flip_stocks.tiff",
#   plot = p,
#   width = 170,
#   height = 100,
#   units = "mm",
#   dpi = 600,
#   compression = "lzw"
# )

p


#calculate the allee effect strength

str(df_seg)
str(df_pred_lm_before)

prod_min <- df_seg %>%
  group_by(stock) %>%
  slice_min(biomass, n = 1, with_ties = FALSE) %>%
  transmute(
    stock,
    biomass,
    prod_segmented = prod.rate
  ) %>%
  left_join(
    df_pred_lm_before %>%
      group_by(stock) %>%
      slice_min(biomass, n = 1, with_ties = FALSE) %>%
      transmute(
        stock,
        prod_extrapolated = pred_lm_after
      ),
    by = "stock"
  )

prod_min <- prod_min %>%
  mutate(
    As = (prod_extrapolated - prod_segmented)/prod_extrapolated
      )%>%
  arrange(desc(As))

prod_min %>% dplyr::select(stock, prod_segmented, As)




##### depletion
library(patchwork)

nw_stocks <- c("2J3KL", "3NO", "3M", "3Pn4RS", "3Ps", "4TVn", "4VsW",
               "4X5Y", "5Zjm", "GB", "EGOM", "WGOM", "SNE", "1F-XIV", "1IN")
ne_stocks <- c("NEAR", "ICE", "FAPL", "IS", "IIIaW-IV-VIId",
               "BA2532", "BA2224", "VIa", "VIIek")

stock_order <- c(nw_stocks, ne_stocks)
stock_order <- stock_order[stock_order %in% unique(df$stock)]


# Common plot function
make_panel <- function(data, title) {
  ggplot(data, aes(x = year, y = biomass_scaled)) +
    geom_line() +
    geom_hline(yintercept = 0.2, color = "#C44E52") +
    geom_hline(yintercept = 0.4, color = "#009E73") +
    facet_wrap(~stock, scales = "free_x", ncol = 4) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text = element_text(size = 7)
    ) +
    ggtitle(title)
}

# all stocks for the depletion figure
df_dep <- df

df_dep$region <- ifelse(df_dep$stock %in% nw_stocks,
                        "Northwest Atlantic", "Northeast Atlantic")

p_nw <- make_panel(filter(df_dep, region == "Northwest Atlantic"), "Northwest Atlantic")
p_ne <- make_panel(filter(df_dep, region == "Northeast Atlantic"), "Northeast Atlantic")

n_nw <- sum(unique(df_dep$stock) %in% nw_stocks)
n_ne <- sum(unique(df_dep$stock) %in% ne_stocks)

p_combined <- p_nw / p_ne +
  plot_layout(heights = c(ceiling(n_nw / 4), ceiling(n_ne / 4))) +
  plot_annotation(caption = "Year",
                  theme = theme(plot.caption = element_text(hjust = 0.5, size = 9))) &
  theme(plot.title = element_text(size = 10, face = "bold"))

# Add shared y-axis label
p_final <- wrap_elements(
  grid::textGrob("Biomass (proportion of maximum)", rot = 90, gp = grid::gpar(fontsize = 9))
) + p_combined + plot_layout(widths = c(1, 25))


p_final 

#ggsave("figures/depletion.png", p_final, width = 7, height = 9)

# ggsave(
#   "figures/depletion.tiff",
#   plot = p_final,
#   width = 170,
#   height = 100,
#   units = "mm",
#   dpi = 600,
#   compression = "lzw"
# )
