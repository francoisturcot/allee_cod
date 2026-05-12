options(scipen = 999)

#remove repeat stocks (shorter time series of 2j3kl and 4tvn)

df = df %>% filter(!stock %in% c("4tvn", "2j3kl"))

#standardize name formats


str(df)
df$stock[df$stock == "4tvn-SPM"] = "4TVn"
df$stock[df$stock == "2j3kl-SPM"] = "2J3KL"
df$stock[df$stock == "4vsw"] = "4VsW"

unique(res$stock)
res = res %>% filter(!stock %in% c("4tvn", "2j3kl"))
res$stock[res$stock == "4tvn-SPM"] = "4TVn"
res$stock[res$stock == "2j3kl-SPM"] = "2J3KL"
res$stock[res$stock == "4vsw"] = "4VsW"

df$stock  <- sub("^COD", "", df$stock)
res$stock <- sub("^COD", "", res$stock)

unique(df$stock)
unique(res$stock)
length(unique(df$stock))
length(unique(res$stock))

#get years

df %>%
    group_by(stock) %>%
    summarise(
        min_year = min(year, na.rm = TRUE),
        max_year = max(year, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    print(n = Inf)



#plots
df %>% ggplot(aes(year, biomass))+
    geom_line()+
    facet_wrap(.~stock, scales = "free")+
    theme_bw()+
    ylab("Biomass (kt)")+
    xlab("")


ggsave("../figures/biomass.png", width = 8, height = 6)

df %>% ggplot(aes(year, prod.rate))+
    geom_line()+
    facet_wrap(.~stock, scales = "free")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.8) +
    theme_bw()+
    ylab("Production rate")+
    xlab("")

ggsave("../figures/production rate.png", width = 8, height = 6)


df_plot <- df %>%
  group_by(stock) %>%
  mutate(
    max_biomass = max(biomass, na.rm = TRUE),
    
    below_20 = biomass < 0.2 * max_biomass,
    above_40 = biomass > 0.4 * max_biomass,
    
    collapsed = {
      x <- logical(n())
      
      for(i in seq_along(x)) {
        
        if(i == 1) {
          x[i] <- below_20[i]
        } else {
          
          # enter collapse state
          if(below_20[i]) {
            x[i] <- TRUE
            
            # remain collapsed until > 40%
          } else if(x[i - 1] && !above_40[i]) {
            x[i] <- TRUE
            
          } else {
            x[i] <- FALSE
          }
        }
      }
      
      x
    },
    
    ever_collapsed = cumany(collapsed),
    
    status = case_when(
      !ever_collapsed ~ "Initial",
      collapsed ~ "Collapsed",
      TRUE ~ "Recovered"
    )
  )
df_plot <- df_plot %>%
  group_by(stock) %>%
  mutate(
    biomass_scaled = (biomass - min(biomass, na.rm = TRUE)) /
      (max(biomass, na.rm = TRUE) - min(biomass, na.rm = TRUE))
  ) %>%
  ungroup()

ggplot(df_plot, aes(x = year, y = biomass_scaled)) +
  
  geom_line(color = "grey") +
  
  geom_line(aes(
    color = factor(
      status,
      levels = c("Initial", "Collapsed", "Recovered")
    ),
    group = 1
  )) +
  
  facet_wrap(~stock, scales = "free") +
  
  scale_color_manual(
    values = c(
      "Initial" = "#619CFF",
      "Collapsed" = "#F8766D",
      "Recovered" = "#00BA38"
    )
  ) +
  
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 3),
    labels = function(x) sprintf("'%02d", x %% 100)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  
  theme_bw() +
  
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 9),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  ) +
  
  labs(
    x = "Year",
    y = "Biomass (proportion of maximum)",
    color = "Status",
    title = ""
  )

ggsave("../figures/depletion.png", width = 6, height = 6)


#stocks table

table = data.frame(stock = stock,allee = NA, collapsed = NA, recovered = NA)
str(table)
str(meta)
table <- table %>%
    rowwise() %>%
    mutate(
        stocklong = meta$stocklong[str_detect(meta$stockid, fixed(stock))][1]
    ) %>%
    ungroup()

#write.csv(table, "table.csv")

#show all stocks

df_plot <- df %>%
    semi_join(res, by = "stock") %>%
    left_join(
        res %>% dplyr::select(stock, psi),
        by = "stock"
    ) %>%
    mutate(year = as.numeric(year))

ggplot(df_plot, aes(x = year, y = biomass)) +
    geom_line() +
    geom_point(size = 1) +
    geom_hline(aes(yintercept = psi),
               linetype = "dashed",
               linewidth = 0.8) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    labs(
        x = "Year",
        y = "Biomass"
    )

ggplot(df_plot, aes(x = biomass, y = prod.rate)) +
    geom_line() +
    geom_point(size = 1) +
    geom_vline(aes(xintercept = psi),
               linetype = "dashed",
               linewidth = 0.8) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    labs(
        x = "Year",
        y = "Production rate"
    )

df_plot <- df_plot %>%
    mutate(segment = ifelse(biomass <= psi, "before", "after"))

df_pred_lm_before <- df_plot %>%
    group_by(stock) %>%
    group_modify(~{
        df_after  <- filter(.x, biomass >= unique(.x$psi))
        df_before <- filter(.x, biomass <  unique(.x$psi))
        
        if (nrow(df_after) < 2 || nrow(df_before) == 0) {
            return(tibble())
        }
        
        fit <- lm(prod.rate ~ biomass, data = df_after)
        
        df_before %>%
            mutate(pred_lm_after = predict(fit, newdata = df_before))
    }) %>%
    ungroup()

ggplot(df_plot, aes(x = biomass, y = prod.rate)) +
    geom_point(size = 1, alpha = 0.7) +
    geom_line(alpha = 0.4) +
    
    # vertical psi line
    geom_vline(aes(xintercept = psi),
               linetype = "dashed",
               linewidth = 0.8) +
    
    # regressions by segment
    geom_smooth(
        aes(color = segment),
        method = "lm",
        se = FALSE,
        linewidth = 1
    ) +
    
    # dashed extrapolation of post-break lm into pre-break biomass
    geom_line(
        data = df_pred_lm_before,
        aes(x = biomass, y = pred_lm_after),
        linetype = "dashed",
        linewidth = 1,
        colour = "coral"
    ) +
    
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.8) +
    
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
        x = "Biomass",
        y = "Production rate",
        color = "Segment"
    )

ggsave("../figures/segmented_all_stocks.png", width = 10, height = 10)

#extract stock where the slope is positive before the breakpoint 
#and negative after (allee effect threshold)
#and where a segmented regression is better the a lm
stock_flip <- res %>%
    filter(lower > 0, higher < 0) %>% 
    filter(seg_AIC<lm_AIC)

stock_flip$recovery=NA
stock_flip$stock
stock_flip

#Allee effect strength As “The allee effect reduces production by x percent relative to normal conditions.”
df


#get corresponding stocks
df_plot <- df %>%
    semi_join(stock_flip, by = "stock") %>%
    mutate(year = as.numeric(year))   # ensure year is numeric for plotting

str(df_plot)

#extract psi and verify if they have not recovered from going under

df_plot <- df %>%
    semi_join(stock_flip, by = "stock") %>%
    left_join(
        stock_flip %>% dplyr::select(stock, psi),
        by = "stock"
    ) %>%
    mutate(year = as.numeric(year))

ggplot(df_plot, aes(x = year, y = biomass)) +
    geom_line() +
    geom_point(size = 1) +
    geom_hline(aes(yintercept = psi),
               linetype = "dashed",
               linewidth = 0.8) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    labs(
        x = "Year",
        y = "Biomass"
    )

ggplot(df_plot, aes(x = biomass, y = prod.rate)) +
    geom_line() +
    geom_point(size = 1) +
    geom_vline(aes(xintercept = psi),
               linetype = "dashed",
               linewidth = 0.8) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    labs(
        x = "Year",
        y = "Biomass"
    )

df_plot <- df_plot %>%
    mutate(segment = ifelse(biomass <= psi, "before", "after"))

df_pred_lm_before <- df_plot %>%
    group_by(stock) %>%
    # fit lm on the post-break segment
    do({
        df_after  <- filter(., biomass >= unique(.$psi))
        df_before <- filter(., biomass <  unique(.$psi))
        
        if (nrow(df_after) < 2 || nrow(df_before) == 0) return(tibble())
        
        fit <- lm(prod.rate ~ biomass, data = df_after)
        
        df_before %>%
            mutate(pred_lm_after = predict(fit, newdata = df_before))
    }) %>%
    ungroup()

ggplot(df_plot, aes(x = biomass, y = prod.rate)) +
    geom_point(size = 1, alpha = 0.7) +
    geom_line(alpha = 0.4) +
    
    # vertical psi line
    geom_vline(aes(xintercept = psi),
               linetype = "dashed",
               linewidth = 0.8) +
    
    # regressions by segment
    geom_smooth(
        aes(color = segment),
        method = "lm",
        se = FALSE,
        linewidth = 1
    ) +
    
    # dashed extrapolation of post-break lm into pre-break biomass
    geom_line(
        data = df_pred_lm_before,
        aes(x = biomass, y = pred_lm_after),
        linetype = "dashed",
        linewidth = 1,
        colour = "coral"
    ) +
    
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.8) +
    
    facet_wrap(~ stock, scales = "free", ncol = 3) +
  theme_bw() +
    theme(legend.position = "none") +
    labs(
        x = "Biomass (kt)",
        y = "Production rate (P/B)",
        color = "Segment"
    )

ggsave("../figures/segmented_flip_stocks.png", width = 6, height = 5)


res_only <- res %>%
    filter(!(stock %in% stock_flip$stock))

df_plot <- df %>%
    semi_join(res_only, by = "stock") %>%
    mutate(year = as.numeric(year))   # ensure year is numeric for plotting



df_plot <- df_plot %>%
    group_by(stock) %>%
    mutate(
        max_biomass = max(biomass, na.rm = TRUE),
        collapsed = biomass < 0.4 * max_biomass
    ) %>%
    ungroup()

summary(df_plot$collapsed)

df_plot <- df_plot %>%
    mutate(
        collapsed = factor(
            collapsed,
            levels = c(TRUE, FALSE),
            labels = c("Collapsed (<40% max SSB)", "Not collapsed")
        )
    )

ggplot(df_plot, aes(x = biomass, y = prod.rate)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~ stock, scales = "free") +
    geom_smooth(
        
        method = "lm",
        se = FALSE,
        linewidth = 1
    ) +
    theme_bw() +
    labs(
        x = "Year",
        y = "Biomass"
    )


ggplot(df_plot, aes(biomass, prod.rate, color = collapsed)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw()


ggplot(df_plot, aes(biomass, prod.rate)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free",ncol=3) +
    theme_bw()+
  ylab("Production rate (P/B)")+
  xlab("Biomass (kt)")

ggsave("../figures/lm.png", width = 7, height = 8)


#the criterias for a stock with a lm to be under allee effect is 
#slope not significantly negative
#or a significantly positive slope

#the lm significant slope test if the slope is different from zero. 
#if its not, its still depensation, as it is not compensation
#so everything that is not significant negative slope is depensation

res2 = res %>% filter(!stock %in% unique(stock_flip$stock))
res2
res2$type = NA

idx <- res2$slope3 < 0 #& res2$p3<0.05
res2$type[idx] <- "compensation"

idx <- res2$slope3 > 0 #& res2$p3<0.05
res2$type[idx] <- "depensation"

res2

idx <- res2$slope3 > 0 & res2$p3<0.05
res2$type[idx] <- "depensation"

res2


idx <- is.na(res2$type)
res2$type[idx] <- "depensation"


str(res2)

ggplot(df_plot, aes(biomass, prod.rate)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ stock, scales = "free",ncol=3) +
  theme_bw()+
  ylab("Production rate (P/B)")+
  xlab("Biomass (kt)")


library(dplyr)
library(ggplot2)

# split stocks by type
stocks_by_type <- split(res2$stock, res2$type)

plots <- lapply(names(stocks_by_type), function(tp) {
  
  df_sub <- df_plot %>% 
    filter(stock %in% stocks_by_type[[tp]])
  
  ggplot(df_sub, aes(biomass, prod.rate)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free", ncol = 3) +
    theme_bw() +
    ylab("Production rate (P/B)") +
    xlab("Biomass (kt)") #+
    #ggtitle(tp)
})

# view one
plots[[1]]
ggsave("../figures/lm- compensation stocks.png", width = 7, height = 8)

plots[[2]]
ggsave("../figures/lm- depensation stocks.png", width = 7, height = 6)

