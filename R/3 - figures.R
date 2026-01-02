df %>% ggplot(aes(year, biomass))+
    geom_line()+
    facet_wrap(.~stock, scales = "free")+
    theme_bw()+
    ylab("Biomass (kt)")+
    xlab("")

ggsave("biomass.png", width = 8, height = 6)

df %>% ggplot(aes(year, prod.rate))+
    geom_line()+
    facet_wrap(.~stock, scales = "free")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.8) +
    theme_bw()+
    ylab("Production rate")+
    xlab("")

ggsave("production rate.png", width = 8, height = 6)

df_plot <- df %>%
    group_by(stock) %>%
    mutate(
        max_biomass = max(biomass, na.rm = TRUE),
        collapsed = biomass < 0.4 * max_biomass
    ) %>%
    ungroup()

df_plot <- df_plot %>%
    mutate(
        collapsed = factor(
            collapsed,
            levels = c(TRUE, FALSE),
            labels = c("Collapsed (<40% max SSB)", "Not collapsed")
        )
    )

ggplot(df_plot, aes(biomass, prod.rate, color = collapsed)) +
    geom_point(size = 1) +
    #geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw()

ggplot(df_plot, aes(year, biomass, color = collapsed)) +
    geom_point(size = 1) +
    #geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw()+
    theme(legend.position = "none")

ggplot(df_plot, aes(year, biomass, color = collapsed)) +
    geom_point(size = 1) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
    )


ggsave("collapse.png", width = 8, height = 6)

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
        y = "Biomass"
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
stock_flip$recovery=c(0,0,0,0,0,1,0)
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
    
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
        x = "Biomass",
        y = "Production rate",
        color = "Segment"
    )

ggsave("../figures/segmented_flip_stocks.png", width = 10, height = 10)


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
    facet_wrap(~ stock, scales = "free") +
    theme_bw()

ggsave("../figures/lm.png", width = 8, height = 6)


allee_lm = c("2j3kl", "3Pn4RS", "4X5Y", "COD3M")
stock_lm =  res %>% filter(stock %in% allee_lm)
#stock_lm$recovery = c(0,0,0,1)
stock_lm    
stock_lm$As = NA

#allee = rbind(stock_flip,stock_lm)
#write.csv(allee, "../figures/allee_table.csv")
