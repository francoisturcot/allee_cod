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


#show all stocks

df_plot <- df %>%
    semi_join(res, by = "stock") %>%
    left_join(
        res %>% dplyr::select(stock, psi),
        by = "stock"
    ) %>%
    mutate(year = as.numeric(year))


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


cod4t = df_plot %>% filter(stock == "4tvn")
min.b = min(cod4t$biomass)
psi = unique(cod4t$psi)
max.b = max(cod4t$biomass)
ymin_arrow <- -0.1226783
ymax_arrow <-  0.1835093


ggplot(cod4t, aes(x = biomass, y = prod.rate)) +
    
    # vertical psi line
    geom_vline(aes(xintercept = psi),
               linetype = "dashed",
               linewidth = 0.5,
               colour = "grey") +
    
    # regressions by segment
    geom_smooth(
        aes(colour = segment),
        method = "lm",
        se = FALSE,
        linewidth = 0.5#, 
        #colour = "black"
    ) +
    
    # dashed extrapolation of post-break lm into pre-break biomass
    geom_line(
        data = df_pred_lm_before %>% filter(stock == "4tvn"),
        aes(x = biomass, y = pred_lm_after),
        linetype = "dashed",
       # linewidth = 1,
        colour = "coral"
    ) +
    
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.5,
               colour = "grey") +
    
    
    theme_bw() +
    
    theme(
        legend.position = "none",
        panel.grid = element_blank()
        #axis.text.x  = element_blank(),
        #axis.ticks.x = element_blank()
    )+
    labs(
        x = "Biomass",
        y = "Production rate"#,
        #color = "Segment"
    )+
    
    annotate(
        "segment",
        x    = min.b,
        xend = min.b,
        y    = ymin_arrow,
        yend = ymax_arrow,
        linewidth = 0.5,
        arrow = arrow(
            ends   = "both",
            length = unit(0.25, "cm"),
            type   = "open"
        )
    )+
    
    annotate(
        "text",
        x     = 40000,
        y     = 0.11,
        label = "A[s]",
        parse = T,
        hjust = 0,
        size  = 5
    )+
    
    annotate(
        "text",
        x     = 40000,
        y     = 0.2,
        label = "pr[c]",
        parse = T,
        hjust = 0,
        size  = 4
    )+
    
    annotate(
        "text",
        x     = 40000,
        y     = -0.12,
        label = "pr[d]",
        parse = T,
        hjust = 0,
        size  = 4
    )+

    scale_x_continuous(
        breaks = c(min.b, psi, max.b),
        labels = c("min.b", "ψ", "max.b"),
        limits = c(-1,max.b)
        
        )


ggsave("../figures/theory.png", width = 4, height = 4)
