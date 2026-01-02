library(tidyverse)
library(dplyr)
library(purrr)
library(segmented)
library(broom)  

df = read.csv("cod table.csv")
df$ssb = NULL
#calculatNULL#calculate surplus production
df$sp = NA #prepare vector
str(df)

df <- df %>%
    group_by(stock) %>%              # compute within each stock
    arrange(year) %>%                  # ensure data is ordered by year
    mutate(
        sp = catch + lead(biomass) - biomass   # lead(biomass) is biomass_{t+1}
    ) %>%
    ungroup()

df = na.omit(df)
df$prod.rate = df$sp/df$biomass

df %>% ggplot(aes(year, biomass))+
    geom_line()+
    facet_wrap(.~stock, scales = "free")+
    theme_bw()+
    ylab("Biomass (tonnes)")+
    xlab("")

ggsave("biomass.png")

df %>% ggplot(aes(year, prod.rate))+
    geom_line()+
    facet_wrap(.~stock, scales = "free")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.8) +
    theme_bw()+
    ylab("Production rate")+
    xlab("")

ggsave("production rate.png")



#find a break-point using piece wise regression
#fit lm to each segment
stock = unique(df$stock)
stock = sort(stock)
ns = length(unique(df$stock))
res = data.frame(stock = NA,  psi = NA, lower=NA, higher=NA, lm_AIC = NA, seg_AIC = NA )

#4t cod is 47

for (i in 1:ns){
    
    #i = 47
    s = stock[i]
    temp = df %>% filter(stock == s)
    #plot(temp$biomass, temp$prod.rate)
    #temp$prod.rate = scale(temp$prod.rate)
    #temp$biomass = scale(temp$biomass)
    
    model = lm(prod.rate~biomass, data = temp)
    segmented <- segmented(model, seg.Z = ~biomass)
    t1 = AIC(model, segmented)
    t1$model = rownames(t1)
    
    # Plot the original data with the fitted model
    #seg_preds <- predict(segmented)
    #seg_res <- temp$prod.rate - seg_preds
    
    #plot(
    #    temp$biomass, temp$prod.rate,
    #    main = "Piecewise Regression Fit",
    #    xlab = "Independent Variable (x)",
    #    ylab = "Dependent Variable (y)",
    #    col = "blue"
    #)
    #lines(temp$biomass, seg_preds,col = "red", lwd = 2)
    
    psi = segmented$psi[2]
    #plot(temp$year, temp$biomass)
    #abline(h = psi)
    
    # Add a column to indicate which segment each row belongs to
    temp <- temp %>%
        mutate(segment = case_when(
            biomass < psi ~ "before_break",
            biomass >= psi ~ "after_break"
        ))
    
    
    # Fit the two regressions 
    m1 <- lm(prod.rate ~ biomass, data = temp %>% filter(segment == "before_break"))
    m2 <- lm(prod.rate ~ biomass, data = temp %>% filter(segment == "after_break"))
    
    lower = as.numeric(m1$coefficients[2])
    higher = as.numeric(m2$coefficients[2])
    
    m_AIC = t1 %>% filter(model == "model")
    s_AIC = t1 %>% filter(model == "segmented")
    
    temp2  = data.frame(stock = s, psi = psi, lower=lower, higher=higher,
                       lm_AIC = m_AIC$AIC, seg_AIC = s_AIC$AIC)
    #temp2
    res = rbind(res, temp2)
}
res = res[-1,]

#extract stock where the slope is positive before the breakpoint 
#and negative after (allee)
#and where a segmented regression is better the a lm
stock_flip <- res %>%
    #filter(lower > 0, higher < 0) %>% 
    filter(seg_AIC<lm_AIC)

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
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               linewidth = 0.8) +
    
    facet_wrap(~ stock, scales = "free") +
    theme_bw() +
    theme(legend.position = "none")+
    labs(
        x = "Biomass",
        y = "Production rate",
        color = "Segment"
    )
ggsave("allee_all.png", width = 10, height = 10)

#now show which ones have recovered from going under vs the ones that dont.

#define collapsed - below 90% of max ssb
str(df_plot)
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
    ylab("Biomass (tonnes)")+
    xlab("")+
    theme(legend.position = "none")

ggsave("recovery.png")

#for stocks that crossed the allee thresold 
#has the stock crossed a proxy for bmsy
#has it recovered from it
#how strong was the allee effect? 
#use the slope of the regression instead of using 0 as a threshold?



#get cod stocks from eastern canada
#use schjins for 2j3kl
#use 4t bsm model catch and biomass
#3pn
#ngsl cod
#gulf of maine cod

















#show stocks were prod rate is increasing as biomass declines as expected for comparison
norm <- res %>%
    filter %>% filter(seg_AIC>lm_AIC)

#get corresponding stocks
df_plot <- df %>%
    semi_join(norm, by = "stock") %>%
    mutate(year = as.numeric(year))   # ensure year is numeric for plotting

str(df_plot)



ggplot(df_plot, aes(x = year, y = biomass)) +
    geom_line() +
    geom_point(size = 1) +
    #facet_wrap(~ stock, scales = "free_y") +
    theme_bw() +
    labs(
        x = "Year",
        y = "Biomass"
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

ggsave("2j3kl.png", width = 30, height = 30)



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


ggplot(df_plot, aes(biomass, prod.rate, color = collapsed)) +
    geom_point(size = 1) +
    #geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw()

ggplot(df_plot, aes(year, biomass, color = collapsed)) +
    geom_point(size = 1) +
    #geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ stock, scales = "free") +
    theme_bw()


# are these stocks recovering better or not

# to do 

#add recent years to 4tvn spm and run again
#2j3kl glm is better than segmented but slope is negative = allee all along, show differently