source("1 - get data.R")
library(mgcv)

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

#perform segmented and linear regressions on each stocks

ns = length(unique(df$stock))
res = data.frame(stock = NA,  depensatory_lm = NA, depensatory_seg = NA, psi = NA, lower=NA, higher=NA, 
                 lm_AIC = NA, seg_AIC = NA, 
                 slope1 = NA, slope2 = NA, slope3 = NA,
                 p1 = NA,p2=NA,p3=NA, 
                 As = NA,
                 delta_0 = NA)


for (i in 1:ns){
    
    s = stock[i]
    temp = df %>% filter(stock == s)
    
    
    #gam
    #i=1
    #i=i+1
    #s = stock[i]
    #temp = df %>% filter(stock == s)
    #g1 = gam(prod.rate~s(biomass, k = 3), data = temp)
    #summary(g1)
    #plot.gam(g1)
    #s
    
    #lm
    model = lm(prod.rate~biomass, data = temp)
    depensatory_lm = coefficients(model)[2]>0
    
    #segmented
    segmented <- segmented(model, seg.Z = ~biomass)
    #save segmented regression breakpoint (phi)
    psi = segmented$psi[2]
    
    #save AIC
    t1 = AIC(model, segmented)
    t1$model = rownames(t1)
    m_AIC = t1 %>% filter(model == "model")
    s_AIC = t1 %>% filter(model == "segmented")
    
    
    # Add a column to indicate which segment each row belongs to
    temp <- temp %>%
        mutate(segment = case_when(
            biomass < psi ~ "before_break",
            biomass >= psi ~ "after_break"
        ))
    
    # Fit linear regressions before and after the break
    m1 <- lm(prod.rate ~ biomass, data = temp %>% filter(segment == "before_break"))
    m2 <- lm(prod.rate ~ biomass, data = temp %>% filter(segment == "after_break"))
    depensatory_seg = coefficients(m1)[2]>0
    
    #save lm slopes
    lower = as.numeric(m1$coefficients[2])
    higher = as.numeric(m2$coefficients[2])
    
    #predicted production rate at absolute minimum biomass from each segmented reg 
    pr_lm1 = predict(m1,data.frame(biomass = min(temp$biomass)))
    pr_lm2 = predict(m2,data.frame(biomass = min(temp$biomass)))
    
    #Allee effect strength
    #As = pr_lm1-pr_lm2 #delta production
    As = (pr_lm2-pr_lm1)/pr_lm2 # Proportional loss relative to normal production (bounded, sign‐aware)
    
    #delta production at minimum biomass to 0 (Allee threshold)
    delta_0 = pr_lm1
    
    #for stocks where a linear model is better, use
    if (s %in% c("2j3kl", "3Pn4RS", "4X5Y", "COD3M")) {
        delta_0 <-  predict(model,data.frame(biomass = min(temp$biomass)))
        
    }
    
    #extract standard slopes for all stocks by scaling biomass
    m1s <- lm(prod.rate ~ scale(biomass), data = temp %>% filter(segment == "before_break"))
    m2s <- lm(prod.rate ~ scale(biomass), data = temp %>% filter(segment == "after_break"))
    m3s <- lm(prod.rate ~ scale(biomass), data = temp)
    
    #save p-value for each slope
    p_slope1 <- summary(m1s)$coefficients["scale(biomass)", "Pr(>|t|)"]
    p_slope2 <- summary(m2s)$coefficients["scale(biomass)", "Pr(>|t|)"]
    p_slope3 <- summary(m3s)$coefficients["scale(biomass)", "Pr(>|t|)"]
    
    #save objects in a data.frame
    temp2  = data.frame(stock = s, psi = psi, lower=lower, higher=higher,
                        lm_AIC = m_AIC$AIC, seg_AIC = s_AIC$AIC, 
                        slope1 = as.numeric(m1s$coefficients[2]),
                        slope2 = as.numeric(m2s$coefficients[2]),
                        slope3 = as.numeric(m3s$coefficients[2]),
                        p1 = p_slope1,
                        p2 = p_slope2,
                        p3 = p_slope3,
                        As = As,
                        delta_0 = delta_0,
                        depensatory_lm = depensatory_lm,
                        depensatory_seg = depensatory_seg)

    res = rbind(res, temp2)
}

res = res[-1,]
res = merge(res,df_depletion, by = c("stock"))
write.csv(res, "../figures/slopes.csv")

