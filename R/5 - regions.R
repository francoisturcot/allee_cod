library(dplyr)
library(ggplot2)

dat <- tibble::tribble(
  ~stock, ~region, ~As, ~prod_lowB, ~depletion, ~allee, ~status,
  "4TVn","NW",1.22,-0.06,0.06,"Yes","Collapsed",
  "4VSw","NW",0.92,0.04,0.16,"Yes","Collapsed",
  "GB","NW",0.92,0.10,0.19,"Yes","Collapsed",
  "2J3KL","NW",0.89,0.03,0.04,"Yes","Collapsed",
  "WGOM","NW",0.80,0.43,0.10,"Yes","Collapsed",
  "3Ps","NW",0.55,0.25,0.27,"Yes","Collapsed",
  "1IN","NW",0.53,0.29,0.08,"Yes","Recovered",
  "3Pn4RS","NW",NA,0.03,0.28,"Yes","Collapsed",
  "4X5Y","NW",NA,0.03,0.25,"Yes","Collapsed",
  "3NO","NW",NA,0.14,0.05,"Yes","Collapsed",
  "1F-XIV","NW",NA,0.34,0.04,"No","Recovered",
  "5Zjm","NW",NA,0.31,0.13,"No","Collapsed",
  "3M","NW",NA,0.11,0.04,"No","Recovered",
  "EGOM","NW",NA,1.22,0.01,"No","Collapsed",
  "SNE","NW",NA,0.80,0.04,"No","Collapsed",
  "IIIaW-IV-VIId","NE",0.57,0.41,0.25,"Yes","Collapsed",
  "ICE","NE",NA,0.51,0.59,"No","Recovered",
  "BA2224","NE",NA,0.87,0.57,"No","Collapsed",
  "NEAR","NE",NA,0.51,0.41,"No","Recovered",
  "BA2532","NE",NA,0.69,0.26,"No","Collapsed",
  "FAPL","NE",NA,0.33,0.24,"No","Collapsed",
  "VIIek","NE",NA,0.73,0.22,"No","Collapsed",
  "VIa","NE",NA,0.74,0.14,"No","Collapsed",
  "IS","NE",NA,0.53,0.13,"No","Collapsed"
)

dat <- dat %>%
  mutate(
    allee_bin = ifelse(allee == "Yes", 1, 0),
    status_bin = ifelse(status == "Recovered", 1, 0)
  )

str(dat)

dat %>% ggplot(aes(as.factor(allee_bin), prod_lowB))+
  geom_boxplot()

dat %>% ggplot(aes(as.factor(allee_bin), depletion))+
  geom_boxplot()

dat %>% ggplot(aes(as.factor(region), prod_lowB))+
  geom_boxplot()

dat %>% ggplot(aes(as.factor(region), depletion))+
  geom_boxplot()

mod_prod <- aov(prod_lowB ~ region, data = dat)
summary(mod_prod)

mod_dep <- aov(depletion ~ region, data = dat)
summary(mod_dep)

shapiro.test(residuals(mod_prod))
shapiro.test(residuals(mod_dep))

bartlett.test(prod_lowB ~ region, data = dat)
bartlett.test(depletion ~ region, data = dat)

wilcox.test(prod_lowB ~ region, data = dat)
wilcox.test(depletion ~ region, data = dat)

ggplot(dat, aes(region, prod_lowB)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Region", y = "Mean production at low biomass")

ggplot(dat, aes(region, depletion)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Region", y = "Depletion")



dat %>% ggplot(aes(as.factor(status_bin), prod_lowB))+
  geom_boxplot()

dat %>% ggplot(aes(as.factor(status_bin), depletion))+
  geom_boxplot()
