library(tidyverse)
library(dplyr)
library(purrr)
library(segmented)
library(broom)  

options(scipen = 999)


### read NW atlantic cod stocks data

df = read.csv("data/cod table.csv")
df = df %>% dplyr::select(year, stock, catch, biomass)

df$biomass = df$biomass/1000
df$catch = df$catch/1000

#calculate surplus production
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
hist(df$prod.rate)

unique(df$stock)
#--------------------------------------------------
# Load RAM data
#--------------------------------------------------
dat  <- readRDS("data/RAM_dat.RDS")
meta <- readRDS("data/RAM_meta.RDS")

#--------------------------------------------------
# Filter cod stocks & compute production rate
#--------------------------------------------------
dat$biomass = dat$biomass/1000
dat$catch = dat$catch/1000
df2 <- dat %>%
  filter(str_detect(species, regex("cod", ignore_case = TRUE))) %>%
  filter(species != "SOLECWAGAB-COD") %>% 
  filter(species != "COD3NO")

plot(df2$biomass,df2$catch)


df2 = df2 %>% 
  group_by(species) %>%
  arrange(year) %>%
  mutate(
    sp        = catch + lead(biomass) - biomass,
    prod.rate = sp / biomass
  ) %>%
  ungroup() %>%
  drop_na(prod.rate, biomass)

df2$prod.rate

df_clean <- df %>%
  mutate(year = as.integer(year))

df2_clean <- df2 %>%
  rename(stock = species) %>%     # make the stock column "stock"
  mutate(year = as.integer(year))

#merge nw and ram stocks
df =rbind(df_clean, df2_clean)

#remove nw stocks from ram stocks
rm = c("COD2J3KL", "COD4TVn")
df = df %>% filter(!stock %in% rm)

#remove duplicates
rm = c("4tvn", "2J3KL")
df = df %>% filter(!stock %in% rm)
df$stock[df$stock == "4tvn-SPM"] = "4TVn"
df$stock[df$stock == "2j3kl-SPM"] = "2J3KL"
rm = c("2j3kl")
df = df %>% filter(!stock %in% rm)


#remove non atlantic cod stocks
unique(df$stock)
rm = c("COWCODSCAL","LINGCODNPCOAST", "LINGCODSPCOAST", "PCODBS", "PCODGA", "PATCODARGS", "PCODAI", "PCODNPAC" )
df = df %>% filter(!stock %in% rm)

#bad or insufficient data
df %>% filter(stock == "CODKAT")
rm = c("CODKAT", "COD3M")
df = df %>% filter(!stock %in% rm)


#fix names
df$stock[df$stock == "4vsw"] = "4VsW"

stock = unique(df$stock)
stock = sort(stock)
stock

meta$stock = meta$stockid
y = data.frame(stock)
x = left_join(y, meta, by = c("stock"))
x
write.csv(x, "figures/meta.csv")


#remove COD in front of names
df <- df %>%
  mutate(stock = str_remove(stock, "^COD"))



stock = unique(df$stock)
stock = sort(stock)
stock


df %>% filter(!stock %in% c("2J3KL", "4TVn")) %>% ggplot(aes(year, stock))+
  geom_point()+
  theme_bw()+
  xlab("")+
  ylab("Stock")

#ggsave("figures/time_series.png")
