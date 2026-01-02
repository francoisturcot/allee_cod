library(tidyverse)
library(dplyr)
library(purrr)
library(segmented)
library(broom)  


### read NW atlantic cod stocks data

df = read.csv("../data/cod table.csv")
df$ssb = NULL

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


#--------------------------------------------------
# Load RAM data
#--------------------------------------------------
dat  <- readRDS("../data/RAM_dat.RDS")
meta <- readRDS("../data/RAM_meta.RDS")



#--------------------------------------------------
# Filter cod stocks & compute production rate
#--------------------------------------------------
df2 <- dat %>%
    filter(str_detect(species, regex("cod", ignore_case = TRUE))) %>%
    filter(species != "SOLECWAGAB-COD") %>%
    group_by(species) %>%
    arrange(year) %>%
    mutate(
        sp        = catch + lead(biomass) - biomass,
        prod.rate = sp / biomass
    ) %>%
    ungroup() %>%
    drop_na(prod.rate, biomass)


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

#remove non atlantic cod stocks
unique(df$stock)
rm = c("COWCODSCAL","LINGCODNPCOAST", "LINGCODSPCOAST", "PCODBS", "PCODGA", "PATCODARGS", "PCODAI", "PCODNPAC" )
df = df %>% filter(!stock %in% rm)

#bad or insufficient data
df %>% filter(stock == "CODKAT")
rm = c("CODKAT", "COD3M")
df = df %>% filter(!stock %in% rm)

stock = unique(df$stock)
stock = sort(stock)
stock

meta$stock = meta$stockid
y = data.frame(stock)
x = left_join(y, meta, by = c("stock"))
write.csv(x, "../figures/meta.csv")
