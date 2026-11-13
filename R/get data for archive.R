library(tidyverse)

options(scipen = 999)

#--------------------------------------------------
# 1. NW Atlantic cod stocks (DFO table)
#--------------------------------------------------
df <- read.csv("data/cod table.csv")
df <- df %>% select(year, stock, catch, biomass)

df$biomass <- df$biomass / 1000
df$catch   <- df$catch / 1000

df$year <- as.integer(df$year)

#--------------------------------------------------
# 2. RAM Legacy cod stocks
#--------------------------------------------------
dat  <- readRDS("data/RAM_dat.RDS")
meta <- readRDS("data/RAM_meta.RDS")

dat$biomass <- dat$biomass / 1000
dat$catch   <- dat$catch / 1000

df2 <- dat %>%
  filter(str_detect(species, regex("cod", ignore_case = TRUE))) %>%
  filter(species != "SOLECWAGAB-COD") %>%
  filter(species != "COD3NO")

df2 <- df2 %>% rename(stock = species)
df2 <- df2 %>% select(year, stock, catch, biomass)

df2$year <- as.integer(df2$year)

#--------------------------------------------------
# 3. Merge sources
#--------------------------------------------------
df <- rbind(df, df2)

#--------------------------------------------------
# 4. Remove duplicates and harmonize names
#--------------------------------------------------
# RAM versions of NW stocks
df <- df %>% filter(!stock %in% c("COD2J3KL", "COD4TVn"))

# duplicate DFO versions
df <- df %>% filter(!stock %in% c("4tvn", "2J3KL"))
df$stock[df$stock == "4tvn-SPM"]  <- "4TVn"
df$stock[df$stock == "2j3kl-SPM"] <- "2J3KL"
df <- df %>% filter(stock != "2j3kl")

# non-Atlantic cod stocks
non_atl <- c("COWCODSCAL", "LINGCODNPCOAST", "LINGCODSPCOAST",
             "PCODBS", "PCODGA", "PATCODARGS", "PCODAI", "PCODNPAC")
df <- df %>% filter(!stock %in% non_atl)

# bad or insufficient data
df <- df %>% filter(!stock %in% c("CODKAT", "COD3M"))

# fix names
df$stock[df$stock == "4vsw"] <- "4VsW"

#--------------------------------------------------
# 5. Stock metadata (before removing COD prefix, to match RAM stockid)
#--------------------------------------------------
stock_list <- data.frame(stock = sort(unique(df$stock)))
meta$stock <- meta$stockid
meta_out <- left_join(stock_list, meta, by = "stock")
write.csv(meta_out, "data/cod_meta.csv", row.names = FALSE)

#--------------------------------------------------
# 6. Final tidy-up and save
#--------------------------------------------------
df <- df %>% mutate(stock = str_remove(stock, "^COD"))
df <- df %>% arrange(stock, year)

sort(unique(df$stock))
str(df)

saveRDS(df, "data/cod_clean.RDS")