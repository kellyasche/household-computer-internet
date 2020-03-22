library(tidyverse)


# regions -----------------------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc),
         edr = str_replace(edr, "  ", " "))



# Organize object ---------------------------------------------------------

data <- read_csv("Data/Computer and internet/acs-computer-internet-2017-county.csv") %>%
  select(2,3,4,8,10,12,14,20,22,28,30,44,46,48,50,52,54,56,58,60,62,72,74,76,78) %>%
  rename(countyfp = 1,
         county = 2,
         total.hh = 3,
         num.comp = 4,
         pct.comp = 5,
         num.desk = 6,
         pct.desk = 7,
         num.smrt = 8,
         pct.smrt = 9,
         num.tabl = 10,
         pct.tabl = 11,
         num.none = 12,
         pct.none = 13,
         num.intr = 14,
         pct.intr = 15,
         num.dial = 16,
         pct.dial = 17,
         num.brod = 18,
         pct.brod = 19,
         num.cell = 20,
         pct.cell = 21,
         num.satt = 22,
         pct.satt = 23,
         num.noin = 24,
         pct.noin = 25) %>%
  mutate(countyfp = str_sub(countyfp, 3,5),
         county = str_replace(county, " County, Minnesota", "")) %>%
  left_join(counties.regions[,c(1,3:6)], by = "countyfp")

data.ruca <- data %>%
  select(1:4,6,8,10,12,14,16,18,20,22,24,27) %>%
  gather(key = "type", value = "number", 3:14) %>%
  group_by(Dem_Desc, type) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  group_by(Dem_Desc) %>%
  mutate(pct.hh = number / number[type == "total.hh"]) %>%
  ungroup() %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"))

data.pr <- data %>%
  select(1:4,6,8,10,12,14,16,18,20,22,24,29) %>%
  gather(key = "type", value = "number", 3:14) %>%
  group_by(planning.region, type) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  group_by(planning.region) %>%
  mutate(pct.hh = number / number[type == "total.hh"]) %>%
  ungroup() %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

data.edr <- data %>%
  select(1:4,6,8,10,12,14,16,18,20,22,24,28,29) %>%
  gather(key = "type", value = "number", 3:14) %>%
  group_by(edr,planning.region, type) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  group_by(edr,planning.region) %>%
  mutate(pct.hh = number / number[type == "total.hh"]) %>%
  ungroup() %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))


# write csv ---------------------------------------------------------------

write_csv(data, "Data/Computer and internet/Master-computer-internet-2017-county.csv")

write_csv(data.ruca, "Data/Computer and internet/Master-computer-internet-2017-ruca.csv")

write_csv(data.pr, "Data/Computer and internet/Master-computer-internet-2017-pr.csv")

write_csv(data.edr, "Data/Computer and internet/Master-computer-internet-2017-edr.csv")
