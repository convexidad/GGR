install.packages("WDI")

library(WorldBankData)
library(WDI)
library(tidyverse)

# Get list of countries
wb_countries <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1985, end = 2023, extra = T) %>%
  distinct(country, iso2c, iso3c)
# Get GDP per capita data
wb_gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1985, end = 2023, extra = T) %>%
  select(iso2c, country, year, NY.GDP.PCAP.CD) %>%
  rename(date = year)
# Get foreign investment data
wb_foreigninvestment <- WDI(country = "all", indicator = "BX.KLT.DINV.WD.GD.ZS", start = 1985, end = 2023, extra = T) %>%
  select(iso2c, year, BX.KLT.DINV.WD.GD.ZS) %>%
  rename(date = year)
# Get poverty data
wb_poverty <- WDI(country = "all", indicator = "SI.POV.DDAY", start = 1985, end = 2023, extra = T) %>%
  select(iso2c, year, SI.POV.DDAY) %>%
  rename(date = year)
# Get life expectancy data
wb_lifeexpectancy <- WDI(country = "all", indicator = "SP.DYN.LE00.IN", start = 1985, end = 2023, extra = T) %>%
  select(iso2c, year, SP.DYN.LE00.IN) %>%
  rename(date = year)

world_bank_data



wb_foreigninvestment$date = as.character(wb_foreigninvestment$date)
wb_poverty$date = as.character(wb_poverty$date)
wb_lifeexpectancy$date = as.character(wb_lifeexpectancy$date)
wb_gdp$date = as.character(wb_gdp$date)

oghist = readxl::read_xls("Bases/OGHIST.xls", sheet = 3)
wb_incomeclass = oghist %>% pivot_longer(
  cols = 3:37,
  names_to = "date",
  values_to = "income_class")

wb_incomeclass$iso2c = countrycode::countrycode(wb_incomeclass$iso2c,
                                                origin = "iso3c",
                                                destination = "iso2c")

world_bank_data <- merge(wb_countries, wb_gdp, by = c("iso2c","country")) %>%
  left_join(wb_foreigninvestment, by = c("iso2c", "date")) %>%
  left_join(wb_poverty, by = c("iso2c", "date")) %>%
  left_join(wb_lifeexpectancy, by = c("iso2c", "date")) %>%
  left_join(wb_incomeclass, by = c("iso2c","date","country")) %>%
  rename(gdp = NY.GDP.PCAP.CD,
         foreign_invest = BX.KLT.DINV.WD.GD.ZS,
         poverty = SI.POV.DDAY,
         life_expect = SP.DYN.LE00.IN)

write.csv(world_bank_data,"Bases/world_bank_data.csv")  




