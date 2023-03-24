library(WDI)
library(tidyverse)
library(dplyr)
library(ggplot2)

######### Creación world_bank ####
# Get list of countries
wb_countries <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1965, end = 2023, extra = T) %>%
  distinct(country, iso2c, iso3c)
# Get GDP per capita data
wb_gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1965, end = 2023, extra = T) %>%
  select(iso2c, country, year, NY.GDP.PCAP.CD) %>%
  rename(date = year)
# Get foreign investment data
wb_foreigninvestment <- WDI(country = "all", indicator = "BX.KLT.DINV.WD.GD.ZS", start = 1965, end = 2023, extra = T) %>%
  select(iso2c, year, BX.KLT.DINV.WD.GD.ZS) %>%
  rename(date = year)
# Get poverty data
wb_poverty <- WDI(country = "all", indicator = "SI.POV.DDAY", start = 1965, end = 2023, extra = T) %>%
  select(iso2c, year, SI.POV.DDAY) %>%
  rename(date = year)
# Get life expectancy data
wb_lifeexpectancy <- WDI(country = "all", indicator = "SP.DYN.LE00.IN", start = 1965, end = 2023, extra = T) %>%
  select(iso2c, year, SP.DYN.LE00.IN) %>%
  rename(date = year)

# Get Income classification data
oghist = readxl::read_xls("Bases/OGHIST.xls", sheet = 3)
wb_incomeclass = oghist %>% pivot_longer(
  cols = 3:37,
  names_to = "date",
  values_to = "income_class") %>%
  mutate(iso2c = countrycode::countrycode(iso3c, 
                                          origin = "iso3c", 
                                          destination = "iso2c"),
         date = date %>% as.integer())

world_bank_data <- merge(wb_countries, wb_gdp, by = c("iso2c","country")) %>%
  left_join(wb_foreigninvestment, by = c("iso2c", "date")) %>%
  left_join(wb_poverty, by = c("iso2c", "date")) %>%
  left_join(wb_lifeexpectancy, by = c("iso2c", "date")) %>%
  left_join(wb_incomeclass, by = c("iso3c","date","country")) %>%
  rename(gdp = NY.GDP.PCAP.CD,
         foreign_invest = BX.KLT.DINV.WD.GD.ZS,
         poverty = SI.POV.DDAY,
         life_expect = SP.DYN.LE00.IN) %>%
  mutate(republican = ifelse(date %in% c(1985:1992,
                                         2001:2008,
                                         2017:2020),1,0),
         income_class = ifelse(income_class=="..",NA,
                               ifelse(income_class=="LM*","LM",income_class)),
         year = date %>% as.double())


write.csv(world_bank_data,"Bases/world_bank_data.csv")  

######### Creación USAID ####

## Interesante de manera descriptiva. 
## Pensar aplicar técnicas de webscrapping con el GGR.
## Hacer una primera evaluación si es factible hacerlo.



usaid = readxl::read_xlsx("Bases/USAID.xlsx", sheet = 2)
usaid = usaid %>% 
  mutate(iso3c = countrycode::countrycode(Recipient, 
                                            origin = "country.name", 
                                            destination = "iso3c"),
         year = Year %>% as.double()) 

usaid = usaid %>%
  left_join(world_bank_data,by=c("iso3c","year"))

write.csv(usaid,"Bases/usaid.csv")  


######### Creación base IHME ####

ihme = read.csv("Bases/IHME_DAH.csv")
ihme = ihme %>% 
  filter(source=="United_States") %>%
  mutate(iso3c = recipient_isocode,
         country = recipient_country,
         year = year %>% as.double()) %>%
  left_join(world_bank_data, by=c("country","year","iso3c"))


write.csv(ihme,"Bases/ihme.csv")  

######### Creación Foreign Budget ####
foreign_budget = read.csv("Bases/us_foreign_budget.csv")

foreign = foreign_budget %>% 
  filter(US.Sector.ID==16,
         Income.Group.Name!="NULL") %>%
  mutate(year = Fiscal.Year %>% as.double(),
         iso3c = Country.Code,
         country = Country.Name) %>%
  left_join(world_bank_data,by=(c("iso3c","year","country")))



# Ver análisis global con varios datos. Comparar los outcomes considerando eso.

##### Gráficos ####
######### Foreign ####
graf_foreign_total = foreign %>%
  filter(republican %in% 0:1,
         Income.Group.Name!="Upper Middle Income Country") %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"))%>%
  ggplot(aes(x = year, y = log(constant_amount))) +
  geom_smooth(method = "lm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Maternal and Child Health",
    y = "Log(Constant Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) 

graf_foreign = foreign %>%
  filter(republican %in% 0:1,
         Income.Group.Name!="Upper Middle Income Country") %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"))%>%
  ggplot(aes(x = year, y = log(constant_amount), colour = partido)) +
  geom_smooth(method = "lm") +
# facet_grid(~Income.Group.Name) + 
#  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Maternal and Child Health",
    y = "Log(Constant Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) 
  
graf_foreign_group = foreign %>%
  filter(republican %in% 0:1,
         Income.Group.Name!="Upper Middle Income Country") %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"))%>%
  ggplot(aes(x = year, y = log(constant_amount), colour = partido)) +
  geom_smooth(method = "lm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Maternal and Child Health",
    y = "Log(Constant Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) +
  facet_grid(~Income.Group.Name)

###
  graf_foreign_total2 = foreign %>%
  filter(republican %in% 0:1,
         Income.Group.Name!="Upper Middle Income Country") %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"))%>%
  ggplot(aes(x = year, y = log(current_amount))) +
  geom_smooth(method = "lm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Maternal and Child Health",
    y = "Log(current Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) 

graf_foreign2 = foreign %>%
  filter(republican %in% 0:1,
         Income.Group.Name!="Upper Middle Income Country") %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"))%>%
  ggplot(aes(x = year, y = log(current_amount), colour = partido)) +
  geom_smooth(method = "lm") +
# facet_grid(~Income.Group.Name) + 
#  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Maternal and Child Health",
    y = "Log(current Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) 
  
graf_foreign_group2 = foreign %>%
  filter(republican %in% 0:1,
         Income.Group.Name!="Upper Middle Income Country") %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"))%>%
  ggplot(aes(x = year, y = log(current_amount), colour = partido)) +
  geom_smooth(method = "lm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Maternal and Child Health",
    y = "Log(current Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) +
  facet_grid(~Income.Group.Name)

######### USAID #####

usaid$purpose1 = usaid$`Code Round1: Purpose` 

graf_usaid = usaid %>%
  filter(republican %in% 0:1) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         health = purpose1 %in% c("13020: Reproductive health care",
                                                "13030: Family planning"))%>%
  ggplot(aes(x = year, y = health, colour = partido)) +
  geom_smooth(method = "glm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "US Foreign Aid (2006-2022)",
    subtitle = "Health and Population",
    y = "Log(Constant Amount)",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right")) + facet_wrap(~partido)

graf_usaid

######### IHME ####
ihme = ihme %>%
  mutate(rmh_dah_20 = rmh_dah_20 %>% as.double(),
         dah_20 = dah_20 %>% as.double())

graf_ihme_total = ihme %>%
  filter(republican %in% 0:1) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         rmh_ratio = rmh_dah_20) %>%
  ggplot(aes(x = year, y = rmh_ratio, colour = partido)) +
  geom_smooth(method = "glm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    y = "Funds for RMH",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right"))


graf_ihme_ratio = ihme %>%
  filter(republican %in% 0:1) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         rmh_ratio = rmh_dah_20/dah_20 ) %>%
  ggplot(aes(x = year, y = rmh_ratio, colour = partido)) +
  geom_smooth(method = "glm") +
  # facet_grid(~Income.Group.Name) + 
  #  geom_point() +
  labs(
    title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    subtitle = "Ratio of the total funds for health",
    y = "Funds for RMH / Total funds for health",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right"))

graf_ihme_ratio_group = ihme %>%
  filter(republican %in% 0:1,
         income_class %in% c("L","LM")) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         rmh_ratio = rmh_dah_20/dah_20,
         income_class = ifelse(income_class=="L","Low Income",
                               ifelse(income_class=="LM","Lower Middle Income",NA))) %>%
  ggplot(aes(x = year, y = rmh_ratio, colour = partido)) +
  geom_smooth(method = "glm") +
   facet_grid(~income_class) + 
  #  geom_point() +
  labs(
    title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    subtitle = "Ratio of the total funds for health",
    y = "Funds for RMH / Total funds for health",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right"))

graf_ihme_total_group = ihme %>%
  filter(republican %in% 0:1,
         income_class %in% c("L","LM")) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         rmh_ratio = rmh_dah_20,
         income_class = ifelse(income_class=="L","Low Income",
                               ifelse(income_class=="LM","Lower Middle Income",NA))) %>%
  ggplot(aes(x = year, y = rmh_ratio, colour = partido)) +
  geom_smooth(method = "glm") +
  facet_grid(~income_class) + 
  #  geom_point() +
  labs(
    title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    y = "Funds for RMH",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right"))

graf_ihme_ratio_general = ihme %>%
  filter(republican %in% 0:1,
         income_class %in% c("L","LM"),
         rmh_dah_20>0,
         year>=1992) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         rmh_ratio = rmh_dah_20/dah_20,
         income_class = ifelse(income_class=="L","Low Income",
                               ifelse(income_class=="LM","Lower Middle Income",NA))) %>%
  ggplot(aes(x = year, y = rmh_ratio)) +
  geom_smooth() +
#  facet_grid(~income_class) + 
  labs(
    title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    subtitle = "Ratio of the total funds for health",
    y = "Funds for RMH / Total funds for health",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right"))

graf_ihme_total_general = ihme %>%
  filter(republican %in% 0:1,
         income_class %in% c("L","LM"),
         rmh_dah_20>0,
         year>=1992) %>%
  mutate(partido = ifelse(republican==0,"Demócrata","Republicano"),
         rmh_ratio = rmh_dah_20,
         income_class = ifelse(income_class=="L","Low Income",
                               ifelse(income_class=="LM","Lower Middle Income",NA))) %>%
  ggplot(aes(x = year, y = rmh_ratio)) +
  geom_smooth() +
  #  facet_grid(~income_class) + 
  labs(
    title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    subtitle = "",
    y = "Funds for RMH",
    x = "Year"
  ) +
  theme_classic() +
  theme(legend.position  = c("right"))



graf_foreign_total
graf_foreign
graf_foreign_group

graf_foreign_total2
graf_foreign2
graf_foreign_group2


graf_ihme_ratio_general
graf_ihme_ratio
graf_ihme_ratio_group

graf_ihme_total_general
graf_ihme_total 
graf_ihme_total_group

##### Estimaciones #####
######### USAID

######### IHME
lm(rmh_dah_20/dah_20~ republican + gdp + poly(year,2) +
     poverty + income_class + foreign_invest + life_expect + republican:income_class,
   data = ihme %>% filter(rmh_dah_20>0)) %>% summary()

lm(rmh_dah_20/dah_20~ republican + gdp + factor(year) +
     poverty + income_class + foreign_invest + life_expect + republican:income_class,
   data = ihme %>% filter(rmh_dah_20>0)) %>% summary()


######### Foreign
lm(log(current_amount) ~ republican + gdp + poly(year,2) + 
     poverty + Income.Group.Name + foreign_invest + life_expect + republican:Income.Group.Name,
   data = foreign) %>% summary()

lm(log(current_amount) ~ republican + gdp + factor(year) + 
     poverty + Income.Group.Name + foreign_invest + life_expect + republican:Income.Group.Name,
   data = foreign) %>% summary()
