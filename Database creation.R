library(WDI)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)



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
                                         2017:2020),1,
                             ifelse(date %in% c(1993:2000,2009:2016,2021:2023),0,NA)),
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

writexl::write_xlsx(usaid %>% select(`Donor Project Id`),"Bases/projectID.xlsx")


######### Creación base IHME ####

## Revisar elim_ch y el codebook para poder trabajarlo.
ihme = read.csv("Bases/IHME_DAH.csv")

us_ihme = ihme %>% 
  filter(source=="United_States") %>%
  mutate(iso3c = recipient_isocode,
         country = recipient_country,
         year = year %>% as.double()) %>%
  left_join(world_bank_data, by=c("country","year","iso3c"))

ihme_general = ihme %>% 
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
foreign_graph = foreign %>% 
  group_by(year) %>%
  summarise(sum_amount = sum(constant_amount)) %>%
  ungroup() %>% 
  left_join(foreign, by="year") %>%
  filter(year<2023)%>%
  mutate(republicano = ifelse(year %in% c(2001:2008,2017:2020),"Republican",
                              ifelse(year %in% c(2009:2016,2021:2023),"Democrat",NA)) %>% 
           as.factor()) %>%
  ggplot(aes(x = year, y = log(sum_amount), color = republicano))+
  geom_point() +
  geom_vline(xintercept = c(2009,2017,2021))+
  labs(title = "Evolution of funds for Maternal and Child Health",
       y = "Log of the Total Amount",
       x = "Year",
       caption = "Data obtained from the Foreign Budget of the United States")+
  scale_color_manual(values = c("blue","red"))+
  theme_classic()
  


######### IHME ####
# Ver si está corregido por inflación
# Tomar la sumatoria

ihme_total_graph = us_ihme %>%
  mutate(rmh_dah_20 = rmh_dah_20 %>% as.double(),
         dah_20 = dah_20 %>% as.double()) %>%
  group_by(year) %>%
  mutate(rmh_sum = sum(rmh_dah_20,na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(republicano = ifelse(year %in% c(1985:1992,2001:2008,2017:2020),"Republican",
                              ifelse(year %in% c(1993:2000,2009:2016,2021:2023),"Democrat",NA)) %>% 
           as.factor()) %>% 
  ggplot(aes(x = year, y = log(rmh_sum), color = republicano))+
  geom_point()+
  geom_vline(xintercept = c(1993,2001,2009,2017,2021))+
  labs(title = "Evolution of funds for reproductive and maternal health (1990-2020)",
    y = "Log of Funds for RMH",
    x = "Year") +
  scale_color_manual(values = c("blue","red"))+
  theme_classic()

ihme_ratio_graph = us_ihme %>%
  mutate(rmh_dah_20 = rmh_dah_20 %>% as.double(),
         dah_20 = dah_20 %>% as.double()) %>%
  group_by(year) %>%
  filter(dah_20>0)%>%
  mutate(rmh_sum = sum(rmh_dah_20,na.rm=TRUE),
         dah_sum = sum(dah_20,na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(republicano = ifelse(year %in% c(1985:1992,2001:2008,2017:2020),"Republican",
                              ifelse(year %in% c(1993:2000,2009:2016,2021:2023),"Democrat",NA)) %>% 
           as.factor(),
         rmh_ratio = rmh_sum/dah_sum) %>% 
  ggplot(aes(x = year, y = rmh_ratio, color = republicano))+
  geom_point()+
  geom_vline(xintercept = c(1993,2001,2009,2017,2021))+
  labs(title = "Evolution of funds for reproductive and maternal health (1990-2020)",
       y = "Funds for RMH / Total funds for Health",
       x = "Year") +
  scale_color_manual(values = c("blue","red"))+
  theme_classic()




foreign_graph

ihme_ratio_graph
ihme_total_graph

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
