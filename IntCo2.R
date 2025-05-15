library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

load("PubPol4557Session17Lab.Rdata")

#Part 2
WDI$Year <- ymd(WDI$Time, truncated = 2L)

#Question 1, How did CO2 emissions change over time for the countries in the sample?
co2graph <- ggplot(data=WDI) +
  geom_line(aes(x=Year,y=CO2percap,color=factor(CountryName))) +
  facet_wrap(~Group) +
  labs(x="Year",y="CO2 per Capita",title="Co2 per capita BRICS vs. G7")
co2graph

#Question 2, How did GDP change over the sample period for countries in the sample?

gdpgraph <- ggplot(data=WDI) +
  geom_line(aes(x=Year,y=log(GDPpercap),color=factor(CountryName))) + 
  labs(x="Year",y="log(GDP)",title="log(GDP) per capita 1990-2020", color="Country Name") +
  scale_x_date(date_breaks = "5 year")

gdpgraph

#Question 3, How did both GDP and CO2 emissions change over the period of the sample? 

subsetdata <- WDI %>% filter(CountryCode == "BRA" | 
                               CountryCode == "ZAF" | CountryCode =="RUS")
gdpco2graph <- ggplot(data = subsetdata) +
  geom_line(aes(x=log(GDP),y=CO2percap,color=Year)) +
  geom_text(aes(x=log(GDP),y=CO2percap,label=year(Year)), vjust = -0.5, size = 2) +
  facet_wrap(~CountryCode,nrow=3,scales="free") +
  labs(title = "Co2 Emission Levels")
gdpco2graph

#Question 4, Show the relationship between economic output (GDP) and Life Expectancy 

canada <- WDI %>% filter(CountryCode == "CAN")

cangraph <- ggplot(data=canada) +
  geom_path(aes(x=log(GDP),y=LifeExp,color=Year))+
  geom_text(aes(x=log(GDP),y=LifeExp,label=year(Year)), vjust = -0.5, size = 2) +
  labs(title = "Life Expectancy vs. log(GDP) over time, 1997-2019")
cangraph

