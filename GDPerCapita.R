#install.packages("sf")

library(ggplot2)
library(readr)
library(dplyr)
library(sf)

load("PubPol4557Session21.Rdata")
COUNTRIES <- read_sf(dsn="world-administrative-boundaries/world-administrative-boundaries.shp",
                     stringsAsFactors = F)

WDIMERGE <- full_join(COUNTRIES,WDI,by = c("color_code"="countrycode"))

#Question 1
#For one of the years in the dataset, create a visualization which explores whether 
#there are spatial/regional differences in the carbon intensity 
#(as measured by CO2 per economic output unit) of countries' economies.
WDI2014 <- WDIMERGE %>% filter(time == 2014)

CO2map2012 <- ggplot(data=WDI2014) +
  geom_sf(aes(fill=co2pergdp,geometry=geometry),size = 0.25) +
  scale_fill_gradient(low="yellow",high="red",na.value = "gray") +
  labs(title="CO2 per GDP",subtitle="By Country")
CO2map2012

#Question 2
#Create one visualization that compares economic output over space using total GDP, 
#then do the same using GDP per capita.
GDPmap <- ggplot(data = WDIMERGE %>% filter(time==2019)) +
  geom_sf(aes(fill=log(gdptotal)),size=0.25) +
  scale_fill_gradient(low="green",high="blue",na.value = "gray") +
  labs(title="Global Log of GDP (2019)",subtitle="By Country")
GDPmap

GDPpercapmap <- ggplot(data = WDIMERGE %>% filter(time==2009)) +
  geom_sf(aes(fill=gdppercap),size=0.25) +
  scale_fill_gradient(low="green",high="red",na.value = "gray") +
  labs(title="GDP per capita",subtitle="By Country")
GDPpercapmap

?scale_fill_gradient
#Question 3
#Create a visualization (or group of visualizations) showing progression of that measure over time 
#and differences over space using multiple maps.

Lifemap <- ggplot(data = WDIMERGE, na.rm=T) +
  geom_sf(aes(fill=lifeexp),size = 0.5) +
  scale_fill_gradient(low="blue",high="red",na.value="white") +
  labs(title = "Life Expectancy") +
  facet_wrap(~factor(time))
Lifemap




