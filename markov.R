library(tidyverse)
library(readr)
library(sf)
library(sp)
library(ussf)




CZ_characteristics <- read_csv("table8.csv")
char_NA <- CZ_characteristics[rowSums(is.na(CZ_characteristics)) > 0,]
race_seg <- CZ_characteristics$`Racial Segregation`
crime_rate <- CZ_characteristics$`Violent Crime Rate`
school_expenditure <- CZ_characteristics$`School Expenditure per Student`
teen_labor <- CZ_characteristics$`Teenage Labor Force Participation Rate`
mig_in <- CZ_characteristics$`Migration Inflow Rate`
mig_out <- CZ_characteristics$`Migration Outlflow Rate`




CZ_quintiles <- read_csv("table6.csv")
head(CZ_quintiles)
CZ_quintiles[[5]] #p (child q1 | par q1)
CZ_map <- boundaries(geography = "cz", projection = "albers")
CZ_geom <- CZ_map[[4]]


CZ_quintiles$geom = CZ_geom
CZ_quintiles$race_seg = race_seg
CZ_quintiles$`Violent Crime Rate` = crime_rate
CZ_quintiles$`School Expenditure per Student` = school_expenditure
CZ_quintiles$`Teenage Labor Force Participation Rate` = teen_labor
CZ_quintiles$`Migration Inflow Rate` = mig_in
CZ_quintiles$`Migration Outflow Rate` = mig_out

#only 12 CZs have empty rows in table6, *DONT* ignore them, plot them specially??
#(26202, 26204, 26405, 26804, 27604, 28303, 29002, 30907, 34101, 34104, 34306, 34604)
#CZ_quintiles <- CZ_quintiles[rowSums(is.na(CZ_quintiles)) == 0,]


#this weird tibble thing works...
test <- st_as_sf(CZ_quintiles)
test <- as_tibble(test)




ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$race_seg)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`Violent Crime Rate`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`School Expenditure per Student`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`Children in 1980-85 Cohorts`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`Teenage Labor Force Participation Rate`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`Migration Inflow Rate`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`Migration Outflow Rate`)) +
  scale_fill_gradientn(colors = rainbow(5))












ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`P(Child q1 |Par q1)`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`P(Child q2 |Par q1)`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`P(Child q3 |Par q1)`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`P(Child q4 |Par q1)`)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`P(Child q5 |Par q1)`)) +
  scale_fill_gradientn(colors = rainbow(5))









