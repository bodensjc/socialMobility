library(readr)
library(sf)
library(ussf)



CZ_quintiles <- read_csv("table6.csv")
CZ_characteristics <- read_csv("table8.csv")

#only 12 CZs have empty rows in table6, ignore them
#(26202, 26204, 26405, 26804, 27604, 28303, 29002, 30907, 34101, 34104, 34306, 34604)
quint_NA <- CZ_quintiles[rowSums(is.na(CZ_quintiles)) > 0,]

print(quint_NA[1])


char_NA <- CZ_characteristics[rowSums(is.na(CZ_characteristics)) > 0,]






CZ_map <- boundaries(geography = "cz", projection = "albers")
state_map <- boundaries(geography = "state", projection = "albers")


CZ_map
plot(st_geometry(CZ_map[4]))

state_map
plot(st_geometry(state_map[10]))








