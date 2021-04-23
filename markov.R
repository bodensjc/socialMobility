library(tidyverse)
library(readr)
library(sf)
library(sp)
library(ussf)
library(expm)




CZ_characteristics <- read_csv("data/table8.csv")
CZ_quintiles <- read_csv("data/table6.csv")
CZ_map <- boundaries(geography = "cz", projection = "albers")
CZ_geom <- CZ_map[[4]]
CZ_quintiles$geom = CZ_geom

#make sure they all have the same order and CZs
cz1 <- CZ_characteristics[[1]]
cz2 <- CZ_quintiles[[1]]
cz3 <- as.numeric(CZ_map[[1]])
identical(cz1, cz2, cz3)





char_NA <- CZ_characteristics[rowSums(is.na(CZ_characteristics)) > 0,]
race_seg <- CZ_characteristics$`Racial Segregation`
crime_rate <- CZ_characteristics$`Violent Crime Rate`
school_expenditure <- CZ_characteristics$`School Expenditure per Student`
teen_labor <- CZ_characteristics$`Teenage Labor Force Participation Rate`
mig_in <- CZ_characteristics$`Migration Inflow Rate`
mig_out <- CZ_characteristics$`Migration Outlflow Rate`


CZ_quintiles$race_seg = race_seg
CZ_quintiles$`Violent Crime Rate` = crime_rate
CZ_quintiles$`School Expenditure per Student` = school_expenditure
CZ_quintiles$`Teenage Labor Force Participation Rate` = teen_labor
CZ_quintiles$`Migration Inflow Rate` = mig_in
CZ_quintiles$`Migration Outflow Rate` = mig_out

#only 12 CZs have empty rows in table6, *DONT* ignore them, plot them specially??
#(26202, 26204, 26405, 26804, 27604, 28303, 29002, 30907, 34101, 34104, 34306, 34604)
#CZ_quintiles <- CZ_quintiles[rowSums(is.na(CZ_quintiles)) == 0,]


kq1pq1 <- CZ_quintiles$`P(Child q1 |Par q1)`*CZ_quintiles$`P(Par q1)`
kq2pq1 <- CZ_quintiles$`P(Child q2 |Par q1)`*CZ_quintiles$`P(Par q1)`
kq3pq1 <- CZ_quintiles$`P(Child q3 |Par q1)`*CZ_quintiles$`P(Par q1)`
kq4pq1 <- CZ_quintiles$`P(Child q4 |Par q1)`*CZ_quintiles$`P(Par q1)`
kq5pq1 <- CZ_quintiles$`P(Child q5 |Par q1)`*CZ_quintiles$`P(Par q1)`
kq1pq2 <- CZ_quintiles$`P(Child q1 |Par q2)`*CZ_quintiles$`P(Par q2)`
kq2pq2 <- CZ_quintiles$`P(Child q2 |Par q2)`*CZ_quintiles$`P(Par q2)`
kq3pq2 <- CZ_quintiles$`P(Child q3 |Par q2)`*CZ_quintiles$`P(Par q2)`
kq4pq2 <- CZ_quintiles$`P(Child q4 |Par q2)`*CZ_quintiles$`P(Par q2)`
kq5pq2 <- CZ_quintiles$`P(Child q5 |Par q2)`*CZ_quintiles$`P(Par q2)`
kq1pq3 <- CZ_quintiles$`P(Child q1 |Par q3)`*CZ_quintiles$`P(Par q3)`
kq2pq3 <- CZ_quintiles$`P(Child q2 |Par q3)`*CZ_quintiles$`P(Par q3)`
kq3pq3 <- CZ_quintiles$`P(Child q3 |Par q3)`*CZ_quintiles$`P(Par q3)`
kq4pq3 <- CZ_quintiles$`P(Child q4 |Par q3)`*CZ_quintiles$`P(Par q3)`
kq5pq3 <- CZ_quintiles$`P(Child q5 |Par q3)`*CZ_quintiles$`P(Par q3)`
kq1pq4 <- CZ_quintiles$`P(Child q1 |Par q4)`*CZ_quintiles$`P(Par q4)`
kq2pq4 <- CZ_quintiles$`P(Child q2 |Par q4)`*CZ_quintiles$`P(Par q4)`
kq3pq4 <- CZ_quintiles$`P(Child q3 |Par q4)`*CZ_quintiles$`P(Par q4)`
kq4pq4 <- CZ_quintiles$`P(Child q4 |Par q4)`*CZ_quintiles$`P(Par q4)`
kq5pq4 <- CZ_quintiles$`P(Child q5 |Par q4)`*CZ_quintiles$`P(Par q4)`
kq1pq5 <- CZ_quintiles$`P(Child q1 |Par q5)`*CZ_quintiles$`P(Par q5)`
kq2pq5 <- CZ_quintiles$`P(Child q2 |Par q5)`*CZ_quintiles$`P(Par q5)`
kq3pq5 <- CZ_quintiles$`P(Child q3 |Par q5)`*CZ_quintiles$`P(Par q5)`
kq4pq5 <- CZ_quintiles$`P(Child q4 |Par q5)`*CZ_quintiles$`P(Par q5)`
kq5pq5 <- CZ_quintiles$`P(Child q5 |Par q5)`*CZ_quintiles$`P(Par q5)`















numPeople <- sum(CZ_quintiles[, 'Children in 1980-85 Cohorts'], na.rm = TRUE)
pctPeople <- CZ_quintiles$`Children in 1980-85 Cohorts`/numPeople

transitions_only <- CZ_quintiles[5:29]
transitions_only = transitions_only*pctPeople
matrix_data <- colSums(transitions_only, na.rm = TRUE, dims = 1)
total_transition_matrix <- t(matrix(matrix_data, nrow = 5, ncol = 5))#transpose to get parent as rows and child as columns
total_transition_matrix
t(total_transition_matrix)

total_transition_matrix %*% total_transition_matrix
total_transition_matrix %^% 3

initial_states <- CZ_quintiles[30:34]
initial_states = initial_states*pctPeople
initial_states <- colSums(initial_states, na.rm = TRUE, dims = 1)
initial_states



upward_cols <- data.frame(kq1pq1, kq2pq1, kq3pq1, kq4pq1, kq5pq1, kq2pq2, kq3pq2, kq4pq2, kq5pq2, kq3pq3, kq4pq3, kq5pq3, kq4pq4, kq5pq4, kq5pq5)
prob_move_upwards <- rowSums(upward_cols, na.rm = FALSE)
new_upward_cols <- data.frame(kq2pq1, kq3pq1, kq4pq1, kq5pq1, kq3pq2, kq4pq2, kq5pq2, kq4pq3, kq5pq3, kq5pq4, kq5pq5)
new_upward <- rowSums(new_upward_cols, na.rm = FALSE)
same_cols <- data.frame(kq1pq1, kq2pq2, kq3pq3, kq4pq4, kq5pq5)
prob_same <- rowSums(same_cols, na.rm = FALSE)
prob_down <- data.frame(kq1pq2, kq1pq3, kq1pq4, kq1pq5, kq2pq3, kq2pq4, kq2pq5, kq3pq4, kq3pq5, kq4pq5)
prob_move_down <- rowSums(prob_down, na.rm = FALSE)


CZ_quintiles$Upwards_Probability = prob_move_upwards
CZ_quintiles$Mod_Up_Prob = new_upward
CZ_quintiles$Prob_Same= prob_same
CZ_quintiles$Prob_Move_Down = prob_move_down


#this weird tibble thing works for making the maps...
test <- st_as_sf(CZ_quintiles)
test <- as_tibble(test)






ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$Upwards_Probability)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$Mod_Up_Prob)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$Prob_Same)) +
  scale_fill_gradientn(colors = rainbow(5))

ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$Prob_Move_Down)) +
  scale_fill_gradientn(colors = rainbow(5))



ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = test$`Children in 1980-85 Cohorts`)) +
  scale_fill_gradientn(colors = rainbow(5))

#sample plots

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

#transition maps

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




kq1 = kq1pq1+kq1pq2+kq1pq3+kq1pq4+kq1pq5
kq2 = kq2pq1+kq2pq2+kq2pq3+kq2pq4+kq2pq5
kq3 = kq3pq1+kq3pq2+kq3pq3+kq3pq4+kq3pq5
kq4 = kq4pq1+kq4pq2+kq4pq3+kq4pq4+kq4pq5
kq5 = kq5pq1+kq5pq2+kq5pq3+kq5pq4+kq5pq5

pq1 = kq1pq1+kq2pq1+kq3pq1+kq4pq1+kq5pq1
pq2 = kq1pq2+kq2pq2+kq3pq2+kq4pq2+kq5pq2
pq3 = kq1pq3+kq2pq3+kq3pq3+kq4pq3+kq5pq3
pq4 = kq1pq4+kq2pq4+kq3pq4+kq4pq4+kq5pq4
pq5 = kq1pq5+kq2pq5+kq3pq5+kq4pq5+kq5pq5





#looking for outlier initial distributions
largestpq5 <- which.max(pq5)
bestVector <- c(pq1[largestpq5],pq2[largestpq5],pq3[largestpq5],pq4[largestpq5],pq5[largestpq5])
bestVector
pq5maxVec <- rep(0, 741)
pq5maxVec[largestpq5] = 1
ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = pq5maxVec)) +
  scale_fill_gradientn(colors = c("white", "black"))

largestpq1 <- which.max(pq1)
worstVector <- c(pq1[largestpq1],pq2[largestpq1],pq3[largestpq1],pq4[largestpq1],pq5[largestpq1])
worstVector
pq1maxVec <- rep(0, 741)
pq1maxVec[largestpq1] = 1
ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = pq1maxVec)) +
  scale_fill_gradientn(colors = c("white", "black"))
CZ_map$place[largestpq1]
CZ_map$state[largestpq1]




#plotting the best place for landing in 5th quintile
bestCZ <- which.max(kq5)#gives the CZ tag for location with highest prob of kids entering 5th quintile
worstCZ <- which.min(kq5)
kq5test <- rep(0, 741)
kq5test2 <- kq5test
kq5test[bestCZ] = 1
kq5test2[worstCZ] = 1
ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = kq5test)) +
  scale_fill_gradientn(colors = c("white", "black"))
ggplot(data = test$geom, aes(geometry = geometry)) +
  geom_sf(aes(fill = kq5test2)) +
  scale_fill_gradientn(colors = c("white", "black"))







