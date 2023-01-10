###### APPLE QUALITY DATA FROM POLLINATION EXPERIMENT, HARDANGER AND SVELVIK ######

library(ggpp)
library(tidyverse)
library(readxl)


#pn <- . %>% print(n = Inf) #allows you to see infinite rows

#Import data on effects of pollination on apple quality under three different 
#treatments: supplemental pollination, natural pollination, pollinators excluded
AppleQualityData <- read_excel("Data/AQv2.xlsx")



##########################################################################


## PREPARING SEEDSET DATA ##

#Get number of seeds in new row
SeedSet <- AppleQualityData %>%
  pivot_longer(Seeds_fully_developed:Seeds_not_developed, names_to = "seed_stage")

#Remove columns not needed for seed set analysis
SeedSet <- SeedSet %>% 
  select(-c(Weight, Height, Diameter, Ratio, Shape, Damage)) 


# organize database to obtain three seed_stage for each treatment, and not per apple
SeedSet_stages <- SeedSet %>% 
  group_by(Apple_variety, Region, Location, ID, Treatment, Tree, Apple_number, seed_stage) %>%
  summarise(tot = sum(value)) %>%
  ungroup() %>%
  group_by(Apple_variety, Region, Location, ID, Tree, Apple_number, Treatment) %>%
  mutate(tot2 = sum(tot))  %>% 
  rename(Total_Seeds_Stage = tot) %>% 
  rename(Total_Seeds_Treatment = tot2)

# Calculate percentage of each seed stage for each treatment  
SeedSet_Percentage <- SeedSet_stages %>% 
  mutate(Percentage_Seeds_Stage = (Total_Seeds_Stage/Total_Seeds_Treatment)*100)

# Calculate mean of seeds per tree and treatment
SeedSet_average <- AppleQualityData %>% 
  select(-c(Weight, Height, Diameter, Ratio, Shape, Damage, Seeds_partially_developed, Seeds_not_developed)) %>% 
  group_by(Apple_variety, Region, Location, Treatment, Tree) %>% 
  mutate(Total_seeds = sum(Seeds_fully_developed)) %>% 
  ungroup() %>% 
  group_by(Apple_variety, Region, Location, Treatment, Tree) %>% 
  top_n(1, Apple_number) %>% 
  rename(Total_number_apples = Apple_number) %>% 
  ungroup() %>% 
  mutate(Average_seeds = (Total_seeds/Total_number_apples)) %>% 
  select(-c(Seeds_fully_developed))


SeedSet_developed <- AppleQualityData %>% 
  select(-c(Weight, Height, Diameter, Ratio, Shape, Damage, Seeds_partially_developed, Seeds_not_developed)) %>% 
  group_by(Apple_variety, Region, Location, Treatment, Tree) %>% 
  ungroup()

#################################################################

## DATASET WITH ONLY TREATMENT, SEED NUMBER AND ID
SeedSetID <- AppleQualityData %>% 
  select(-c(Seeds_partially_developed, Seeds_not_developed)) %>% 
  select(-c(Weight, Height, Diameter, Ratio, Shape, Damage))


#SeedSetID2 <- SeedSetID %>% 
  #pivot_wider(names_from = Treatment, values_from = Seeds_fully_developed)





#Separate Seed Set measurements based on region and remove treatment where pollinators where excluded
Svelvik <- SeedSet_developed %>% 
  filter(Region == 'Svelvik') %>% 
  filter(Treatment != 'C')

Ullensvang <- SeedSet_developed %>% 
  filter(Region == 'Ullensvang') %>% 
  filter(Treatment != 'C')

#Separate Seed Set measurements based on apple variety and region
Summerred <- filter(SeedSet_developed, Apple_variety == 'Summerred') 

SummerredSvelvik <- filter(Summerred, Region == 'Svelvik')
SummerredUllensvang <- filter(Summerred, Region == 'Ullensvang')


Discovery <- filter(SeedSet_developed, Apple_variety == 'Discovery') 
DiscoverySvelvik <- filter(Discovery, Region == 'Svelvik')
DiscoveryUllensvang <- filter (Discovery, Region == 'Ullensvang')

Aroma <- filter (SeedSet_developed, Apple_variety == 'Aroma')
AromaSvelvik <- filter(Aroma, Region == 'Svelvik')
AromaUllensvang <- filter (Aroma, Region == 'Ullensvang')

###########################################################


## PREPARING APPLE QUALITY DATA ##


#Only use seeds fully developed
AppleQuality <- AppleQualityData %>%
  select(-c(Seeds_partially_developed, Seeds_not_developed)) 

#Remove C treatment
AppleQualityHPN <- AppleQualityData %>%
  filter(Treatment != 'C')


#For Aroma

AppleQualityAroma <- AppleQuality %>% 
  filter(Apple_variety == "Aroma")

#Remove C treatment
AppleQualityAromaHPN <- AppleQuality %>% 
  filter(Treatment != 'C')



#For Discovery

AppleQualityDiscovery <- AppleQuality %>% 
  filter(Apple_variety == "Discovery")

#Remove C treatment
AppleQualityDiscoveryHPN <- AppleQuality %>% 
  filter(Treatment != 'C')

#For Summerred

AppleQualitySummerred <- AppleQuality %>% 
  filter(Apple_variety == "Summerred")

#Remove C treatment
AppleQualitySummerredHPN <- AppleQuality %>% 
  filter(Treatment != 'C')

## PREPARING APPLE QUALITY DATA WITH EXTRA MEASUREMENTS ##




####################################################################

#Import data on number of flower clusters and number of apples picked per treatment
ClusterApple <- read_excel("Data/ClusterFruit.xlsx")

###### HUSK Å FJERNE DATA FRA NYE GREINER!!!###







