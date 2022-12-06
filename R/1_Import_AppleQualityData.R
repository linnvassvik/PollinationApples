###### APPLE QUALITY DATA FROM POLLINATION EXPERIMENT, HARDANGER AND SVELVIK ######

#library(viridis)
#library(RColorBrewer)
#library(ggpp)
library(tidyverse)
library(readxl)


pn <- . %>% print(n = Inf) #allows you to see infinite rows

#Import data on effects of pollination on apple quality under three different 
#treatments: supplemental pollination, natural pollination, pollinators excluded
AppleQualityData <- read_excel("Data/AQv2.xlsx")



##########################################################################


## PREPARING SEEDSET DATA ##

#Get number of seeds in new row
SeedSet <- AppleQualityData %>%
  pivot_longer(Seeds_fully_developet:Seeds_not_developed, names_to = "seed_stage")

#Remove columns not needed for seed set analysis
SeedSet <- SeedSet %>% 
  select(-c(Weight, Height, Diameter, Ratio, Shape, Damage))

# organize database to obtain three seed_stage for each treatment, and not per apple
SeedSet_stages <- SeedSet %>% 
  group_by(Apple_variety, Region, Location, Treatment, seed_stage) %>%
  summarise(tot = sum(value)) %>%
  ungroup() %>%
  group_by(Apple_variety, Region, Location, Treatment) %>%
  mutate(tot2 = sum(tot))

# Calculate percentage of each seed stage for each treatment  
SeedSet_stages <- SeedSet_stages %>% 
  mutate(percentage = tot/tot2)


#Separate Seed Set measurements based on apple variety and region
Summerred <- filter(SeedSet_stages, Apple_variety == 'Summerred') 

SummerredSvelvik <- filter(Summerred, Region == 'Svelvik')
SummerredUllensvang <- filter(Summerred, Region == 'Ullensvang')


Discovery <- filter(SeedSet_stages, Apple_variety == 'Discovery') 
DiscoverySvelvik <- filter(Discovery, Region == 'Svelvik')
DiscoveryUllensvang <- filter (Discovery, Region == 'Ullensvang')

Aroma <- filter (SeedSet_stages, Apple_variety == 'Aroma')
AromaSvelvik <- filter(Aroma, Region == 'Svelvik')
AromaUllensvang <- filter (Aroma, Region == 'Ullensvang')



######################################################################


## PREPARING APPLE QUALITY DATA ##







## PREPARING APPLE QUALITY DATA WITH EXTRA MEASUREMENTS ##




####################################################################

#Import data on number of flower clusters and number of apples picked per treatment
ClusterApple <- read_excel("Data/ClusterFruit.xlsx")

###### HUSK Å FJERNE DATA FRA NYE GREINER!!!###







