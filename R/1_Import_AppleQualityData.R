###### APPLE QUALITY DATA FROM POLLINATION EXPERIMENT, HARDANGER AND SVELVIK ######

library(ggpp)
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
  pivot_longer(Seeds_fully_developed:Seeds_not_developed, names_to = "seed_stage")

#Remove columns not needed for seed set analysis
SeedSet <- SeedSet %>% 
  select(-c(Weight, Height, Diameter, Ratio, Shape, Damage)) 


# organize database to obtain three seed_stage for each treatment, and not per apple
SeedSet_stages <- SeedSet %>% 
  group_by(Apple_variety, Region, Location, Treatment, seed_stage) %>%
  summarise(tot = sum(value)) %>%
  ungroup() %>%
  group_by(Apple_variety, Region, Location, Treatment) %>%
  mutate(tot2 = sum(tot))  %>% 
  rename(Total_Seeds_Stage = tot) %>% 
  rename(Total_Seeds_Treatment = tot2)

# Calculate percentage of each seed stage for each treatment  
SeedSet_stages_Percentage <- SeedSet_stages %>% 
  mutate(percentage = (Total_Seeds_Stage/Total_Seeds_Treatment)*100) %>% 
  rename(Percentage_Seeds_Stage = percentage)


#Separate Seed Set measurements based on apple variety and region
Summerred <- filter(SeedSet_stages_Percentage, Apple_variety == 'Summerred') 

SummerredSvelvik <- filter(Summerred, Region == 'Svelvik')
SummerredUllensvang <- filter(Summerred, Region == 'Ullensvang')


Discovery <- filter(SeedSet_stages_Percentage, Apple_variety == 'Discovery') 
DiscoverySvelvik <- filter(Discovery, Region == 'Svelvik')
DiscoveryUllensvang <- filter (Discovery, Region == 'Ullensvang')

Aroma <- filter (SeedSet_stages_Percentage, Apple_variety == 'Aroma')
AromaSvelvik <- filter(Aroma, Region == 'Svelvik')
AromaUllensvang <- filter (Aroma, Region == 'Ullensvang')

###########################################################

#Filtrate datasets on location for ANOVA analysis

SummerredBerle <- filter(SeedSet, Location == 'Berle')
SummerredHoyen <- filter(Summerred, Location == 'Hoyen')
SummerredSando <- filter(Summerred, Location == 'Sando')
SummerredLofthus <- filter(Summerred, Location == 'Lofthus')
SummerredUrheim <- filter(Summerred, Location == 'Urheim')
SummerredDjonno <- filter(Summerred, Location == 'Djonno')

DiscoveryBerle <- filter(Discovery, Location == 'Berle')
DiscoveryHoyen <- filter(Discovery, Location == 'Hoyen')
DiscoverySando <- filter(Discovery, Location == 'Sando')
DiscoveryLofthus <- filter(Discovery, Location == 'Lofthus')
DiscoveryUrheim <- filter(Discovery, Location == 'Urheim')
DiscoveryDjonno <- filter(Discovery, Location == 'Djonno')

AromaBerle <- filter(SeedSet, Apple_variety == "Aroma", Location == 'Berle')
AromaHoyen <- filter(Aroma, Location == 'Hoyen')
AromaSando <- filter(Aroma, Location == 'Sando')
AromaLofthus <- filter(Aroma, Location == 'Lofthus')
AromaUrheim <- filter(Aroma, Location == 'Urheim')
AromaDjonno <- filter(Aroma, Location == 'Djonno')


######################################################################
######################################################################
#####################################################################


## PREPARING APPLE QUALITY DATA ##


#Only use seeds fully developed
AppleQuality <- AppleQualityData %>%
  select(-c(Seeds_partially_developed, Seeds_not_developed)) 

#For Aroma

AppleQualityAroma <- AppleQuality %>% 
  filter(Apple_variety == "Aroma")

#For Discovery

AppleQualityDiscovery <- AppleQuality %>% 
  filter(Apple_variety == "Discovery")

#For Summerred

AppleQualitySummerred <- AppleQuality %>% 
  filter(Apple_variety == "Summerred")

## PREPARING APPLE QUALITY DATA WITH EXTRA MEASUREMENTS ##




####################################################################

#Import data on number of flower clusters and number of apples picked per treatment
ClusterApple <- read_excel("Data/ClusterFruit.xlsx")

###### HUSK Å FJERNE DATA FRA NYE GREINER!!!###







