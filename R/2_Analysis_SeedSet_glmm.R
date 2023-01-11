### GLMM FOR SEED SET DATA ON APPLE POLLINATION ###

library(lme4)
#library(patchwork)#??
library(tidyr)
library(MASS)

# import data
source("R/1_Import_AppleQualityData.R")

##### GENERALIZED LINEAR MIXED EFFECT MODELS #####

#Check if there is a difference in seed set per apple variety:
# - between location 
# - between regions (Svelvik vs Ullensvang)
# - within site


## SEED SET BETWEEN LOCATION ##

#### Test for difference in seed set within orchard in Aroma in Berle
# - per apple variety and location

#EAST
# using negative binomial as the residual plot with poission had the residuals more spread out than the negative binomial residuals

##### BERLE #######
# - Aroma
AromaBerle <- AromaSvelvik %>% 
  filter(Location == 'Berle') %>% 
  filter(Treatment != 'C')


SeedSet_AromaB0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaBerle)
SeedSet_AromaB1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaBerle)


#AIC test
AIC(SeedSet_AromaB0, SeedSet_AromaB1)

# Model 0 was the best fit (lowest AIC value) meaning treatment had no effect on seedset
summary(SeedSet_AromaB0)


## 

# - Discovery
DiscoveryBerle <- DiscoverySvelvik %>% 
  filter(Location == 'Berle') %>% 
  filter(Treatment != 'C')


SeedSet_DiscoveryB0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = DiscoveryBerle)
SeedSet_DiscoveryB1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = DiscoveryBerle)


#AIC test
AIC(SeedSet_DiscoveryB0, SeedSet_DiscoveryB1)

# Model 0 and 1 had the same best fit (lowest AIC value) meaning treatment had no effect on seedset
summary(SeedSet_DiscoveryB1)

## OBS no difference in residual test, however with BN AIC values showed no difference in models

## 

# - Summerred
SummerredBerle <- SummerredSvelvik %>% 
  filter(Location == 'Berle') %>% 
  filter(Treatment != 'C')


SeedSet_SummerredB0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = SummerredBerle)
SeedSet_SummerredB1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = SummerredBerle)


#AIC test
AIC(SeedSet_SummerredB0, SeedSet_SummerredB1)

# Model 0 was the best fit (lowest AIC value) meaning treatment had no effect on seedset
summary(SeedSet_SummerredB0)



###### HØYEN #######
# - Aroma
AromaHoyen <- AromaSvelvik %>% 
  filter(Location == 'Høyen') %>% 
  filter(Treatment != 'C')


SeedSet_AromaH0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaHoyen)
SeedSet_AromaH1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaHoyen)


#AIC test
AIC(SeedSet_AromaH0, SeedSet_AromaH1)

# Model 1 was the best fit (***; lowest AIC value)
summary(SeedSet_AromaH1)



####

# - Discovery
DiscoveryHoyen <- DiscoverySvelvik %>% 
  filter(Location == 'Høyen') %>% 
  filter(Treatment != 'C')


SeedSet_DiscoveryH0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = DiscoveryHoyen)
SeedSet_DiscoveryH1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = DiscoveryHoyen)


#AIC test
AIC(SeedSet_DiscoveryH0, SeedSet_DiscoveryH1)

# Model 1 was the best fit (***; lowest AIC value)
summary(SeedSet_DiscoveryH1)

####

# - Summerred
SummerredHoyen <- SummerredSvelvik %>% 
  filter(Location == 'Høyen') %>% 
  filter(Treatment != 'C')


SeedSet_SummerredH0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = SummerredHoyen)
SeedSet_SummerredH1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = SummerredHoyen)


#AIC test
AIC(SeedSet_SummerredH0, SeedSet_SummerredH1)

# Model 1 was the best fit (***; lowest AIC value)
summary(SeedSet_SummerredH1)

###### SANDO #######
# - Aroma
AromaSando <- AromaSvelvik %>% 
  filter(Location == 'Sando') %>% 
  filter(Treatment != 'C')


SeedSet_AromaS0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaSando)
SeedSet_AromaS1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaSando)


#AIC test
AIC(SeedSet_AromaS0, SeedSet_AromaS1)

# Model 1 was the best fit (***; lowest AIC value)
summary(SeedSet_AromaS1)

# - Discovery
DiscoverySando <- DiscoverySvelvik %>% 
  filter(Location == 'Sando') %>% 
  filter(Treatment != 'C')


SeedSet_DiscoveryS0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = DiscoverySando)
SeedSet_DiscoveryS1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = DiscoverySando)


#AIC test
AIC(SeedSet_DiscoveryS0, SeedSet_DiscoveryS1)

# Model 0 was the best fit (lowest AIC value)
summary(SeedSet_DiscoveryS0)

# - Summerred
SummerredSando <- SummerredSvelvik %>% 
  filter(Location == 'Sando') %>% 
  filter(Treatment != 'C')


SeedSet_SummerredS0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = SummerredSando)
SeedSet_SummerredS1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = SummerredSando)


#AIC test
AIC(SeedSet_SummerredS0, SeedSet_SummerredS1)

# Model 0 was the best fit (lowest AIC value)
summary(SeedSet_SummerredS0)


###########################################
###########################################

### Difference in seed set between orchards in East Norway
# - Aroma

AromaEast <- Svelvik %>% 
  filter(Apple_variety == 'Aroma')

SeedSet_AromaSvelvik0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree:Location), family = "poisson", data = AromaEast)
SeedSet_AromaSvelvik1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree:Location), family = "poisson", data = AromaEast)
SeedSet_AromaSvelvik2 <- glmer(Seeds_fully_developed ~ Treatment + Location + (1 | Tree:Location), family = "poisson", data = AromaEast)
#SeedSet_AromaSvelvik3 <- glmer(Seeds_fully_developed ~ Treatment * Location + (1 | Tree:Location), family = "poisson", data = AromaEast)

#AIC test
AIC(SeedSet_AromaSvelvik0, SeedSet_AromaSvelvik1, SeedSet_AromaSvelvik2, SeedSet_AromaSvelvik3)

# Model 0 was the best fit (lowest AIC value)
summary(SeedSet_AromaSvelvik2)


