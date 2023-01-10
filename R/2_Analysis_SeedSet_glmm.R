### GLMM FOR SEED SET DATA ON APPLE POLLINATION ###

library(lme4)
library(patchwork)#??
library(tidyr)

# import data
source("R/1_Import_AppleQualityData.R")

#### Test for difference in seed set within orchard in Aroma in Berle

#PER APPLE VARIETY AND LOCATION

#EAST

##### BERLE #######
AromaBerle <- AromaSvelvik %>% 
  filter(Location == 'Berle') %>% 
  filter(Treatment != 'C')


SeedSet_AromaB0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaBerle)
SeedSet_AromaB1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaBerle)


#AIC test
AIC(SeedSet_AromaB0, SeedSet_AromaB1)

# Model 0 was the best fit (lowest AIC value) meaning treatment had no effect on seedset
summary(SeedSet_AromaB0)


###### HØYEN #######

AromaHoyen <- AromaSvelvik %>% 
  filter(Location == 'Høyen') %>% 
  filter(Treatment != 'C')


SeedSet_AromaH0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaHoyen)
SeedSet_AromaH1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaHoyen)


#AIC test
AIC(SeedSet_AromaH0, SeedSet_AromaH1)

# Model 1 was the best fit (***; lowest AIC value)
summary(SeedSet_AromaH1)

###### SANDO #######

AromaSando <- AromaSvelvik %>% 
  filter(Location == 'Sando') %>% 
  filter(Treatment != 'C')


SeedSet_AromaS0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaSando)
SeedSet_AromaS1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaSando)


#AIC test
AIC(SeedSet_AromaS0, SeedSet_AromaS1)

# Model 1 was the best fit (***; lowest AIC value)
summary(SeedSet_AromaS1)
