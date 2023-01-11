### STATISTICAL ANALYSIS FOR SEED SET DATA ON APPLE POLLINATION ###

library(lme4)
library(patchwork)

# import data
source("R/1_Import_AppleQualityData.R")










############# TEST ##############

ModelSeedSet0 <- glmer(Total_Seeds_Stage ~ 1 + (1 | Location), family = "poisson", data = SeedSet_stages)
ModelSeedSet1 <- glmer(Total_Seeds_Stage ~ Treatment + (1 | Location), family = "poisson", data = SeedSet_stages)
ModelSeedSet2 <- glmer(Total_Seeds_Stage ~ Apple_variety + (1 | Location), family = "poisson", data = SeedSet_stages)
ModelSeedSet3 <- glmer(Total_Seeds_Stage ~ Treatment + Apple_variety + (1 | Location), family = "poisson", data = SeedSet_stages)
ModelSeedSet4 <- glmer(Total_Seeds_Stage ~ Treatment * Apple_variety + (1 | Location), family = "poisson", data = SeedSet_stages)

AIC(ModelSeedSet0, ModelSeedSet1, ModelSeedSet2, ModelSeedSet3, ModelSeedSet4)

#Modell 4 har lavest AIC verdi
summary(ModelSeedSet4)

##############################################################


##### GENERALIZED LINEAR MIXED EFFECT MODELS #####

#Check if there is a difference in seed set per apple variety:
# - between location 
# - between regions (Svelvik vs Ullensvang)
# - within site


## SEED SET BETWEEN LOCATION ##

##AROMA##

#Models with fixed and random effects
SeedSet_Aroma0 <- glmer(Percentage_Seeds_Stage ~ 1 + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma1 <- glmer(Total_Seeds_Stage ~ Treatment + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma2 <- glmer(Total_Seeds_Stage ~ seed_stage + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma3 <- glmer(Total_Seeds_Stage ~ Treatment + seed_stage + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma4 <- glmer(Total_Seeds_Stage ~ Treatment * seed_stage + (1 | Location), family = "poisson", data = Aroma)


#AIC test
AIC(SeedSet_Aroma0, SeedSet_Aroma1, SeedSet_Aroma2, SeedSet_Aroma3, SeedSet_Aroma4)

# Model 4 was the best fit (lowest AIC value)
summary(SeedSet_Aroma4)




#Models with fixed and random effects
SeedSet_Aroma0 <- glmer(Total_Seeds_Stage ~ 1 + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma1 <- glmer(Total_Seeds_Stage ~ Treatment + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma2 <- glmer(Total_Seeds_Stage ~ Location + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma2 <- glmer(Total_Seeds_Stage ~ Location + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma3 <- glmer(Total_Seeds_Stage ~ Treatment + Location + (1 | Location), family = "poisson", data = Aroma)
SeedSet_Aroma4 <- glmer(Total_Seeds_Stage ~ Treatment * Location + (1 | Location), family = "poisson", data = Aroma)


#AIC test
AIC(SeedSet_Aroma0, SeedSet_Aroma1, SeedSet_Aroma2, SeedSet_Aroma3, SeedSet_Aroma4)

# Model 4 was the best fit (lowest AIC value)
summary(SeedSet_Aroma4)

visualize(SeedSet_Aroma4, plot = "model", sample = 50)


PLAndSeedSetPlot <- ggplot(SeedSet_stages, aes(x = Treatment, y = Total_Seeds_Treatment, color = Location)) +
  geom_boxplot() +
  facet_wrap(~ Location)





















##### Mann-Whitney U test #####

#All apple varieties combined
#hist(log(SeedSet_average$Average_seeds)) #non-normal distributen on plot

#shapiro.test(SeedSet_average$Average_seeds)

#Split by the three different apple varieties
#shapiro.test(SeedSet_average$Average_seeds)

#All values are significant, meaning they are not normally distributed
#data is also not paired, therefore using Mann-Whitney U test:

#ggplot(SeedSet_average, aes(x = Treatment, y = Average_seeds, color = Treatment)) +
#geom_boxplot() + 
#theme_bw() +
#facet_wrap(~Apple_variety)
#Plot: does not look like there is a difference between HP and N, but they are different from C

#SeedSet_average_HPN <- SeedSet_average %>% 
#filter(Treatment != 'C')

#WilcoxonTestAllVarieties <- wilcox.test(log(Average_seeds) ~ Treatment, data = SeedSet_average_HPN, 
#paired = FALSE)
#WilcoxonTestAllVarieties
## DOESNT WORK

