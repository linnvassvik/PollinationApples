### MODEL AVERAGING FOR SEED SET DATA ON APPLE POLLINATION ###

library(MuMIn)
#library(GISTools)
#library(spatialreg)
#library(spdep)

# import data
source("R/1_Import_AppleQualityData.R")

### DREDGING

options(na.action = "na.fail")   #  prevent fitting models to different datasets

# First "global" model for Aroma apples
SeedSetModelAroma = stats::lm(formula = Total_Seeds_Stage ~ Region + Location + Treatment, data = Aroma)
summary(SeedSetModelAroma)


#Dredge to confirm variable and best model
dredge(global.model = SeedSetModelAroma)

#Best model to explain Total_Seeds_Stage is treatment, then treatment + region
#Is it correct to use Total_Seeds_Stage?