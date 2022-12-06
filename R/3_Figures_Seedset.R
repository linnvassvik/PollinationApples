### CREATE FIGURES FOR PRESENTATIONS AND ARTICLE


# import data
source("R/1_Import_AppleQualityData.R")
source("R/2_Analysis_SeedSet.R")



## PLOT FOR CHRISTMAT LETTER

#ALL APPLE VARIETIES PLOTTED TOGETHER

ggplot(SeedSet_stages_Percentage, aes(x = Treatment, y = Percentage_Seeds_Stage, fill = seed_stage)) +
  geom_boxplot() +
  facet_wrap(~ Location) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fully developed seeds", "No seeds", "Partially developed seeds"),values = c("#99CC66", "#CC6666", "#FFCC66")) +
  labs(y="Percentage", x="Treatment", fill="") +
  ggtitle("Seed set") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))



#AROMA

ggplot(Aroma, aes(x = Treatment, y = Percentage_Seeds_Stage, fill = seed_stage)) +
  geom_col() +
  facet_wrap(~ Location)


#DISCOVERY

ggplot(Discovery, aes(x = Treatment, y = Percentage_Seeds_Stage, fill = Location)) +
  geom_boxplot() +
  facet_wrap(~ Location)


#Summerred

ggplot(Summerred, aes(x = Treatment, y = Percentage_Seeds_Stage, fill = Location)) +
  geom_col() +
  facet_wrap(~ Location)

