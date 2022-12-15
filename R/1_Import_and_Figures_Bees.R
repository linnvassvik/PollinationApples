#### NUMBER OF BEES IN VESTLANDET

library(ggpp)
library(tidyverse)
library(readxl)


## IMPORT DATASET
BeesWest <- read_excel("Data/Bees_Vestlandet_SiljeOgSara.xlsx")
BeesEast <- read_excel("Data/Bees_Østlandet_Jane.xlsx")

BeesEastEnglish <- read_excel("Data/Bees_Østlandet_Jane_english.xlsx")


## PIVOT BEESEAST
BeesEast <- BeesEast %>% 
  pivot_longer(Blomsterflue:Villbie, names_to = "Norsk_gruppe")

BeesEastEnglish <- BeesEastEnglish %>% 
  pivot_longer(Hoverfly:'Wild bee', names_to = "English_group")

##PLOTS

PollinatorsWestEng <- BeesWest %>% 
  mutate(across(English_gruppe, factor, levels=c("Honeybee","Bumble bee","Wild bee", "Hoverfly"))) %>%
  ggplot(aes(x = English_gruppe, y = after_stat(count), fill = English_gruppe)) +
  geom_bar() +
  facet_wrap(~ Location) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 35, size = 12, vjust = 1, hjust = 1),
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        strip.text = element_text (size = 18),
        legend.position = "none",
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold")) +
  labs(y="Number of pollinators", x="", fill="Pollinators") +
  scale_fill_manual(values = c("#006600", "#669966", "#993333", "#CC6666")) +
  ggtitle("Antall pollinatorer på Vestlandet")
ggsave(PollinatorsWestEng, filename = "Figures/Pollinators_WestNorway_Eng.jpeg", height = 6, width = 8)


PollinatorsEastEnglish <- BeesEastEnglish %>% 
  mutate(across(English_group, factor, levels=c("Honeybee","Bumble bee","Wild bee", "Hoverfly"))) %>%
  ggplot(aes(x = English_group, y = value, fill = English_group)) +
  geom_col() +
  facet_wrap(~ Location) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 35, size = 12, vjust = 1, hjust = 1),
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        strip.text = element_text (size = 18),
        legend.position = "none",
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold")) +
  labs(y="Number of pollinators", x="", fill="Pollinators") +
  scale_fill_manual(values = c("#006600", "#669966", "#993333", "#CC6666")) +
  ggtitle("Antall pollinatorer på Østlandet")
ggsave(PollinatorsEastEnglish, filename = "Figures/Pollinators_East_English.jpeg", height = 6, width = 8)

