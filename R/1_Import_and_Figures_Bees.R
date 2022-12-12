#### NUMBER OF BEES IN VESTLANDET

library(ggpp)
library(tidyverse)
library(readxl)


## IMPORT DATASET
BeesWest <- read_excel("Data/Bees_Vestlandet_SiljeOgSara.xlsx")



PollinatorsWest <- BeesWest %>% 
  mutate(across(Norsk_gruppe, factor, levels=c("Honningbie","Humle","Villbie", "Blomsterflue", "Veps"))) %>%
  ggplot(aes(x = Norsk_gruppe, y = after_stat(count), fill = Norsk_gruppe)) +
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
  labs(y="Antall", x="", fill="Pollinatorer") +
  scale_fill_manual(values = c("#006600", "#669966", "#993333", "#CC6666", "#FF9900")) +
  ggtitle("Antall pollinatorer på Vestlandet")
ggsave(PollinatorsWest, filename = "Figures/Pollinators_WestNorway.jpeg", height = 6, width = 8)

