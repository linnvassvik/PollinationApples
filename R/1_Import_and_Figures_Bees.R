#### NUMBER OF BEES IN VESTLANDET

library(ggpp)
library(tidyverse)
library(readxl)
library(ggpattern)

## IMPORT DATASET
BeesWest <- read_excel("Data/Bees_Vestlandet_SiljeOgSara.xlsx")



ggplot(BeesWest) +
  geom_bar(aes(x = Norsk_gruppe, y = after_stat(count), fill = Norsk_gruppe)) +
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
  scale_fill_manual(values = c("#996600", "#336633", "#CC3300", "#FF9900", "#CC3300")) +
  ggtitle("Antall pollinatorer på Vestlandet")

