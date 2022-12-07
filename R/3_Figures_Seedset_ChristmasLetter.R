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
AromaSvelvikPlot <- ggplot(AromaSvelvik, aes(fill=seed_stage, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#336633", "#CC3300", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(AromaSvelvikPlot, filename = "Figures/Aroma_Svelvik.jpeg", height = 6, width = 8)

AromaUllensvangPlot <- ggplot(AromaUllensvang, aes(fill=seed_stage, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#CC3300", "#336633", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(AromaUllensvangPlot, filename = "Figures/Aroma_Ullensvang.jpeg", height = 6, width = 8)

#DISCOVERY

DiscoverySvelvikPlot <- ggplot(DiscoverySvelvik, aes(fill=seed_stage, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#336633", "#CC3300", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(DiscoverySvelvikPlot, filename = "Figures/Discovery_Svelvik.jpeg", height = 6, width = 8)

DiscoveryUllensvangPlot <- ggplot(DiscoveryUllensvang, aes(fill=seed_stage, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#CC3300", "#336633", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(DiscoveryUllensvangPlot, filename = "Figures/Discovery_Ullensvang.jpeg", height = 6, width = 8)


#Summerred

SummerredSvelvikPlot <- ggplot(SummerredSvelvik, aes(fill=seed_stage, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#336633", "#CC3300", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(SummerredSvelvikPlot, filename = "Figures/Summerred_Svelvik.jpeg", height = 6, width = 8)

SummerredUllensvangPlot <- ggplot(SummerredUllensvang, aes(fill=seed_stage, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#CC3300", "#336633", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(SummerredUllensvangPlot, filename = "Figures/Summerred_Ullensvang.jpeg", height = 6, width = 8)

