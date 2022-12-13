### CREATE FIGURES FOR PRESENTATIONS AND ARTICLE


# import data
source("R/1_Import_AppleQualityData.R")
source("R/2_Analysis_SeedSet.R")


## PLOT FOR CHRISTMAT LETTER

#ALL APPLE VARIETIES PLOTTED TOGETHER

SeedSetTreatment <- AppleQuality %>% 
  mutate(across(Treatment, factor, levels=c("HP","N","C"))) %>%
  ggplot(aes(x = Treatment, y = Seeds_fully_developed, fill = Treatment)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("HP", "N", "C")) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(labels = c("Hand pollinated", "Naturally pollinated", "Pollinators excluded"),values = c("#336633", "#CC3300", "#FF9900")) +
  labs(y="Number of seeds per apple", x="Treatment", fill="") +
  ggtitle("Seed set") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  #facet_wrap(~ Location) + 
  facet_wrap(~ Apple_variety)





### PLOTS FOR SEEDSET UNDER DIFFERENT TREATMENTS ###

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
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#336633", "#CC3300", "#FF9900")) +
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
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#336633", "#CC3300", "#FF9900")) +
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
  scale_fill_manual(labels = c("Fullt utviklet frø", "Ingen frø", "Delvis utviklet frø"),values = c("#336633", "#CC3300", "#FF9900")) +
  labs(y="Prosent", x="Behandling", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(SummerredUllensvangPlot, filename = "Figures/Summerred_Ullensvang.jpeg", height = 6, width = 8)


####################################################################################################################

### HOW NUMBER OF SEEDS AFFECTS APPLE WEIGHT UNDER DIFFERENT TREATMENT AND COMBINED ###
#Dashed line are lower priced apples, solid line are higher priced apples

#SEPARATED BY TREATMENT

AppleWeightSeeds <- AppleQuality %>% 
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Vekt (g)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(0, 350) + #REMOVED TWO OUTLIERS
  ggtitle("Korrelasjon mellom vekt og antall frø") +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=95, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=135.5, linetype="dashed", color = "black", size=0.5) + #line taken from internal apple quality measurement assesment
  geom_hline(yintercept=134, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=275, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Apple_variety) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleWeightSeeds, filename = "Figures/Apple_Weight_Seeds.jpeg", height = 6, width = 8)

#SEPARATED BY APPLE VARIETY

AppleWeightSeeds_Location <- AppleQuality %>% 
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Apple_variety)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Vekt (g)", color = "Eplesort", fill = "Eplesort") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Apple_variety)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(values = c("#996600", "#336633", "#CC3300")) +
  ylim(0, 350) + #REMOVED TWO OUTLIERS
  ggtitle("Korrelasjon mellom vekt og antall frø") +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=95, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=135.5, linetype="dashed", color = "black", size=0.5) + #line taken from internal apple quality measurement assesment
  geom_hline(yintercept=134, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=275, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleWeightSeeds_Location, filename = "Figures/Apple_Weight_Seeds_Location.jpeg", height = 6, width = 8)


## AROMA

AppleWeightSeeds_Aroma <- AppleQualityAroma %>% 
  mutate(across(Location, factor, levels=c("Berle","Høyen","Sando", "Lofthus", "Urheim", "Djønno"))) %>%
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Vekt (g)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(0, 350) + #REMOVED TWO OUTLIERS
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=97, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=128, linetype="dashed", color = "black", size=0.5) + #line taken from internal apple quality measurement assesment
  geom_hline(yintercept=142, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=260, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleWeightSeeds_Aroma, filename = "Figures/Apple_Weight_Seeds_Aroma.jpeg", height = 6, width = 8)


## DISCOVERY

AppleWeightSeeds_Discovery <- AppleQualityDiscovery %>% 
  mutate(across(Location, factor, levels=c("Berle","Høyen","Sando", "Lofthus", "Urheim", "Djønno"))) %>%
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Vekt (g)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(0, 350) + #REMOVED TWO OUTLIERS
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=95, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=124, linetype="dashed", color = "black", size=0.5) + #line taken from internal apple quality measurement assesment
  geom_hline(yintercept=134, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=275, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleWeightSeeds_Discovery, filename = "Figures/Apple_Weight_Seeds_Discovery.jpeg", height = 6, width = 8)


##SUMMERRED

AppleWeightSeeds_Summerred <- AppleQualitySummerred %>% 
  mutate(across(Location, factor, levels=c("Berle","Høyen","Sando", "Lofthus", "Urheim", "Djønno"))) %>%
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Vekt (g)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(0, 350) + #REMOVED TWO OUTLIERS
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=100, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=135, linetype="dashed", color = "black", size=0.5) + #line taken from internal apple quality measurement assesment
  geom_hline(yintercept=149, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=275, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleWeightSeeds_Summerred, filename = "Figures/Apple_Weight_Seeds_Summerred.jpeg", height = 6, width = 8)



### HOW NUMBER OF SEEDS AFFECTS APPLE RATIO (HEIGHT/DIAMETER) UNDER DIFFERENT TREATMENT AND COMBINED ###

AppleRatioSeeds <- AppleQuality %>% 
  ggplot(aes(y = Ratio, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Ratio", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(values = c("#996600", "#336633", "#CC3300")) +
  ylim(0.65, 1.35) + #REMOVED TWO OUTLIERS
  ggtitle("Korrelasjon mellom ratio (høyde/diameter) og antall frø") +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  facet_wrap(~ Apple_variety) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 14),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20))
ggsave(AppleRatioSeeds, filename = "Figures/Apple_Ratio_Seeds.jpeg", height = 6, width = 8)


### HOW NUMBER OF SEEDS AFFECTS APPLE HEIGHT UNDER DIFFERENT TREATMENT AND COMBINED ###

AppleHeightSeeds <- AppleQuality %>% 
  ggplot(aes(y = Height, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Høyde (mm)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(values = c("#996600", "#336633", "#CC3300")) +
  ggtitle("Korrelasjon mellom høyde og antall frø") +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  facet_wrap(~ Apple_variety) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 14),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20))
ggsave(AppleHeightSeeds, filename = "Figures/Apple_Height_Seeds.jpeg", height = 6, width = 8)


### HOW NUMBER OF SEEDS AFFECTS APPLE DIAMETER UNDER DIFFERENT TREATMENT AND COMBINED ###
#Dashed line are lower priced apples, solid line are higher priced apples

AppleDiameterSeeds <- AppleQuality %>% 
  ggplot(aes(y = Diameter, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Diameter (mm)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(30, 110) + #REMOVED TWO OUTLIERS
  ggtitle("Korrelasjon mellom diameter og antall frø") +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=60, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=69.5, linetype="dashed", color = "black", size=0.5) +#line taken from internal apple quality measurement assesment
  geom_hline(yintercept=70, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=90, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Apple_variety) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleDiameterSeeds, filename = "Figures/Apple_Diameter_Seeds.jpeg", height = 6, width = 8)

## AROMA ##

AppleDiameterSeeds_Aroma <- AppleQualityAroma %>% 
  mutate(across(Location, factor, levels=c("Berle","Høyen","Sando", "Lofthus", "Urheim", "Djønno"))) %>%
  ggplot(aes(y = Diameter, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Diameter (mm)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(30, 110) + #REMOVED TWO OUTLIERS
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=60, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=69.5, linetype="dashed", color = "black", size=0.5) +#line taken from internal apple quality measurement assesment
  geom_hline(yintercept=70, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=90, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleDiameterSeeds_Aroma, filename = "Figures/Apple_Diameter_Seeds_Aroma.jpeg", height = 6, width = 8)

## DISCOVERY ##

AppleDiameterSeeds_Discovery <- AppleQualityDiscovery %>% 
  mutate(across(Location, factor, levels=c("Berle","Høyen","Sando", "Lofthus", "Urheim", "Djønno"))) %>%
  ggplot(aes(y = Diameter, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Diameter (mm)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(30, 110) + #REMOVED TWO OUTLIERS
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=60, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=68, linetype="dashed", color = "black", size=0.5) +#line taken from internal apple quality measurement assesment
  geom_hline(yintercept=70, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=90, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleDiameterSeeds_Discovery, filename = "Figures/Apple_Diameter_Seeds_Discovery.jpeg", height = 6, width = 8)

## SUMMERRED ##

AppleDiameterSeeds_Summerred <- AppleQualitySummerred %>% 
  mutate(across(Location, factor, levels=c("Berle","Høyen","Sando", "Lofthus", "Urheim", "Djønno"))) %>%
  ggplot(aes(y = Diameter, x = Seeds_fully_developed, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter()) +
  labs(x = "Antall frø", y = "Diameter (mm)", color = "Behandling", fill = "Behandling") +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + #remove line with se=FALSE, small dataset remove method=lm, 
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(30, 110) + #REMOVED TWO OUTLIERS
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(strip.background = element_blank(), text = element_text(size = 25, hjust = 0.5))+
  geom_hline(yintercept=60, linetype="dashed", color = "black", size=0.5) +
  geom_hline(yintercept=69.5, linetype="dashed", color = "black", size=0.5) +#line taken from internal apple quality measurement assesment
  geom_hline(yintercept=70, linetype="solid", color = "black", size=0.5) +
  geom_hline(yintercept=90, linetype="solid", color = "black", size=0.5) +
  facet_wrap(~ Location) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15),
        legend.text = element_text (size = 12),
        axis.text = element_text (size = 18),
        axis.title = element_text (size = 18),
        plot.title = element_text (hjust = 0.5, size = 20, face = "bold"))
ggsave(AppleDiameterSeeds_Summerred, filename = "Figures/Apple_Diameter_Seeds_Summerred.jpeg", height = 6, width = 8)
