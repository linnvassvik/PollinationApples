### CREATE FIGURES FOR PRESENTATIONS AND ARTICLE


# import data
source("R/1_Import_AppleQualityData.R")
source("R/2_Analysis_SeedSet.R")


## PLOT FOR CHRISTMAT UPDATE TO PRODUCERS AND PROJECT PARTNERS

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

## PLOTS FOR SEEDSET 



## EAST NORWAY
SvelvikPlotEng <- Svelvik %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(y=Percentage_Seeds_Stage, x=Treatment, fill = Treatment)) + 
  stat_summary(fun = "mean", geom = "col") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Apple_variety) + 
  theme(strip.background = element_blank()) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  scale_fill_manual(values = c("#336633", "#CC3300")) +
  theme(legend.position = "bottom",
        legend.text = element_text (size = 15),
        axis.text = element_text (size = 18),
        strip.text = element_text (size = 18),
        axis.title = element_text (size = 18))
ggsave(SvelvikPlotEng, filename = "Figures/SvelvikEng.jpeg", height = 6, width = 8)


## WEST NORWAY

UllensvangPlotEng <- Ullensvang %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(y=Percentage_Seeds_Stage, x=Treatment, fill = Treatment)) + 
  stat_summary(fun = "mean", geom = "col") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Apple_variety) + 
  theme(strip.background = element_blank()) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  scale_fill_manual(values = c("#336633", "#CC3300")) +
  theme(legend.position = "bottom",
        legend.text = element_text (size = 15),
        axis.text = element_text (size = 18),
        strip.text = element_text (size = 18),
        axis.title = element_text (size = 18)) 
ggsave(UllensvangPlotEng, filename = "Figures/UllensvangEng.jpeg", height = 6, width = 8)

### PLOTS FOR SEEDSET UNDER DIFFERENT TREATMENTS ###

#AROMA
AromaSvelvikPlotEng <- AromaSvelvik %>% 
  filter(Treatment != 'C') %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(fill=Treatment, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("#336633", "#CC3300"), guide = guide_legend(reverse = TRUE)) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(AromaSvelvikPlotEng, filename = "Figures/Aroma_SvelvikEng.jpeg", height = 8, width = 8)

AromaUllensvangPlotEng <- AromaUllensvang %>% 
  filter(Treatment != 'C') %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(fill=Treatment, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("#336633", "#CC3300"), guide = guide_legend(reverse = TRUE)) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(AromaUllensvangPlotEng, filename = "Figures/Aroma_UllensvangEng.jpeg", height = 8, width = 8)

#DISCOVERY

DiscoverySvelvikPlotEng <- DiscoverySvelvik %>% 
  filter(Treatment != 'C') %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(fill=Treatment, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("#336633", "#CC3300"), guide = guide_legend(reverse = TRUE)) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(DiscoverySvelvikPlotEng, filename = "Figures/Discovery_SvelvikEng.jpeg", height = 8, width = 8)

DiscoveryUllensvangPlotEng <- DiscoveryUllensvang %>% 
  filter(Treatment != 'C') %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(fill=Treatment, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("#336633", "#CC3300"), guide = guide_legend(reverse = TRUE)) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(DiscoveryUllensvangPlotEng, filename = "Figures/Discovery_UllensvangEng.jpeg", height = 8, width = 8)


#Summerred

SummerredSvelvikPlotEng <- SummerredSvelvik %>% 
  filter(Treatment != 'C') %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(fill=Treatment, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("#336633", "#CC3300"), guide = guide_legend(reverse = TRUE)) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(SummerredSvelvikPlotEng, filename = "Figures/Summerred_SvelvikEng.jpeg", height = 8, width = 8)

SummerredUllensvangPlotEng <- SummerredUllensvang %>%  
  filter(Treatment != 'C') %>% 
  filter(seed_stage == 'Seeds_fully_developed') %>% 
  ggplot(aes(fill=Treatment, y=Percentage_Seeds_Stage, x=Treatment)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = c("HP", "N")) +
  facet_wrap(~ Location) + 
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("#336633", "#CC3300"), guide = guide_legend(reverse = TRUE)) +
  labs(y="Percentage fully developed seeds per apple", x="Treatment", fill="") +
  theme(text = element_text(size = 25, hjust = 0.5)) 
ggsave(SummerredUllensvangPlotEng, filename = "Figures/Summerred_UllensvangEng.jpeg", height = 8, width = 8)


####################################################################################################################

### HOW NUMBER OF SEEDS AFFECTS APPLE WEIGHT UNDER DIFFERENT TREATMENT AND COMBINED ###
#Dashed line are lower priced apples, solid line are higher priced apples

#SEPARATED BY TREATMENT

AppleWeightSeeds <- AppleQuality %>% 
  mutate(across(Location, factor, levels = c ("Berle", "Høyen", "Sando", "Lofthus", "Urheim", "Djønno"))) %>% 
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Treatment)) +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) + 
  labs(x = "Antall frø per eple", y = "Vekt (g)", color = "Behandling", fill = "Behandling") +
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(50, 300) + #REMOVED TWO OUTLIERS
  ggtitle("Korrelasjon mellom vekt og antall frø") +
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
ggsave(AppleWeightSeeds, filename = "Figures/Apple_Weight_Seeds.jpeg", height = 6, width = 8)

#SEPARATED BY APPLE VARIETY

AppleWeightSeeds_Location <- AppleQuality %>% 
  ggplot(aes(y = Weight, x = Seeds_fully_developed, color = Apple_variety)) +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Apple_variety)) + 
  labs(x = "Antall frø", y = "Vekt (g)", color = "Eplesort", fill = "Eplesort") +
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
  mutate(across(Location, factor, levels = c ("Berle", "Høyen", "Sando", "Lofthus", "Urheim", "Djønno"))) %>% 
  ggplot(aes(y = Diameter, x = Seeds_fully_developed, color = Treatment)) +
  geom_smooth(method = lm, fullrange=FALSE, aes(fill=Treatment)) +
  labs(x = "Antall frø per eple", y = "Diameter (mm)", color = "Behandling", fill = "Behandling") +
  theme_classic() +
  scale_color_manual (labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  scale_fill_manual(labels = c("Pollinatorer ekskludert (C)", "Håndpollinert (HP)", "Naturlig pollinert (N)"), values = c("#996600", "#336633", "#CC3300")) +
  ylim(50, 95) + #REMOVED TWO OUTLIERS
  ggtitle("Korrelasjon mellom diameter og antall frø") +
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
