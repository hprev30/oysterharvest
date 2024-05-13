##### packages-----
library(ggplot2)
library(dplyr)
library(glmmTMB)a
library(lme4)
library(car)
library(DHARMa)
library(performance)
library(emmeans)
library(estimability)
install.packages('estimability')

##### data----
data = read.csv("C:/Users/Prevost_H/Desktop/HE-HP/15-16Reef.csv")
data$Harvest = as.factor(data$Harvest)
data$Region = as.factor(data$Region)
Sh = read.csv("C:/Users/Prevost_H/Desktop/HE-HP/15-16Oys.csv")
cultch = read.csv("Z:/Research/Manuscripts/oyster harvest effects/Hans Graphics/Cultch.csv")
data$Harvest<-replace(data$Harvest, data$Harvest=='Y', "Yes")
data$Harvest<-replace(data$Harvest, data$Harvest=='N', "No")
Sh$Harvest<-replace(Sh$Harvest, Sh$Harvest=='Y', "Yes")
Sh$Harvest<-replace(Sh$Harvest, Sh$Harvest=='N', "No")
cultch$Harvest<-replace(cultch$Harvest, cultch$Harvest=='Y', "Yes")
cultch$Harvest<-replace(cultch$Harvest, cultch$Harvest=='N', "No")

Sh2v = c('Region', 'Harvest', 'Shell')

##### making means------
means_oys = data %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Oys, na.rm = T))
means_mus = data %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Mus, na.rm = T))
means_clus = data %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Cluster, na.rm = T))
Sh_CollectionAvg = Sh %>% group_by(Harvest, Region, Collection)  %>%
  summarize(mean = mean(Shell, na.rm = T))
Sh_Avg = Sh_CollectionAvg %>% group_by(Harvest, Region)  %>%
  summarize(mean = mean(mean, na.rm = T))
means_sh = Sh %>% group_by(Harvest, Region) %>% 
  summarize(mean = mean(Shell, na.rm = T))
means_cultch = cultch %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Cultch, na.rm = T))

##### violin plots-----
a_oys = data %>% 
  ggplot(aes(x = Harvest, y = Oys, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_oys, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_oys, aes(x = Harvest, y = mean, 
                                              label = round(mean, digits = 2),
                                              family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
                                         plot.title = element_text(hjust = 0.5)) +
  labs(x = "Harvest", y = "Oyster Counts")+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) +  ggtitle("Oyster Counts by Region")

a_oys 

a_mus = data %>% 
  ggplot(aes(x = Harvest, y = Mus, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_mus, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_mus, 
                            aes(x = Harvest, y = mean, 
                                label = round(mean, digits = 2),
                                              family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Harvest", y = "Mussel Counts")+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) + ggtitle("Mussel Counts by Region")

a_mus

a_clus = data %>% 
  ggplot(aes(x = Harvest, y = Cluster, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_clus, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_clus, aes(x = Harvest, y = mean, label = round(mean, digits = 2),
                                                  family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Harvest", y = "Cluster Counts")+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) + ggtitle("Cluster Counts by Region")

a_clus

a_sh = Sh_CollectionAvg %>% 
  ggplot(aes(x = Harvest, y = mean, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_sh, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_sh, aes(x = Harvest, y = mean, label = round(mean, digits = 2),
                                                   family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Harvest", y = "Average Shell Height (mm)")+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) + ggtitle("Average Shell Heights")

a_sh

a_cultch = cultch %>% 
  ggplot(aes(x = Harvest, y = Cultch, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_cultch, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_cultch, aes(x = Harvest, y = mean, label = round(mean, digits = 2),
                                                 family = "serif"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "Harvest", y = "Cultch Mass (g)")+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) + ggtitle("Cultch Mass by Region")

a_cultch

##### SH histogram-----

hist_sh <- Sh %>%
  ggplot(aes(x=Shell, fill=Harvest, color = Harvest)) +
  geom_histogram(alpha=0.5, position = 'identity') + 
  scale_fill_manual(values = harvest_colors) + 
  scale_color_manual(values = harvest_colors) +
  labs(x = "Shell Height (mm)", y = "Frequency") + facet_wrap(~Region) +
  theme(panel.background = element_blank(), axis.line=element_line(size=1), 
        plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 75, linetype = "dashed") + 
  ggtitle("Oyster Measurement Frequency by Region")
   

hist_sh

write.csv(Sh, "Sh.csv")

##### colors----
harvest_fill = c("Y" = 'blue', 'N' = "yellow3")
harvest_colors = c("Yes" = 'black', "No" = 'gray49')

##### data analysis -----
data$Harvest = as.factor(data$Harvest)
data$Region = as.factor(data$Region)


#live oyster counts model fitting
glmm_oys1 = glmer.nb(Oys ~ Harvest + (1| ReefID) + (1|Region), data = data) #3276.2
glmm_oys2 = glmer.nb(Oys ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) #3276.4
glmm_oys3 = glmer.nb(Oys ~ Harvest + (1|ReefID), data = data) #3277.8
glmm_oys4 = glmer.nb(Oys ~ Harvest + (1|Region), data = data) #3333.3
glmm_oys5 = glmer.nb(Oys ~ Harvest * Region + (1|ReefID), data = data) #3263.6

Anova(glmm_oys5)
summary(glmm_oys5)

res <- DHARMa::simulateResiduals(glmm_oys5)
plot(res)
testResiduals(res)

performance::check_autocorrelation(glmm_oys5) # Durbin-Watson-Test

m.region <- emmeans(glmm_oys5, ~ region)

#cultch model fitting
glmm_cultch1 = glmer.nb(Cultch ~ Harvest * Region + (1|Reef), data = cultch)
summary(glmm_cultch1)
#oyster length


#Checking for overdisperion-----
#Cluster counts: var = 62.65978, mean = 13.07591, means over dispersion, meaning negative binomial distribution for clusters
#Oys counts: var = 3889.65, mean = 89.51911, means over dispersion, meaning negative binomial distribution for clusters
mean(data$Cluster, na.rm = TRUE)
var(data$Cluster, na.rm = TRUE)
var(data$Oys, na.rm = TRUE)
mean(data$Oys, na.rm = TRUE)
