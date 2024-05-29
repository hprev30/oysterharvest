##### packages-----
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(lme4)
library(car)
library(DHARMa)
library(performance)
library(emmeans)
library(estimability)
library(readr)
install.packages('estimability')
install.packages('readr')

##### data----
#reading in data
data = read_csv("https://raw.githubusercontent.com/hprev30/oysterharvest/main/15-16Reef.csv")
Sh = read_csv("https://raw.githubusercontent.com/hprev30/oysterharvest/main/Sh.csv")
cultch = read_csv("https://raw.githubusercontent.com/hprev30/oysterharvest/main/Cultch.csv")


#making Y and N actual words
data$Harvest<-replace(data$Harvest, data$Harvest=='Y', "Yes")
data$Harvest<-replace(data$Harvest, data$Harvest=='N', "No")
Sh$Harvest<-replace(Sh$Harvest, Sh$Harvest=='Y', "Yes")
Sh$Harvest<-replace(Sh$Harvest, Sh$Harvest=='N', "No")
cultch$Harvest<-replace(cultch$Harvest, cultch$Harvest=='Y', "Yes")
cultch$Harvest<-replace(cultch$Harvest, cultch$Harvest=='N', "No")


#subsetting datasets
SRCultch = cultch[which(cultch$Region == 'Salt Run'),]
MRCultch = cultch[which(cultch$Region == 'Matanzas River'),]
SROys = data[which(data$Region == 'Salt Run'),]
MROys = data[which(data$Region == 'Matanzas River'),]
TROys = data[which(data$Region == 'Tolomato River'),]
Sh2v = c('Region', 'Harvest', 'Shell')
spat = Sh[which(Sh$Class == 'Spat'),]
SRSH = Sh[which(Sh$Region == 'Salt Run'),]
MRSH = Sh[which(Sh$Region == 'Matanzas River'),]
TRSH = Sh[which(Sh$Region == 'Tolomato River'),]
TRSHYes = TRSH[which(TRSH$Harvest == 'Yes'),]
TRSHNo = TRSH[which(TRSH$Harvest == 'No'),]
SRSHYes = SRSH[which(SRSH$Harvest == 'Yes'),]
SRSHNo = SRSH[which(SRSH$Harvest == 'No'),]
MRSHYes = MRSH[which(MRSH$Harvest == 'Yes'),]
MRSHNo = MRSH[which(MRSH$Harvest == 'No'),]



#making factors
data$Harvest = as.factor(data$Harvest)
data$Region = as.factor(data$Region)


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

##### violin-jitter plots-----
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
  labs(x = "Harvest", y = bquote(Density~(Oysters/0.0625~m^2)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) +  ggtitle("Oyster Density by Region") 

ann_text <- data.frame(Harvest = 'No',Oys = 400,lab = "ab",
                       Region = factor('Matanzas River',levels = c("Matanzas River","Salt Run","Tolomato River")))

a_oys + geom_text(data = ann_text,label = "ab")
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
  labs(x = "Harvest", y = bquote(Density~(Mussels/0.0625~m^2)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) + ggtitle("Mussel Density by Region")

a_mus

a_clus = data %>% 
  ggplot(aes(x = Harvest, y = Cluster, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_clus, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_clus, aes(x = Harvest, y = mean, 
                                                   label = round(mean, digits = 2),
                                                  family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Harvest", y = bquote(Density~(Clusters/m^2)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) + ggtitle("Cluster Density by Region")

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
  ggrepel::geom_label_repel(data = means_cultch, aes(x = Harvest, y = mean, 
                                                     label = round(mean, digits = 2),
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
  labs(x = "Shell Height (mm)", y = "Frequency") + facet_grid(Harvest ~ Region) +
  theme(panel.background = element_blank(), axis.line=element_line(size=1), 
        plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 75, linetype = "dashed") + #geom_vline(xintercept = 25, linetype = 'dashed') +
  ggtitle("Oyster Height Frequency")
   

hist_sh

write.csv(Sh, "Sh.csv")

##### colors----
harvest_fill = c("Y" = 'blue', 'N' = "yellow3")
harvest_colors = c("Yes" = 'black', "No" = 'gray49')
region_colors = c('Salt Run' = 'yellow3', 'Matanzas River' = 'blue', 'Tolomato River' = 'red')

##### data analysis -----



##### live oyster density model fitting------
glmm_oys1 = glmer.nb(Oys ~ Harvest + (1| ReefID) + (1|Region), data = data) #3276.2
glmm_oys2 = glmer.nb(Oys ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) #3276.4
glmm_oys3 = glmer.nb(Oys ~ Harvest + (1|ReefID), data = data) #3277.8
glmm_oys4 = glmer.nb(Oys ~ Harvest + (1|Region), data = data) #3333.3
glmm_oys5 = glmer.nb(Oys ~ Harvest * Region + (1|ReefID), data = data) #3263.6
glmm_oys6 = glmer.nb(Oys ~ Harvest * Region + (1|ReefID) + (1|Sample), data = data)
plot(glmm_oys5)

#estimated marginal means
AIC(glmm_oys1, glmm_oys2, glmm_oys3, glmm_oys4, glmm_oys5, glmm_oys6)
m.region <- emmeans(glmm_oys6, ~ Region)

Anova(glmm_oys6)
summary(glmm_oys6)

res <- DHARMa::simulateResiduals(glmm_oys5)
plot(res)
testResiduals(res)

performance::check_autocorrelation(glmm_oys5) # Durbin-Watson-Test to test for autocorrelation in a regression models output


contrast(m.region, method = 'pairwise', adjust = 'tukey')
plot(m.region, comparisons = T)

#site specific oys density comparisons -- Salt Run
glmm_oysSR = lmer(Oys ~ Harvest + (1|ReefID), 
                    data = SROys)
summary(glmm_oysSR)
Anova(glmm_oysSR)
SRc.region <- emmeans(glmm_oysSR, ~ Harvest)
contrast(SRc.region, method = 'pairwise', adjust = 'tukey')

#site specific oyster density comparisons -- Tolomato River
glmm_oysTR = lmer(Oys ~ Harvest + (1|ReefID), 
                  data = TROys)
summary(glmm_oysTR)
Anova(glmm_oysTR)
TRo.harvest <- emmeans(glmm_oysTR, ~ Harvest)
contrast(TRo.harvest, method = 'pairwise', adjust = 'tukey')

#site specific oyster density comparisons -- Matanzas River
glmm_oysMR = lmer(Oys ~ Harvest + (1|ReefID), 
                  data = MROys)
summary(glmm_oysMR)
Anova(glmm_oysMR)
MRo.harvest <- emmeans(glmm_oysMR, ~ Harvest)
contrast(MRo.harvest, method = 'pairwise', adjust = 'tukey')

# blue bars are the confidence intervals
# red arrows represent a scheme to determine homogeneous groups
# if the red lines overlap for two groups, they are not significantly different using the method chosen
summary(m.region)

##### cultch model fitting-----
glmm_cultch1 = lmer(Cultch ~ Harvest * Region + (1|Reef), 
                    data = cultch)

gc1r = residuals(glmm_cultch1)
shapiro.test(gc1r)
plot(gc1r)
glmm_cultch2 = lmer(Cultch ~ Harvest + (1|Region) + (1|Reef), 
                    data = cultch, REML = FALSE)

c.region <- emmeans(glmm_cultch1, ~ Region)
contrast(c.region, method = 'pairwise', adjust='tukey')
hist(cultch$Cultch)
summary(glmm_cultch1)
summary(glmm_cultch2)
Anova(glmm_cultch1)
AIC(glmm_cultch1, glmm_cultch2)
AIC(glmm_cultch2)

res_cultch <- DHARMa::simulateResiduals(glmm_cultch1)
plot(res_cultch)
testResiduals(res_cultch)

performance::check_autocorrelation(glmm_cultch1)

#cultch mass in just Salt Run
glmm_cultch3 = lmer(Cultch ~ Harvest + (1|Reef), 
                    data = SRCultch)
summary(glmm_cultch3)
Anova(glmm_cultch3)
SRc.region <- emmeans(glmm_cultch3, ~ Harvest)
contrast(SRc.region, method = 'pairwise', adjust = 'tukey')

#cultch mass in just Matanzas River
glmm_cultch4 = lmer(Cultch ~ Harvest + (1|Reef), 
                    data = MRCultch)
summary(glmm_cultch4)
Anova(glmm_cultch4)
SRc.region <- emmeans(glmm_cultch4, ~ Harvest)
contrast(SRc.region, method = 'pairwise', adjust = 'tukey')

##### oyster length KS testing-----
MRSH.test = ks.test(MRSHNo$Shell, MRSHYes$Shell)
MRSH.test
TRSH.test = ks.test(TRSHNo$Shell, TRSHYes$Shell)
TRSH.test
SRSH.test = ks.test(SRSHNo$Shell, SRSHYes$Shell)
SRSH.test
summary(SRSH.test)
test = ks.test(SRSHNo$Shell, SRSHNo$Shell)
test
MTSH.test = ks.test(SRSHYes$Shell, TRSHYes$Shell)
MTSH.test

MRSH.atest = Anova(MRSHNo$Shell, MRSHYes$Shell)
MRSH.atest
TRSH.atest = anova(TRSHNo$Shell, TRSHYes$Shell)
TRSH.atest
SRSH.atest = anova(SRSHNo$Shell, SRSHYes$Shell)
##### harvest on mussels-----
glmm_mus1 = glmer.nb(Mus ~ Harvest + (1| ReefID) + (1|Region), data = data) 
glmm_mus2 = glmer.nb(Mus ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) 
glmm_mus3 = glmer.nb(Mus ~ Harvest + (1|ReefID), data = data) 
glmm_mus4 = glmer.nb(Mus ~ Harvest + (1|Region), data = data)
glmm_mus5 = glmer.nb(Mus ~ Harvest * Region + (1|ReefID), data = data) 
AIC(glmm_mus3, glmm_mus4, glmm_mus5)
summary(glmm_mus5)
Anova(glmm_mus5)
contrast(mus.region, method = 'pairwise', adjust='tukey')
mus.region <- emmeans(glmm_mus5, ~ Region)
mus.region

##### Harvest on clusters--------
glmm_clus1 = glmer.nb(Cluster ~ Harvest + (1| ReefID) + (1|Region), data = data) 
glmm_clus2 = glmer.nb(Cluster ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) 
glmm_clus3 = glmer.nb(Cluster ~ Harvest + (1|ReefID), data = data) 
glmm_clus4 = glmer.nb(Cluster ~ Harvest + (1|Region), data = data)
glmm_clus5 = glmer.nb(Cluster ~ Harvest * Region + (1|ReefID), data = data) 

AIC(glmm_clus1, glmm_clus2, glmm_clus3, glmm_clus4, glmm_clus5)
summary(glmm_clus5)
Anova(glmm_clus5)
contrast(clus.region, method = 'pairwise', adjust='tukey')
clus.region <- emmeans(glmm_clus5, ~ Region)
clus.region


##### Harvest on spat------
glmm_spat1 = glmer.nb(Spat ~ Harvest + (1| ReefID) + (1|Region), data = data) 
glmm_spat2 = glmer.nb(Spat ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) 
glmm_spat3 = glmer.nb(Spat ~ Harvest + (1|ReefID), data = data) 
glmm_spat4 = glmer.nb(Spat ~ Harvest + (1|Region), data = data)
glmm_spat5 = glmer.nb(Spat ~ Harvest * Region + (1|ReefID), data = data) 
glmm_spat6 = glmer.nb(Spat ~ Harvest * Region * Sample + (1|ReefID), data = data)  #failed to converge
AIC(glmm_spat1, glmm_spat2, glmm_spat3, glmm_spat4, glmm_spat5)


summary(glmm_spat2)
Anova(glmm_spat2)
contrast(spat.region, method = 'pairwise', adjust='tukey')
spat.region <- emmeans(glmm_spat2, ~Harvest)
clus.region
#####Checking for overdisperion-----
#Cluster counts: var = 62.65978, mean = 13.07591, means over dispersion, meaning negative binomial distribution for clusters
#Oys counts: var = 3889.65, mean = 89.51911, means over dispersion, meaning negative binomial distribution for clusters
mean(data$Cluster, na.rm = TRUE)
var(data$Cluster, na.rm = TRUE)
var(data$Oys, na.rm = TRUE)
mean(data$Oys, na.rm = TRUE)

#####