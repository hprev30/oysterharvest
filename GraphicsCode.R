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
library(ggsignif)
library(bbmle)
library(AICcmodavg)
install.packages('estimability')
install.packages('readr')
install.packages('ggsignif')
install.packages('bbmle')
install.packages('AICcmodavg')

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

data$Oys = data$Oys*16 #scaling up to a meter squared
data$Mus = data$Mus*16
cultch$Cultch = cultch$Cultch*16
cultch$Cultch = cultch$Cultch/100

means_oys = data %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Oys, na.rm = T)) 
means_oys$mean = round(means_oys$mean, 0)

means_mus = data %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Mus, na.rm = T))
means_mus$mean = round(means_mus$mean, 0)

means_clus = data %>% group_by(Harvest, Region) %>%
  summarize(mean = mean(Cluster, na.rm = T))
means_clus$mean = round(means_clus$mean, 0)

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
  theme_bw(base_family = "TT Times New Roman") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
                                         plot.title = element_text(hjust = 0.5)) +
  labs(x = "In Harvest Zone?", y = bquote(Density~(Oysters/m^2)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) 

a_oys

ann_text2 = data.frame(Harvest = 'Yes', Oys = 8500, lab = "A",
                       Region = factor('Matanzas River',levels = c("Matanzas River", "Salt Run", "Tolomato River")))
ann_text3 = data.frame(Harvest = 'Yes', Oys = 8500, lab = "B*",
                       Region = factor('Salt Run',levels = c("Matanzas River", "Salt Run", "Tolomato River")))
ann_text4 = data.frame(Harvest = 'Yes', Oys = 8500, lab = "AB*",
                       Region = factor('Tolomato River',levels = c("Matanzas River", "Salt Run", "Tolomato River")))


a_oys2= a_oys + geom_text(data = ann_text2, label = "A", color='black', hjust = 7.50) +
  geom_text(data = ann_text3, label = "B*", color = 'black', hjust = 4.00) +
  geom_text(data = ann_text4, label = "AB*", color = 'black', hjust = 3.00)
  
a_oys2


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
  theme_bw(base_family = "times new roman") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "In Harvest Zone?", y = bquote(Density~(Mussels/m^2)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) 

ann_text7 = data.frame(Harvest = 'Yes', Mus = 2000, lab = "A",
                       Region = factor('Matanzas River',levels = c("Matanzas River", "Salt Run", "Tolomato River")))
ann_text8 = data.frame(Harvest = 'Yes', Mus = 2000, lab = "A*",
                       Region = factor('Salt Run',levels = c("Matanzas River", "Salt Run", "Tolomato River")))
ann_text9 = data.frame(Harvest = 'Yes', Mus = 2000, lab = "A",
                       Region = factor('Tolomato River',levels = c("Matanzas River", "Salt Run", "Tolomato River")))


a_mus2= a_mus + geom_text(data = ann_text7, label = "AB*", color='black', hjust = 3.00) +
  geom_text(data = ann_text8, label = "A*", color = 'black', hjust =4.50) +
  geom_text(data = ann_text9, label = "B", color = 'black', hjust = 7.00)

a_mus2



a_clus = data %>% 
  ggplot(aes(x = Harvest, y = Cluster, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_clus, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_clus, aes(x = Harvest, y = mean, 
                                                   label = round(mean, digits = 2),
                                                  family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "times new roman") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "In Harvest Zone?", y = bquote(Density~(Clusters/m^2)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines')) 

ann_text10 = data.frame(Harvest = 'Yes', Cluster = 55, lab = "A",
                       Region = factor('Matanzas River',levels = c("Matanzas River", "Salt Run", "Tolomato River")))
ann_text11 = data.frame(Harvest = 'Yes', Cluster = 55, lab = "B*",
                       Region = factor('Salt Run',levels = c("Matanzas River", "Salt Run", "Tolomato River")))
ann_text12 = data.frame(Harvest = 'Yes', Cluster = 55, lab = "A",
                       Region = factor('Tolomato River',levels = c("Matanzas River", "Salt Run", "Tolomato River")))


a_clus2= a_clus + geom_text(data = ann_text10, label = "A", color='black', hjust = 7.50) +
  geom_text(data = ann_text11, label = "B*", color = 'black', hjust =4.50) +
  geom_text(data = ann_text12, label = "A", color = 'black', hjust = 7.50)



a_clus2

#a_sh = Sh_CollectionAvg %>% 
  #ggplot(aes(x = Harvest, y = mean, color = Harvest)) + geom_violin(trim = F) +
  #geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  #geom_point(data = means_sh, aes(x = Harvest, y = mean, size = 4)) +
  #scale_color_manual(values = harvest_colors) +
  #ggrepel::geom_label_repel(data = means_sh, aes(x = Harvest, y = mean, label = round(mean, digits = 2),
                                                  # family = "times new roman"), size = 3.3,
                           # nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  #theme_bw(base_family = "serif") +
  #theme(axis.text = element_text(color = "black"),
       # legend.position = "none", panel.background = element_blank(),
       # plot.title = element_text(hjust = 0.5)) +
  #labs(x = "Harvest", y = "Average Shell Height (mm)")+ facet_wrap(~Region) +
  #theme(panel.spacing = unit(0, 'lines')) + ggtitle("Average Shell Heights")

#a_sh

a_cultch = cultch %>% 
  ggplot(aes(x = Harvest, y = Cultch, color = Harvest)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means_cultch, aes(x = Harvest, y = mean, size = 4)) +
  scale_color_manual(values = harvest_colors) +
  ggrepel::geom_label_repel(data = means_cultch, aes(x = Harvest, y = mean, 
                                                     label = round(mean, digits = 2),
                                                 family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "times new roman") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "In Harvest Zone?", y = bquote(Cultch~Mass~(kg)))+ facet_wrap(~Region) +
  theme(panel.spacing = unit(0, 'lines'))

a_cultch

ann_text5 = data.frame(Harvest = 'Yes', Cultch = 3500, lab = "A",
                       Region = factor('Matanzas River',levels = c("Matanzas River", "Salt Run")))
ann_text6 = data.frame(Harvest = 'Yes', Cultch = 3500, lab = "A*",
                       Region = factor('Salt Run',levels = c("Matanzas River", "Salt Run")))



a_cultch2= a_cultch + geom_text(data = ann_text5, label = "A", color='black', hjust = 11.0) +
  geom_text(data = ann_text6, label = "A*", color = 'black', hjust =6.50) 


a_cultch2

##### SH histogram-----

hist_sh <- Sh %>%
  ggplot(aes(x=Shell, fill=Harvest, color = Harvest)) +
  geom_histogram(alpha=0.5, position = 'identity') + 
  scale_fill_manual(legend_title, values = harvest_colors) + 
  scale_color_manual(legend_title, values = harvest_colors) +
  labs(x = "Shell Height (mm)", y = "Frequency") + facet_grid(Harvest ~ Region) +
  theme(panel.background = element_blank(), axis.line=element_line(size=1), 
        plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 75, linetype = "dashed")
  #geom_vline(xintercept = 25, linetype = 'dashed') +
  

legend_title <- "In Harvest Zone?"

hist_sh

write.csv(Sh, "Sh.csv")

##### colors----
harvest_fill = c("Y" = 'blue', 'N' = "yellow3")
harvest_colors = c("Yes" = 'black', "No" = 'gray49')
region_colors = c('Salt Run' = 'yellow3', 'Matanzas River' = 'blue', 'Tolomato River' = 'red')

##### data analysis -----



##### live oyster density model fitting------
glmm_oys0 = glmmTMB::glmmTMB(Oys ~ Harvest, data = data, family = "nbinom2")
glmm_oys1 = glmer.nb(Oys ~ Harvest + (1|ReefID), data = data) #3277.8
glmm_oys2 = glmer.nb(Oys ~ Harvest + (1|Region), data = data) #3333.3
glmm_oys3 = glmer.nb(Oys ~ Harvest + (1| ReefID) + (1|Region), data = data) #3276.2
glmm_oys4 = glmer.nb(Oys ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) #3276.4
glmm_oys5 = glmer.nb(Oys ~ Harvest * Region + (1|ReefID), data = data) #3263.6
glmm_oys6 = glmer.nb(Oys ~ Harvest * Region + (1|ReefID) + (1|Sample), data = data)
plot(glmm_oys5)

modset = list(glmm_oys0, glmm_oys1,glmm_oys2, glmm_oys3, glmm_oys4, glmm_oys5, glmm_oys6)
modnames2 = c("intercept", "oys ~ harvest + Reef", "oys ~ harvest + region", 
              "oys ~ harvest + reef + region", "oys~harvest + region + sample", 
              "oys ~ harvest * region + reef", "oys~harvest*region+reef+sample")
names(modset) <- modnames2

bbmle::AICctab(modset, weights = TRUE, base = T)

AICcmodavg::aictab(modset, modnames2, second.ord = TRUE) #model selection table with AICc

#estimated marginal means
AIC(glmm_oys0, glmm_oys1, glmm_oys2, glmm_oys3, glmm_oys4, glmm_oys5, glmm_oys6)
m.region <- emmeans(glmm_oys5, ~ Region)

Anova(glmm_oys5)
summary(glmm_oys5)

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
glmm_cultch0 = glmmTMB::glmmTMB(Cultch ~ Harvest, data = cultch, family = "gaussian")
glmm_cultch1 = lmer(Cultch ~ Harvest + (1|Reef), data = cultch) #3277.8
glmm_cultch2 = lmer(Cultch ~ Harvest + (1|Region), data = cultch) #3333.3
glmm_cultch3 = lmer(Cultch ~ Harvest + (1|Reef) + (1|Region), data = cultch) #3276.2
glmm_cultch4 = lmer(Cultch ~ Harvest * Region + (1|Reef), data = cultch)

modset_cultch = list(glmm_cultch0, glmm_cultch1,glmm_cultch2, glmm_cultch3, glmm_cultch4)
modnames3 = c("intercept", "cultch ~ harvest + Reef", "cultch ~ harvest + region", 
              "cultch ~ harvest + reef + region","oys ~ harvest * region + reef")
names(modset_cultch) <- modnames3

bbmle::AICctab(modset_cultch, weights = TRUE, base = T)

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

contrast(region_cultchEMM, method = 'pairwise', adjust = 'tukey')
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
glmm_mus0 = glmmTMB::glmmTMB(Mus ~ Harvest, data = data, family = "nbinom2")
glmm_mus1 = glmer.nb(Mus ~ Harvest + (1|ReefID), data = data) 
glmm_mus2 = glmer.nb(Mus ~ Harvest + (1|Region), data = data)
glmm_mus3 = glmer.nb(Mus ~ Harvest + (1|ReefID) + (1|Region), data = data) 
glmm_mus4 = glmer.nb(Mus ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) 
glmm_mus5 = glmer.nb(Mus ~ Harvest * Region + (1|ReefID), data = data) 
glmm_mus6 = glmer.nb(Mus ~ Harvest * Region + (1|ReefID) + (1|Sample), data = data) 

modset_mus = list(glmm_mus0, glmm_mus1, glmm_mus2, glmm_mus3, glmm_mus4, glmm_mus5, glmm_mus6)
modnames4 = c("intercept", "mus ~ harvest + Reef", "mus ~ harvest + region", 
              "mus ~ harvest + reef + region","mus ~ harvest + reef + region + sample", 
              "mus ~ harvest * region + reef", "mus ~ harvest * region + reef + sample")
names(modset_mus) <- modnames4

bbmle::AICctab(modset_mus, weights = TRUE, base = T)







AIC(glmm_mus3, glmm_mus4, glmm_mus5)
summary(glmm_mus5)
Anova(glmm_mus5)
contrast(mus.region, method = 'pairwise', adjust='tukey')
mus.region <- emmeans(glmm_mus5, ~ Region)
mus.region 

SRMus = glmer.nb(Mus ~ Harvest + (1|ReefID), data = SROys)
SR_musEMM = emmeans(SRMus, ~Harvest)
contrast(SR_musEMM, method = 'pairwise', adjust='tukey')

TRMus = glmer.nb(Mus ~ Harvest + (1|ReefID), data = TROys)
Trmusemm = emmeans(TRMus, ~Harvest)
contrast(Trmusemm, method = 'pairwise', adjust='tukey')

MRMus = glmer.nb(Mus ~ Harvest  + (1|ReefID), data = MROys)
MRmusEMM = emmeans(MRMus, ~Harvest)
contrast(MRmusEMM, method = 'pairwise', adjust='tukey')
##### Harvest on clusters--------
glmm_clus0 = glmmTMB::glmmTMB(Cluster ~ Harvest, data = data, family = "nbinom2")
glmm_clus1 = glmer.nb(Cluster ~ Harvest + (1|ReefID), data = data) 
glmm_clus2 = glmer.nb(Cluster ~ Harvest + (1|Region), data = data)
glmm_clus3 = glmer.nb(Cluster ~ Harvest + (1|ReefID) + (1|Region), data = data) 
glmm_clus4 = glmer.nb(Cluster ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) 
glmm_clus5 = glmer.nb(Cluster ~ Harvest*Region  + (1|ReefID), data = data) 
glmm_clus6 = glmer.nb(Cluster ~ Harvest*Region  + (1|ReefID) + (1|Sample), data = data) 

modset_clus = list(glmm_clus0, glmm_clus1, glmm_clus2, glmm_clus3, glmm_clus4, glmm_clus5, glmm_clus6)
modnames5 = c("intercept", "clus ~ harvest + Reef", "clus ~ harvest + region", 
              "clus ~ harvest + reef + region","clus ~ harvest + reef + region + sample", 
              "clus ~ harvest * region + reef", "clus ~ harvest * region + reef + sample")
names(modset_clus) <- modnames5

bbmle::AICctab(modset_clus, weights = TRUE, base = T)


AIC(glmm_clus1, glmm_clus2, glmm_clus3, glmm_clus4, glmm_clus5)
summary(glmm_clus5)
Anova(glmm_clus5)
contrast(clus.region, method = 'pairwise', adjust='tukey')
clus.region <- emmeans(glmm_clus5, ~ Region)
clus.region

SRClus =  glmer.nb(Cluster ~ Harvest + (1|ReefID), data = SROys) 
SRClusEMM = emmeans(SRClus, ~Harvest)
contrast(SRClusEMM, method = 'pairwise', adjust='tukey')

MRClus =  glmer.nb(Cluster ~ Harvest + (1|ReefID), data = MROys) 
MRClusEMM = emmeans(MRClus, ~Harvest)
contrast(MRClusEMM, method = 'pairwise', adjust='tukey')

TRClus =  glmer.nb(Cluster ~ Harvest + (1|ReefID), data = TROys) 
TRClusEMM = emmeans(TRClus, ~Harvest)
contrast(TRClusEMM, method = 'pairwise', adjust='tukey')

##### Harvest on spat------
glmm_spat0 = glmmTMB::glmmTMB(Spat ~ Harvest, data = data, family = "nbinom2")
glmm_spat1 = glmer.nb(Spat ~ Harvest + (1|ReefID), data = data) 
glmm_spat2 = glmer.nb(Spat ~ Harvest + (1|Region), data = data)
glmm_spat3 = glmer.nb(Spat ~ Harvest + (1|ReefID) + (1|Region), data = data) 
glmm_spat4 = glmer.nb(Spat ~ Harvest + (1|ReefID) + (1|Region) + (1|Sample), data = data) 
glmm_spat5 = glmer.nb(Spat ~ Harvest * Region + (1|ReefID), data = data) 
glmm_spat6 = glmer.nb(Spat ~ Harvest * Region + (1|ReefID) + (1|Sample), data = data)  
glmm_spat7 = glmer.nb(Spat ~ Harvest * Sample + (1|ReefID) + (1|Region), data = data)

modset_spat= list(glmm_spat0, glmm_spat1, glmm_spat2, glmm_spat3, glmm_spat4, glmm_spat5, glmm_spat6, glmm_spat7)
modnames6 = c("m0intercept", "m1spat ~ harvest + Reef", "m2spat ~ harvest + region", 
              "m3spat ~ harvest + reef + region","m4spat ~ harvest + reef + region + sample", 
              "m5spat ~ harvest * region + reef", "m6spat ~ harvest * region + reef + sample",
              "m7spat ~ harvest * season + reef + region")
names(modset_spat) <- modnames6

bbmle::AICctab(modset_spat, weights = TRUE, base = T)
AIC(glmm_spat1, glmm_spat2, glmm_spat3, glmm_spat4, glmm_spat5, glmm_spat6, glmm_spat7)


summary(glmm_spat7)
Anova(glmm_spat7)
Anova(glmm_spat6)
contrast(spat.region, method = 'pairwise', adjust='tukey')
contrast(spat.season, method = 'pairwise', adjust='tukey')
spat.region <- emmeans(glmm_spat7, ~Region)
spat.season = emmeans(glmm_spat7, ~Sample)
spat.region


#####Checking for overdisperion-----
#Cluster counts: var = 62.65978, mean = 13.07591, means over dispersion, meaning negative binomial distribution for clusters
#Oys counts: var = 3889.65, mean = 89.51911, means over dispersion, meaning negative binomial distribution for clusters
mean(data$Cluster, na.rm = TRUE)
var(data$Cluster, na.rm = TRUE)
var(data$Oys, na.rm = TRUE)
mean(data$Oys, na.rm = TRUE)

#####