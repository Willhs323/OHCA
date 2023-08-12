# Clean work space
rm(list=ls(all=T))
options(stringAsFactors = F)
options("scipen" = 100, "digits" = 4)

setwd("/Users/M276066/Documents/Siontis research/OHCAHCM/")
hcm <- read.csv("OOHCA FINAL DATA R(1) 23.05.02.csv", na.strings = c("", "NA"))
hcm <- hcm[1:27,]
View(hcm)

library(descr)
library(survival)
library(ggplot2)
library(ggpubr)
library(survminer)
library(dplyr)

# Age
hist(hcm[,c(4)])
mean(hcm[,c(4)])
sd(hcm[,c(4)])
summary(hcm[,c(4)])
freq(hcm[,c(4)])
hist(hcm[,c(4)])

# Sex
freq(hcm[,c(5)])

# Ethnicity
freq(hcm[,c(6)])

# Race
freq(hcm[,c(7)])

# Time to eval
hist(hcm[,c(10)])
summary(hcm[,c(10)]) / 365

# Dx location
freq(hcm[,c(11)])

# Dx imaging
freq(hcm[,c(12)])

# Obstructive
freq(hcm[,c(13)])

# Type
freq(hcm[,c(14)])

# Rhythm
freq(hcm[,c(15)])

# Genetic testing
freq(hcm[,c(18)])

# Pathological variant
freq(hcm[,c(19)])
# Variants
freq(hcm[,c(20)])

# Comorbidities
freq(hcm[,c(22)])
freq(hcm[,c(23)])

# TTE
  # Subtype
  freq(hcm[,c(14)])
  # Time OHCA to TTE
  hist(hcm[,c(25)])
  summary(hcm[,c(25)])/365
  # IV septum
  summary(hcm[,c(26)])
  hist(hcm[,c(26)])
  shapiro.test(hcm[,c(26)])
  mean(hcm[,c(26)])
  sd(hcm[,c(26)])
  # Posterior wall thickness
  hist(hcm[,c(27)])
  summary(hcm[,c(27)])
  mean(hcm[,c(27)])
  sd(hcm[,c(27)])
  shapiro.test(hcm[,c(27)])
  # LV mass (grams)
  hist(hcm[,c(28)])
  shapiro.test(hcm[,c(28)])
  summary(hcm[,c(28)])
  # LA Volume index
  summary(hcm[,c(29)])
  hist(hcm[,c(29)])
  shapiro.test(hcm[,c(29)])
  # EF
  summary(hcm[,c(30)])
  hist(hcm[,c(30)])
  shapiro.test(hcm[,c(30)])
  mean(hcm[,c(30)])
  sd(hcm[,c(30)])
  # EF < 50%
  freq(hcm[,c(30)] <= 50)
  # Apical pouch
  freq(hcm[,c(31)])
  # apical aneurysm
  freq(hcm[,c(82)])
  # LVOT at rest
  summary(hcm[,c(32)])
  freq(hcm[,c(32)])
  freq(hcm[,c(32)] >= 30)
  # LVOT dynamic
  summary(hcm[,c(33)])
  freq(hcm[,c(33)])
  freq(hcm[,c(32)] < 30 & hcm[,c(33)] >= 30)
  # SAM
  freq(hcm[,c(34)])
  # Mitral regurg
  freq(hcm[,c(35)])
  # RVSP
  hcm[,c(36)]
  summary(hcm[,c(36)])
  hist(hcm[,c(36)])
  shapiro.test(hcm[,c(36)])
  mean(hcm[-c(22, 26),c(36)])
  sd(hcm[-c(22, 26),c(36)])
# CMR
freq(hcm[,c(37)])
# days OHCA to CMR
hist(hcm[,c(38)])
summary(hcm[,c(38)])/365
# LGE present
freq(hcm[,c(39)])
# LGE > 15%
freq(hcm[,c(40)])
# apical pouch
freq(hcm[,c(41)])

# ECG
  # Time OHCA to ECG
  hist(hcm[,c(44)])
  summary(hcm[,c(88)])/365
  # Normal ECG
  freq(hcm[,c(89)])
  # Afib/flutter
  freq(hcm[,c(90)])
  # Ventricular paced ECG
  freq(hcm[,c(94)])
  # LBBB
  freq(hcm[,c(95)])
  # RBBB
  freq(hcm[,c(96)])
  # T wave inversion
  freq(hcm[,c(97)])
  # LVH
  freq(hcm[,c(98)])
  # Q waves
  freq(hcm[,c(99)])
  # Abnormal re-polarization
  freq(hcm[,c(100)])
  # AI score
  summary(hcm[,c(102)])
  sort(as.numeric(hcm[,c(102)]), decreasing = TRUE)

# ICD implanted
  freq(hcm[,c(49)])
# Days OHCA to ICD
  hist(hcm[,c(51)])
  summary(hcm[,c(51)])
  shapiro.test(hcm[,c(51)])
# ICD therapy
  freq(hcm[,c(54)])
# ICD Shock
  freq(hcm[,c(55)]) 
# ATP
  freq(hcm[,c(56)])
# Time to ICD therapy
  summary(hcm[,c(58)])/365
  hist(hcm[,c(58)])
  shapiro.test(hcm[,c(58)])
# Myectomy
  freq(hcm[,c(67)])
# days to myectomy
  hist(hcm[,c(68)])
  shapiro.test(hcm[,c(68)])
  summary(hcm[,c(68)])/365
# death
  freq(hcm[,c(64)])

# Inappropriate shocks
  freq(hcm[,c(73)])
  
# Time to recurrent VT/VF
  summary(hcm[,c(63)])/365
  hist(hcm[,c(63)])
  shapiro.test(hcm[,c(63)])

# Time to last f/u
  hist(hcm[,c(69)])
  shapiro.test(hcm[,c(69)])
  mean(hcm[,c(69)]) / 365
  sd(hcm[,c(69)]) / 365
  summary(hcm[,c(69)]/365)
        
# Risk factors
  # Family history
  freq(hcm[,c(79)])
  # Massive LVH
  freq(hcm[,c(80)])
  # Unexplained syncope
  freq(hcm[,c(81)])
  # Apical aneurysm
  freq(hcm[,c(82)])
  # EF < 50%
  freq(hcm[,c(83)])
  # NSVT on ICD or holter <1 year
  freq(hcm[,c(84)])

totalrf <- hcm[,c(79)] + hcm[,c(80)] + hcm[,c(81)] + hcm[,c(82)] + hcm[,c(83)] + hcm[,c(84)] + hcm[,c(85)]
totalrf
freq(totalrf)


barplot(totalrf,
#main = "Total Number of Risk Factors for SCD",
xlab = "Number of Risk Factors",
freq = TRUE,
breaks = 5
)

x11()

ggplot(data = hcm, aes(x=Total.RFs))  +
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  labs(x = "Risk Factors", y = "Frequency") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25),
    axis.title.x = element_text(size=18),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 18)
    )

# Test
ggplot(data = hcm, aes(x=Total.RFs, fill = Age.at.OHCA > 18))  +
  geom_histogram(binwidth = 1) +
  labs(x = "Risk Factors", y = "Frequency", title = "Frequency of SCD Risk Factors") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25),
    axis.title.x = element_text(size=18),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 18)
  )

# Test
ggplot(data = hcm, aes(x=Total.RFs, fill = hcm$obstructive.vs.non.obstructive))  +
  geom_histogram(binwidth = 1) +
  labs(x = "Risk Factors", y = "Frequency", title = "Frequency of SCD Risk Factors") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25),
    axis.title.x = element_text(size=18),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 18)
  )

# Figures for AI-ECG score
boxplot(hcm[,c(102)])

x11()
ggplot(data = hcm, aes(x=AI.score)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 4, fill = "gray", color = 'black') + 
  geom_dotplot(stackdir = "center") +
  coord_flip() +
  theme_classic() +
  labs(x = "Probability of HCM") +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )
  



#####
# Survival stuff
library(survival)
library(ggplot2)
library(ggpubr)
library(survminer)
library(dplyr)

# time to VT/VF recurrence treated or untreated
endpoint <- hcm[,c(70)]
endpoint
km.model <- survfit(Surv(time = hcm[,c(72)]/365, event = endpoint) ~ 1, data = hcm)
km.model
summary(km.model)
ggsurvplot(km.model,
           data = hcm,
           conf.int = FALSE,
           #title = "Survival from recurrent VT/VF",
           xlab = "Time to VT/VF recurrence (years)",
           ylab = "Survival",
           font.main = 24,
           font.y = 18,
           font.x = 18,
           surv.plot.height = 0.85,
           risk.table.height = 0.15,
           surv.scale = "percent",
           palette = "black",
           censor = TRUE,
           risk.table = TRUE,
           legend.title = "",
           risk.table.title = "Number at risk"
          
           )

# Split by age - not interesting
hcm[,c(4)]
age20 <- as.numeric(hcm[,c(4)] <= 20)
age20
age25 <- as.numeric(hcm[,c(4)] <= 25)
age25
age30 <- as.numeric(hcm[,c(4)] <= 30)
age30
age40 <- as.numeric(hcm[,c(4)] <= 40)
age40

km.model <- survfit(Surv(time = hcm[,c(73)], event = endpoint) ~ age25, data = hcm)  
ggsurvplot(km.model,
           data = hcm,
           conf.int = FALSE,
           title = "Time to recurrent VT/VF",
           censor = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           pval.method = TRUE
           )

# Split by obstructive - not interesting
obs <- hcm[,c(13)]
obs
km.model <- survfit(Surv(time = hcm[,c(73)], event = endpoint) ~ obs, data = hcm)  
ggsurvplot(km.model,
           data = hcm,
           conf.int = FALSE,
           title = "Time to recurrent VT/VF",
           censor = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           pval.method = TRUE
)

# split by comorbidities - not interesting
comorbid <- hcm[,c(22)]
comorbid
km.model <- survfit(Surv(time = hcm[,c(73)], event = endpoint) ~ comorbid, data = hcm)  
ggsurvplot(km.model,
           data = hcm,
           conf.int = FALSE,
           title = "Time to recurrent VT/VF",
           censor = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           pval.method = TRUE
)

# split by pathogenic mutation - not interesting
mutation <- hcm[,c(21)]
mutation
mutation[is.na(mutation)] = "0"
mutation <- as.numeric(gsub(3, "0", mutation))
mutation    

km.model <- survfit(Surv(time = hcm[,c(73)], event = endpoint) ~ mutation, data = hcm)  
ggsurvplot(km.model,
           data = hcm,
           conf.int = FALSE,
           title = "Time to recurrent VT/VF",
           censor = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           pval.method = TRUE
)


# person-year incidence
27 / sum(hcm[,c(69)]) / 365 * 1000 / 6267

#####
maron <- read.csv("Maronadultadolescents.csv", na.strings = c("", "NA"))
View(maron)

m <- maron[-c(1:3, 16:17, 27, 39, 71, 72, 90, 91, 95, 98),]
# age
mean(m[,c(4)])
sd(m[,c(4)])
# sex
freq(m[,c(3)])
# age at event
# NYHA start
# NYHA end
# LVOT
freq(m[,c(7)] > 30)
# max thickness
mean(m[,c(8)])
sd(m[,c(8)])
# ef
mean(m[,c(11)])
sd(m[,c(11)])

survive <- maron[c(18:26,28:38,92:94,96:97),]
View(survive)

# age
  mean(survive[,c(4)])
  sd(survive[,c(4)])
# sex
  freq(survive[,c(3)])
# age at event
# NYHA start
# NYHA end
# LVOT
  freq(survive[,c(7)] > 30)
# max thickness
  mean(survive[,c(8)])
  sd(survive[,c(8)])
# ef
  mean(survive[,c(11)])
  sd(survive[,c(11)])  
  