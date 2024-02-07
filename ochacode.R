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
library(lubridate)
library(reshape2)
library(ggdist)
library(ggthemes)
library(tidyquant)
library(lubridate)
library(RColorBrewer)
library(scales)




# Age
hist(hcm[,c(4)])
mean(hcm[,c(4)])
sd(hcm[,c(4)])
summary(hcm[,c(4)])
freq(hcm[,c(4)])
hist(hcm[,c(4)])

sum(hcm[,4] < 30)


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
  summary(hcm[,c(68)])/30
  summary(hcm[,c(68)])/365.25
# death
  freq(hcm[,c(64)])
  # Time to death - Not done in excel already
  deathdate <- hcm[c(18,25),64]
  deathdate <- as.Date(deathdate, format = "%m/%d/%Y" )
  deathdate
  
  ohcadate<- hcm[c(18, 25),8]
  ohcadate <- as.Date(ohcadate, format = "%m/%d/%Y")
  ohcadate

  datediff <- (deathdate - ohcadate) / 365.25
  datediff
  
  
# Inappropriate shocks
  freq(hcm[,c(73)])
  
# Time to recurrent VT/VF
  summary(hcm[,c(63)])/365
  hist(hcm[,c(63)])
  shapiro.test(hcm[,c(63)])
  
# Time OHCA to death
library(lubridate)
(hcm[,64]) - as.date(hcm[,8])
hcm[,8]
a <- mdy(hcm[,8])
a
b <- mdy(hcm[,64])
b  
(b - a) / 365.25
  
# Time to last f/u
  hist(hcm[,c(69)])
  shapiro.test(hcm[,c(69)])
  mean(hcm[,c(69)]) / 365
  sd(hcm[,c(69)]) / 365
  summary(hcm[,c(69)]/365.25)

# VT during F/u
freq(hcm[,143])
# VF during f/u
freq(hcm[,144])
# VT or VF during F/u
freq(hcm[,145])
# Both VT and VF during f/u
hcm[,143] == 1 & hcm[,144] == 1

freq(hcm[,54])

# Myectomy
freq(hcm[,146])
 
# VT/VF before myectomy
freq(hcm[,147])

# VT/VF after myectomy
freq(hcm[,148])

# Beta blocker
  freq(hcm[,152])
# Antiarrhythmic
  freq(hcm[,153])  
  
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
  # LGE on cmr
  freq(hcm[,85])
  

sum(hcm[,79])
sum(hcm[,80])
sum(hcm[,81])
sum(hcm[,82])
sum(hcm[,83])
sum(hcm[,84])

rf <- matrix(data = hcm[,79:85])
View(rf)


totalrf <- hcm[,c(79)] + hcm[,c(80)] + hcm[,c(81)] + hcm[,c(82)] + hcm[,c(83)] + hcm[,c(84)] + hcm[,c(85)]
totalrf
freq(totalrf)
summary(totalrf)
View(totalrf)

modrf <- hcm[,c(79)] + hcm[,c(80)] + hcm[,c(81)] + hcm[,c(82)] + hcm[,c(83)]
modrf
summary(modrf)

#exclude nsvt
nonsvt <- hcm[,79:83]
j <- 0
for (i in 1:27) {
  if (nonsvt[i,1] | nonsvt[i,2] | nonsvt[i,3] | nonsvt[i,4] | nonsvt[i,5] > 0) {
    j = j + 1
    print(j)
  }
}

# AI ECG score
summary(hcm[,102])
hcm[,102]


# AI ECG score for those within 6 months
sixmo <- subset(x = hcm, subset = hcm$ECG..6mo.SCA. == 1)
sixmo
summary(sixmo[,102])
sixmo[,102]

barplot(totalrf,
#main = "Total Number of Risk Factors for SCD",
xlab = "Number of Risk Factors",
freq = TRUE,
breaks = 5
)

# Circumstances during SCA
freq(hcm[,133])

# Symptoms attributable to SCA
  # dyspnea
  freq(hcm[,135])
  # chest discomfort
  freq(hcm[,136])
  # syncope
  freq(hcm[,137])
  # presyncope
  freq(hcm[,138])
  # palpitations
  freq(hcm[,139])
hcmsx <- hcm[,c(135)] + hcm[,c(136)] + hcm[,c(137)] + hcm[,c(138)] + hcm[,c(139)]
hcmsx
summary(hcmsx)

# 5 year SCD risk score
summary(hcm[,157])
hcm[,157] <4
hcm[,157] >6

hcm[,157]

# LA size
hcm[,160]
summary(hcm[,160])

# Incidence calculation
events <- 42
firsttimevents <- 15
hcm[,69]
timetofu <- hcm[,69]/365.25
timetofu
summary(timetofu)
personyears <- sum(timetofu)
personyears
personyears / 27

events / personyears * 1000
firsttimevents / personyears * 1000

# Figures
#####
### Figure 1 ### - AI ECG score
dotcolor <- hcm$ECG..6mo.SCA.
dotcolor
x11()
ggplot(data = hcm, aes(x=AI.score, fill = as.factor(ECG..6mo.SCA.))) +
  geom_boxplot(outlier.shape = 16, outlier.size = 4, fill = "gray90", color = "black") + 
  geom_dotplot(stackdir = "center", binwidth = 3) +
  theme_classic() +
  coord_flip() +
  labs(x = "AI-ECG Probability of HCM") +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     labels = paste0(seq(0, 100, 25), "%")) +
  geom_vline(xintercept = 11, color = "black", linetype = "dashed", linewidth = 1) +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(title = "ECG <6 months after SCA"), label = TRUE) +
  scale_fill_discrete(labels=c('No', 'Yes'), type = c("black", "white"))

### Figure 2 ###
x11()
totalrf <- hcm[,c(79)] + hcm[,c(80)] + hcm[,c(81)] + hcm[,c(82)] + hcm[,c(83)] + hcm[,c(84)] + hcm[,c(85)]
totalrf

### Figure 2a ###
p1 <- ggplot(data = hcm, aes(x=Total.RFs))  +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  theme_classic() +
  labs(x = "Number of SCD Risk Factors", y = "Number of patients") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme(
    axis.title.x = element_text(size = 18,vjust = 0),
    axis.text = element_text(size = 14),
    axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
    axis.title.y = element_text(size = 18)
    )

# Figure 2b
realrf <- hcm[,c(79)] + hcm[,c(80)] + hcm[,c(81)] + hcm[,c(82)] + hcm[,c(83)]
realrf

c <- hcm[,79:83] 
View(c)
d <-cbind(c, realrf)
View(d)

p2 <- ggplot(data = d, aes(realrf))  +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  theme_classic() +
  labs(x = "Number of SCD Risk Factors", y = "Number of patients") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14)) +
  theme(
    axis.title.x = element_text(size = 18,vjust = 0),
    axis.text = element_text(size = 14),
    axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
    axis.title.y = element_text(size = 18)
  )

x11()
ggpubr::ggarrange(p1, p2, nrow = 1,labels = c("", ""))

## *** Figure 3 ***
# Old figure - without blending of colors
x11()
ggplot(data = hcm, aes(x=SCD.HCM.score)) +
  geom_rect(data = NULL, aes(xmin = -Inf, xmax = 4, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.2) +
  geom_rect(data = NULL, aes(xmin = 4, xmax = 6, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.2) +
  geom_rect(data = NULL, aes(xmin = 6, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_boxplot(outlier.shape = 16, outlier.size = 4, fill = "transparent", color = 'black', lwd = 1.1) + 
  geom_dotplot(stackdir = "center",binwidth = 0.6) +
  coord_flip() +
  theme_classic() +
  scale_x_continuous(limits = c(0, 25), breaks = c(0, 5, 10, 15, 20, 25),
                     labels = paste0(seq(0, 25, 5), "%")) +
  labs(x = "HCM-SCD 5 year risk") +
  geom_vline(xintercept = 4, color = "black", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 6, color = "black", linetype = "dashed", linewidth = 1) +
  theme(
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

# Figure 3 in paper
# Blended colors background
# Making colorscales
colorscale1 <- colorRampPalette(c("green", "yellow"), bias = 3.36)
show_col(colorscale1(1000), labels = FALSE)
colorscale2 <- colorRampPalette(c("yellow", "red"), bias = 0.152)
show_col(colorscale3(1000), labels = FALSE)

nicecolors <- data.frame(colorchange = c(1:1000),
                         static = rep(c(0)),
                         nc = 0
                         )

# sharpening up borders to make it less hazy - selecting less boxes on a shorter color palette and more from solids
for (i in 1:1000) {
  if (i < 121){
    nicecolors[i,3] <- "green"
  } else if (i < 161) {
    nicecolors[i,3] <- colorscale1(40)[i-120]
  } else if (i < 241) {
    nicecolors[i,3] <- "yellow"
  } else if (i < 281) {
    nicecolors[i,3] <- colorscale2(40)[i-240]
  } else if (i < 1001){
    nicecolors[i,3] <- "red"
  }
}
x11()
ggplot() +
  geom_tile(data = nicecolors, aes(x = colorchange/40, y = static), fill = nicecolors$nc, alpha = 0.5) +
  geom_boxplot(data = hcm, aes(x = SCD.HCM.score),outlier.shape = 16, outlier.size = 4, fill = "transparent", color = 'black', lwd = 1.1) +
  geom_dotplot(data = hcm, aes(x = SCD.HCM.score),stackdir = "center",binwidth = 0.6) +
  theme_classic() +
  coord_flip() +
  scale_x_continuous(limits = c(0, 25), breaks = c(0, 5, 10, 15, 20, 25),
                     labels = paste0(seq(0, 25, 5), "%")) +
  labs(x = "HCM-SCD 5 year risk") +
  geom_vline(xintercept = 4, color = "black", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 6, color = "black", linetype = "dashed", linewidth = 1) +
  theme(
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )

### Figure 4 ###
endpoint <- hcm[,c(70)]
endpoint
km.model <- survfit(Surv(time = hcm[,c(72)]/365.25, event = endpoint) ~ 1, data = hcm)
km.model
summary(km.model)
p1 <- ggsurvplot(km.model,
           data = hcm,
           conf.int = FALSE,
           color = c("red"),
           #title = "Survival from recurrent VT/VF",
           xlab = "Time to VT/VF recurrence (years)",
           ylab = "Survival free of VT/VF",
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
p1$plot <- p1$plot + ggplot2::annotate("text", 
                                       x = 8, y = 0.9, # x and y coordinates of the text
                                       label = "Annual incidence = 15%", size = 6)

p1
# Panel for overall survival - last f/u date 69, death is 64
death <- hcm[,c(69)]
death
km.model.2 <- survfit(Surv(time = hcm[,c(69)]/365.25, event = !is.na(hcm[,64])) ~ 1, data = hcm)
km.model.2

p2 <- ggsurvplot(km.model.2,
           data = hcm,
           conf.int = FALSE,
           color = c("red"),
           xlab = "Time to death (years)",
           ylab = "Survival free of all-cause death",
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
p2$plot <- p2$plot + ggplot2::annotate("text", 
                                       x = 15, y = 0.5, # x and y coordinates of the text
                                       label = "Annual incidence = 0.7%", size = 6)
p2

arrange_ggsurvplots(x = list(p1,p2))

ggarrange(p1, p2, labels = "auto")

# Possible appendix
test <- 100* hcm[,171:195]
test <- cbind(rownames(test), test)
colnames(test)[1] <- "Subject"
View(test)

a <- melt(test, id.vars = "Subject" )
View(a)
b <- a[1:27,]
View(b)

# P2 - every AIHCM risk score months 1-12
x11()
ggplot(data = a, aes(x = Subject, y = value)) +
  geom_point(size = 4) +
  #geom_point(data = b, aes(x = Subject, y = value), color = "gray80", size = 4) +
  geom_hline(yintercept = 11, size = 1, color = "black", linetype = "dashed") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = paste0(seq(0, 100, 10), "%")) +
  labs(y = "AI-ECG Score", x = "Patient") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14))
        

-# person-year incidence
27 / sum(hcm[,c(69)]) / 365 * 1000 / 6267
library(incidence)


datetable <- cbind(hcm[,8], hcm[,9], hcm[,197:212], hcm[,52])
View(datetable)
str(datetable)

dates <- matrix(0,27,17)

for (i in 1:27) {
  for (j in 1:17) {
    if(is.na(datetable[i,j])) {
      dates[i,j] <- 
    } else {
      dates[i,j] <- as.Date(datetable[i,j], format = "%m/%d/%Y")
    }
  }
}

View(dates)



as.Date(dates[1,1], format = "%m/%d/%Y")


dates <- as.Date(dates,format = "%m/%d/%Y")




incidence(dates = dates[1,],interval = 365)



# Log rank for male vs. female
endpoint <- hcm[,c(70)]
endpoint
malefem <- hcm[,c(5)]
km.model <- survfit(Surv(time = hcm[,c(72)]/365.25, event = endpoint) ~ malefem, data = hcm)
km.model
summary(km.model)
      ggsurvplot(km.model,
                 conf.int = FALSE,
                 palette = c("black", "gray"),
                 xlab = "Time to VT/VF recurrence (years)",
                 ylab = "Survival free of VT/VF",
                 font.main = 24,
                 font.y = 18,
                 font.x = 18,
                 surv.plot.height = 0.85,
                 risk.table.height = 0.15,
                 surv.scale = "percent",
                 censor = TRUE,
                 risk.table = TRUE,
                 legend.title = "",
                 risk.table.title = "Number at risk",
                 pval = TRUE,
                 pval.method = TRUE
              
                )

mfincidence <- cbind(hcm[,5], hcm[,8], hcm[,9], hcm[,52], hcm[,69], hcm[,197:212])
colnames(mfincidence) <- c("sex", "ohca date", "mayo eval", "last follow up", "f/u time", "VT1", "VT2", "VT3", "VT4", "VT5", 
                           "VF1", "VF2", "VF3", "VF4", "VF5", "VF6", "VF7", "VF8", "VF9", "VF10", "VF11")     
View(mfincidence)

male <- subset(x = mfincidence,subset = mfincidence$sex == "M")
View(male)

female <- subset(x = mfincidence,subset = mfincidence$sex == "F")      
View(female)

# t test
malet <- c(1, 0, 1, 2, 1, 12, 1, 4, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 1)
femalet <- c(5, 1, 0, 5, 1, 4, 0, 0)

t.test(femalet, malet)
wilcox.test(malet, femalet, paired = F)

?t.test

# Annual incidence male
      events <- 26
      firsttimevents <- 10
      timetofu <- male[,5]/365.25
      timetofu
      summary(timetofu)
      personyears <- sum(timetofu)
      personyears
      personyears / 19
      
      events / personyears * 1000
      firsttimevents / personyears * 1000
      
# Annual incidence female
      events <- 16
      firsttimevents <- 5
      timetofu <- female[,5]/365.25
      timetofu
      summary(timetofu)
      personyears <- sum(timetofu)
      personyears
      personyears / 8
      
      events / personyears * 1000
      firsttimevents / personyears * 1000 

      
# Num VT and VF recur
recur <- hcm[,197:212]
View(recur)

# Delayed diagnosis
dxdate <- hcm[,213]
arrestdate <- hcm[,8]

dxdifference <- (mdy(dxdate) - mdy(arrestdate) ) / 365.25
dxdifference
summary(as.numeric(dxdifference))


# Figure for central illustration
endpoint <- hcm[,c(70)]
endpoint
km.model <- survfit(Surv(time = hcm[,c(72)]/365.25, event = endpoint) ~ 1, data = hcm)
km.model
summary(km.model)
x11()
ggsurvplot(km.model,
                 data = hcm,
                 conf.int = FALSE,
                 xlab = "Time to VT/VF recurrence (years)",
                 ylab = "Survival free of VT/VF",
                 font.main = 24,
                 color = c("red"),
                 font.y = c(24, "bold"),
                 font.x = c(24, "bold"),
                 font.tickslab = c(20),
                 surv.scale = "percent",
                 palette = "black",
                 censor = FALSE,
                 risk.table = FALSE,
                 legend.title = ""
)

# Panel for overall survival - last f/u date 69, death is 64
death <- hcm[,c(69)]
death
km.model.2 <- survfit(Surv(time = hcm[,c(69)]/365.25, event = !is.na(hcm[,64])) ~ 1, data = hcm)
km.model.2

ggsurvplot(km.model.2,
                 data = hcm,
                 conf.int = FALSE,
                 xlab = "Time to death (years)",
                 ylab = "Survival free of all-cause death",
                 font.main = 24,
                 color = c("red"),
                 font.y = c(24, "bold"),
                 font.x = c(24, "bold"),
                 font.tickslab = c(20),
                 surv.scale = "percent",
                 palette = "black",
                 censor = FALSE,
                 risk.table = FALSE,
                 legend.title = "",
)
