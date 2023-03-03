#
# Usage:
# 
# Rscript papaver.R <input_dir> <output_dir>
#
# <input_dir>: Has to contain Measures.xlsx, Soil_humidity.xlsx and COX.xlsx
# <output_dir>: The analysis plots will be stored in this directory (has to exist)

## Load Packages
#install.packages("")
library(DataExplorer) # to explore the data
library(lme4) # for LMER
library(car) # for Anova
library(lsmeans) # for posthocs (alternative emmeans)
library(ggplot2) # for ggplot2
library(Rmisc) # for summarySE
library(dplyr)
library(ggpubr) # for ggarrange
library(readxl)
library(survival)
library(survminer)

args = commandArgs(trailingOnly=TRUE)

wd <- "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment"
#wd <- args[1]
outdir <- "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics"
#outdir <- args[2]


setwd(wd)
data <-read_excel("Measures.xlsx", col_types = c("text", 
                                                 "text", "text", "text", "numeric","numeric",   "numeric", "text", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",   "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
Soil_humidity <- read_excel("Soil_humidity.xlsx")
COX <- read_excel("COX.xlsx")
#---------------------------------------------------------------------------
############################################################################
#                         Soil humidity
############################################################################
## First we checked if the drought treatments worked well

# preparing data
Soil_humidity$drought_treatment <- as.factor(Soil_humidity$drought_treatment)
Soil_humidity$prec_pred <- as.factor(Soil_humidity$prec_pred)
Soil_humidity$competition <- as.factor(Soil_humidity$competition)
Soil_humidity$date <- as.factor(Soil_humidity$date)
Soil_humidity$humidity <- as.numeric(Soil_humidity$humidity)
str(Soil_humidity)

aggregate(humidity~drought_treatment + date,data=Soil_humidity,mean)

summaryHUM <- summarySE(Soil_humidity, measurevar="humidity", groupvars=c("date","drought_treatment"))
summaryHUM

# Graph
a <- ggplot(summaryHUM, aes(x=date, y=humidity, group=drought_treatment, colour=drought_treatment)) + 
  geom_line() +
  geom_point() +
  ylim(c(0,60)) +
  scale_color_manual(values = c("green","blue","gold"), name = "Drought treatment", labels = c("Control", "Short drought", "Long drought")) +
  xlab("Days of treatment") + ylab("Soil humidity (%)") +
  theme_bw() +
  ggtitle("Soil humidity during the drought treatment") +
  labs(fill = "Drought treatment") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  geom_errorbar(aes(ymin=humidity-se,ymax=humidity+se), position = position_dodge(width=0), width=.2, size=0.5, color="black")
a # looks as the drought treatment worked really well


# Creating model and checking model assumptions
model <- lm(humidity ~ drought_treatment * competition * date, data=Soil_humidity)

shapiro.test(resid(model))     # p-value = 0.07 > 0.05 => normality OK
hist(resid(model),breaks=50)   # no transformation needed
qqnorm(resid(model)) 
qqline(resid(model)) 

plot(resid(model) ~ fitted(model)) # Test for homoscedasticity
bartlett.test((resid(model))~interaction(Soil_humidity$drought_treatment,Soil_humidity$competition,Soil_humidity$date), data=Soil_humidity)

Anova(model,type=2)
summary(model)

lsmeans(model,pairwise~drought_treatment:date, adjust="tukey") # pairwise comparison


############################################################################
#                         Survival/wilting point
############################################################################
# Here we looked at the wilting point of the plants and the proportion

# preparing data
COX$competition <- as.factor(COX$competition)
COX$precip_pred <- as.factor(COX$precip_pred)
COX$family <- as.factor(COX$family)
COX$Group <- as.factor(COX$Group)
COX$status <- as.numeric(COX$status)
COX$time <- as.numeric(COX$time)
COX$nb_leavesi <- as.numeric(COX$nb_leavesi)
COX$time = COX$time + 15 
str(COX)

# Plot: Non-wilting proportion = f(days)
ggsurvplot(survfit(Surv(time, status) ~ Group + precip_pred + competition, data=COX),
           conf.int = ,
           xlim = c(13,26),
           break.x.by = 2,
           legend = c("right"),
           axes.offset = F,
           palette = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "lightblue", "lightblue", "blue", "blue", "yellow3", "yellow3", "darkorange", "darkorange"),
           linetype=c("solid","dashed","solid","dashed","solid","dashed","solid","dashed","solid","dashed","solid","dashed"),
           legend.labs= c("Control More C-", "Control More C+", "Control Less C-", "Control Less C+", "Short drought More C-", "Short drought More C+", "Short drought Less C-", "Short drought Less C+","Long drought More C-", "Long drought More C+", "Long drought Less C-", "Long drought Less C+"),
           legend.title = " Treatment combination",
           title = "",
           xlab = "Days",
           ylab = "Proportion of non-wilted plants",
           font.title = "bold",
           ggtheme = theme_classic())

# Linear mixed model

aggregate(time~ Group + precip_pred + competition, data=COX,mean)

mod <- lmer(time ~ nb_leavesi  + precip_pred * competition * Group + (1|family), data=COX)
Anova(mod, type = 2) 

shapiro.test(resid(mod))     # p-value = 2.979e-06
hist(resid(mod),breaks=50)   
qqnorm(resid(mod)) 
qqline(resid(mod))     

#Model transformation
mod <- lmer((time)^3.5 ~ nb_leavesi  + precip_pred * competition * Group + (1|family), data=COX)
shapiro.test(resid(mod))     # p-value = 1.625e-06 => Normality OK
hist(resid(mod),breaks=50)   
qqnorm(resid(mod)) 
qqline(resid(mod))    

plot(resid(mod) ~ fitted(mod)) #Homoscedasticity

#Anova
Anova(mod, type = 2) 
lsmeans(mod,pairwise ~ precip_pred:competition:Group, adjust="tukey")

# function for labeling the plots
names <- list("1control" = "Control","2short_drought" = "Short drought","3long_drought" = "Long drought")
labeller <- function(variable,value){
  return(names[value])
}

COX1 <- COX
COX1 <- COX1[complete.cases(COX1[,18]),]
levels(COX1$Group) <- c("control","short_drought", "long_drought" )
levels(COX1$precip_pred) <- c("More", "Less")
levels(COX1$competition) <- c("0","1")

COX1 <- summarySE(COX1, measurevar="time", groupvars=c("Group", "precip_pred", "competition"))
COX1

COX2 <- COX
COX2 <- COX2[complete.cases(COX2[,18]),]
levels(COX2$Group) <- c("control","short_drought", "long_drought" )
COX2 <- summarySE(COX2, measurevar="time", groupvars=c( "precip_pred"))
COX2

t.test(time ~ precip_pred, COX) # compare M and L

SubWilting1 <- subset(COX, Group == "1_control") # subset
SubWilting2 <- subset(COX, Group == "2_short_drought") # subset
SubWilting3 <- subset(COX, Group == "3_long_drought") # subset
t.test(time ~ precip_pred, SubWilting1) # compare M and L 100% 
t.test(time ~ precip_pred, SubWilting2) # compare M and L
t.test(time ~ precip_pred, SubWilting3) # compare M and L

ggplot(COX1, aes(x=competition, y=time, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("") + ylab("Day") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=time-se,ymax=time+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + 
  theme(legend.position="top") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) # as point plot

ggplot(COX1, aes(x=competition, y=time, fill = precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("Competition") + ylab("Number of days") + theme(text = element_text(size=18)) + geom_bar(stat = "identity", position = "dodge") + ylim(c(0,29))  +
  geom_errorbar(aes(ymin=time-se,ymax=time+se), position = position_dodge(width=1), width=0.4, size=0.5, color="black")+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability")  +
  theme(legend.position="top")  +
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "without", "with")) # as TNT or bar plot

############################################################################
#                         Plant traits
############################################################################
# Here we are looking at the 6 traits we were measuring:
# - Dry aboveground biomass
# - Dry belowground biomass
# - Root shoot ratio
# - Root length
# - Number of leaves
# - length of longest leaf
# but before we checked if there is an edge effect for the pots with competition 
# (at a point of time it was not possible to randomize them because Galium had 
# grown over several pots and the pots could no longer be replaced without damaging 
# the plants.)

#   checking and calculate root shoot ratio
str(data)

data$competition <- as.factor(data$competition)
data$precip_pred <- as.factor(data$precip_pred)
data$family <- as.factor(data$family)
data$Group <- as.factor(data$Group)
data$edge_effect <- as.factor(data$edge_effect)

data$RSratio <- data$bground_biomass/data$abground_biomass # calculate root shoot ratio

# Sub-setting drought and competition conditions
Sub1 <- subset(data, Group == "1control") # subset
Sub2 <- subset(data, Group == "2short_drought") # subset
Sub3 <- subset(data, Group == "3long_drought") # subset
SubPlus <- subset(data, competition == "1") # subset
SubMinus <- subset(data, competition == "0") # subset  


#---------------------------------------------------------------------------
############################################################################
# Testing whether there is an edge effect 
#     All 6 competition trays

Subcompetition <- subset(data, competition == "1") # subset
Subcompetition <- subset(Subcompetition, !is.na(Subcompetition[,31])) # removing died ones (NA everywhere)

str(Subcompetition)
# abovegrounf biomass
t.test( x = Subcompetition$abground_biomass[Subcompetition$edge_effect == '1'], y = Subcompetition$abground_biomass[Subcompetition$edge_effect == '0'])
#     Long drought M
Sub_Comp_Long <- subset(Subcompetition, Group == "3long_drought") # subset
Sub_Comp_Long_M <- subset(Sub_Comp_Long, precip_pred == "1M")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Long_M)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Long_M)
t.test(root_length ~ edge_effect, Sub_Comp_Long_M)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Long_M)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Long_M)
#     Long drought L
Sub_Comp_Long_L <- subset(Sub_Comp_Long, precip_pred == "2L")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Long_L)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Long_L)
t.test(root_length ~ edge_effect, Sub_Comp_Long_L)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Long_L)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Long_L)
#     Short drought M
Sub_Comp_Short <- subset(Subcompetition, Group == "2short_drought") # subset
Sub_Comp_Short_M <- subset(Sub_Comp_Short, precip_pred == "1M")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Short_M)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Short_M)
t.test(root_length ~ edge_effect, Sub_Comp_Short_M)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Short_M)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Short_M)
#     Short drought L
Sub_Comp_Short_L <- subset(Sub_Comp_Short, precip_pred == "2L")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Short_L)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Short_L)
t.test(root_length ~ edge_effect, Sub_Comp_Short_L)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Short_L)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Short_L)
#     Control M
Sub_Comp_Con <- subset(Subcompetition, Group == "1control") # subset
Sub_Comp_Con_M <- subset(Sub_Comp_Con, precip_pred == "1M")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Con_M)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Con_M)
t.test(root_length ~ edge_effect, Sub_Comp_Con_M)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Con_M)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Con_M)
#     Control L
Sub_Comp_Con_L <- subset(Sub_Comp_Con, precip_pred == "2L")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Con_L)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Con_L)
t.test(root_length ~ edge_effect, Sub_Comp_Con_L)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Con_L)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Con_L)
# checking for outliers --> by removing outlier, no significant effect
boxplot(Sub_Comp_Con_L$nb_leavesf)
NoOut_Sub_Comp_Con_L<-Sub_Comp_Con_L[!Sub_Comp_Con_L$nb_leavesf>16,] 
t.test(nb_leavesf ~ edge_effect, NoOut_Sub_Comp_Con_L)

# Overall, there is no edge effect (--> not needed as random factor)
#---------------------------------------------------------------------------
############################################################################
#              aboveground biomass
############################################################################

    # 1: Exploring data
data1<-data[complete.cases(data[,32]),] # to use only complete cases
boxplot(data1$abground_biomass)
boxplot.stats(data1$abground_biomass)$out # 

#-----------------------------------------------
    # 2: Model building
mod_abground_biomass <- lmer(abground_biomass ~ nb_leavesi  + precip_pred * competition * Group
               + (1|family), data=data1)
Anova(mod_abground_biomass, type = 2) # aboveground biomass
# competition, Group, competition:Group // precip_pred:Group .

#-------------------------------------------------
    # 3: Checking Assumptions
## Test NORMALITY of residuals
shapiro.test(resid(mod_abground_biomass))     # p-value =   2.905e-10
hist(resid(mod_abground_biomass),breaks=50)   # no Gaussian distribution --> Kurtotis (see next 2 commands)
qqnorm(resid(mod_abground_biomass)) 
qqline(resid(mod_abground_biomass))           

# NEW model
mod_abground_biomass_NEW <- lmer((abground_biomass)^0.55 ~ nb_leavesi  + precip_pred * competition * Group
                                 + (1|family), data=data1) 

shapiro.test(resid(mod_abground_biomass_NEW))   # p =  1.083e-05 much better
hist(resid(mod_abground_biomass_NEW),breaks=50) 
qqnorm(resid(mod_abground_biomass_NEW))
qqline(resid(mod_abground_biomass_NEW)) 

## HETEROCEDASTICITY
plot(resid(mod_abground_biomass_NEW) ~ fitted(mod_abground_biomass_NEW)) # it seems that there is some heteroscedasticity
# use the Bartlett test
bartlett.test((resid(mod_abground_biomass_NEW))~interaction(data1$Group, data1$precip_pred, data1$competition), data=data1)
#CONCLUSION: p should be > 0.05 to be a Homocedastic model

# --------------------------------------------------------------------------
      # 4: ANOVA 
Anova(mod_abground_biomass_NEW,type=2) # type = 2 represents the error type 2
# competition, Group, competition:Group, precip_pred:Group
summary(mod_abground_biomass_NEW)

# ---------------------------------------------------------------------------
      # 5: Post-hoc 

# A Post-hoc test is only needed if there is significant interactions or significant factors with 3 or more levels
# competition:Group
lsmeans(mod_abground_biomass_NEW,pairwise ~ competition:Group, adjust="tukey")
a<-lsmeans(mod_abground_biomass_NEW, ~ competition:Group) 
plot(a)
#precip_pred:Group
lsmeans(mod_abground_biomass_NEW,pairwise ~ precip_pred:Group, adjust="tukey")

# precip_pred:competition:Group for Whole Complete Graphic
lsmeans(mod_abground_biomass_NEW,pairwise ~ precip_pred:competition:Group, adjust="tukey")

# ---------------------------------------------------------------------------
#  main precipitation predictability effect 
t.test(abground_biomass ~ precip_pred, data) # compare M and L

t.test(abground_biomass ~ precip_pred, Sub1) # compare M and L  
t.test(abground_biomass ~ precip_pred, Sub2) # compare M and L
t.test(abground_biomass ~ precip_pred, Sub3) # compare M and L
t.test(abground_biomass ~ precip_pred, SubPlus) # compare M and L
t.test(abground_biomass ~ precip_pred, SubMinus) # compare M and L

# ---------------------------------------------------------------------------
      # 6: Graphics 
# always in alphabetic order
summary_Aboveground_TEST <- data1
levels(summary_Aboveground_TEST$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_Aboveground_TEST$precip_pred) <- c("More", "Less")
summary_Aboveground <- summarySE(summary_Aboveground_TEST, measurevar="abground_biomass", groupvars=c("Group", "precip_pred"))
summary_Aboveground

dot_aboveground <- ggplot(summary_Aboveground, aes(x=Group, y=abground_biomass, fill=precip_pred)) +
  theme_classic() +
  xlab("") + ylab("Dry aboveground biomass (g)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=abground_biomass-se,ymax=abground_biomass+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2")) + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", "", ""))

plot(dot_aboveground)
#----------------------------------------------------------------------------
#     Complete Graphic
summary_Aboveground_TEST <- data1
levels(summary_Aboveground_TEST$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_Aboveground_TEST$precip_pred) <- c("More", "Less")
levels(summary_Aboveground_TEST$competition) <- c("0","1")
summary_Aboveground_TEST <- summarySE(summary_Aboveground_TEST, measurevar="abground_biomass", groupvars=c("Group", "precip_pred", "competition"))
summary_Aboveground_TEST

Aboveground_biomass <- ggplot(summary_Aboveground_TEST, aes(x=competition, y=abground_biomass, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("") + ylab("Dry aboveground biomass (g)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=abground_biomass-se,ymax=abground_biomass+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", ""))

ggsave(path = outdir, 
       filename = "Aboveground_biomass.png", width = 5, height = 5)

#---------------------------------------------------------------------------
############################################################################
#              belowground biomass
############################################################################

# 1: Exploring data
data2<-data[complete.cases(data[,31]),] # to use only complete cases
boxplot(data2$bground_biomass) # looks good 
boxplot.stats(data2$bground_biomass)$out # 

#-----------------------------------------------
# 2: Model building
mod_belowground_biomass <- lmer(bground_biomass ~ nb_leavesi  + precip_pred * competition * Group
                             + (1|family), data=data2)
Anova(mod_belowground_biomass, type = 2) #blowground biomass
# competition, Group, competition:Group, precip_pred:competition:Group

#-------------------------------------------------
# 3: Checking Assumptions
## Test NORMALITY of residuals
shapiro.test(resid(mod_belowground_biomass))     # p-value =  1.974e-08
hist(resid(mod_belowground_biomass),breaks=50)   # no Gaussian distribution --> Kurtotis (see next 2 commands)
qqnorm(resid(mod_belowground_biomass)) 
qqline(resid(mod_belowground_biomass))           

# NEW model
mod_belowground_biomass_NEW <- lmer((bground_biomass)^0.42 ~ nb_leavesi  + precip_pred * competition * Group
                                 + (1|family), data=data2) # 

shapiro.test(resid(mod_belowground_biomass_NEW))   # p = 0.06292
hist(resid(mod_belowground_biomass_NEW),breaks=50) 
qqnorm(resid(mod_belowground_biomass_NEW)) 
qqline(resid(mod_belowground_biomass_NEW)) 

## HETEROCEDASTICITY
plot(resid(mod_belowground_biomass_NEW) ~ fitted(mod_belowground_biomass_NEW)) # it seems that there is some heteroscedasticity
# use the Bartlett test
bartlett.test((resid(mod_belowground_biomass_NEW))~interaction(data2$Group, data2$precip_pred, data2$competition), data=data2)
#CONCLUSION: p should be > 0.05 to be a Homocedastic model

# --------------------------------------------------------------------------
# 4: ANOVA 
Anova(mod_belowground_biomass_NEW,type=2) # type = 2 represents the error type 2
# # competition, Group, competition:Group, precip_pred:competition:Group // precip_pred:Group .
summary(mod_belowground_biomass_NEW)

# ---------------------------------------------------------------------------
# 5: Post-hoc 

# A Post-hoc test is only needed if there is significant interactions or significant factors with 3 or more levels
# competition:Group 
lsmeans(mod_belowground_biomass_NEW,pairwise ~ competition:Group, adjust="tukey")
#a<-lsmeans(mod_belowground_biomass_NEW, ~ competition:Group) 
#plot(a)

# precip_pred:competition:Group 
lsmeans(mod_belowground_biomass_NEW,pairwise ~ precip_pred:competition:Group, adjust="tukey")
a<-lsmeans(mod_belowground_biomass_NEW, ~ precip_pred:competition:Group) # M & L seem to be significant different in long drought without competition
plot(a)

# ---------------------------------------------------------------------------
#  6: main precipitation predictability effect 
t.test(bground_biomass ~ precip_pred, data) # compare M and L


t.test(bground_biomass ~ precip_pred, Sub1) # compare M and L  
t.test(bground_biomass ~ precip_pred, Sub2) # compare M and L
t.test(bground_biomass ~ precip_pred, Sub3) # compare M and L
t.test(bground_biomass ~ precip_pred, SubPlus) # compare M and L
t.test(bground_biomass ~ precip_pred, SubMinus) # compare M and L


# ---------------------------------------------------------------------------
# 7: Graphics 
data2$bground_biomass  <- data2$bground_biomass * 1000
summary_Belowground_TEST <- data2
levels(summary_Belowground_TEST$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_Belowground_TEST$precip_pred) <- c("L", "M")
summary_Belowground <- summarySE(summary_Belowground_TEST, measurevar="bground_biomass", groupvars=c("Group", "precip_pred"))
summary_Belowground

dot_belowground <- ggplot(summary_Belowground, aes(x=Group, y=bground_biomass, fill=precip_pred)) +
  theme_classic() +
  xlab("") + ylab("Dry belowground biomass (mg)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=bground_biomass-se,ymax=bground_biomass+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation history") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", "", ""))

plot(dot_belowground)

#----------------------------------------------------------------------------
#     Complete Graphic
summary_Belowground_TEST <- data2
levels(summary_Belowground_TEST$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_Belowground_TEST$precip_pred) <- c("L", "M")
levels(summary_Belowground_TEST$competition) <- c("0","1")
summary_Belowground_TEST <- summarySE(summary_Belowground_TEST, measurevar="bground_biomass", groupvars=c("Group", "precip_pred", "competition"))
summary_Belowground_TEST

Belowground_biomass <- ggplot(summary_Belowground_TEST, aes(x=competition, y=bground_biomass, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("") + ylab("Dry belowground biomass (mg)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=bground_biomass-se,ymax=bground_biomass+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", ""))

ggsave(path = outdir, 
       filename = "Belowground_biomass.png", width = 5, height = 5)


#---------------------------------------------------------------------------
############################################################################
#              root shoot ratio
############################################################################

# 1: Exploring data
data3<-data[complete.cases(data[,33]),] # to use only complete cases
boxplot(data3$RSratio)
boxplot.stats(data3$RSratio)$out # 
data3<-data3[!data3$RSratio>0.8,] # this has to be removed, probaly a mistake
boxplot(data3$RSratio) # looks good

#-----------------------------------------------
# 2: Model building
mod_RSratio <- lmer(RSratio ~ nb_leavesi  + precip_pred * competition * Group
                                + (1|family), data=data3)
Anova(mod_RSratio, type = 2) #blowground biomass
# Group, competition:Group, precip_pred:competition:Group

#-------------------------------------------------
# 3: Checking Assumptions
## Test NORMALITY of residuals
shapiro.test(resid(mod_RSratio))     # p =  0.001638
hist(resid(mod_RSratio),breaks=50)   # looks good, but try to improve
qqnorm(resid(mod_RSratio)) 
qqline(resid(mod_RSratio))           

# NEW model
mod_RSratio_NEW <- lmer((RSratio)^0.7 ~ nb_leavesi  + precip_pred * competition * Group
                                    + (1|family), data=data3) # p= 0.08487 --> not nice but above 0.05

shapiro.test(resid(mod_RSratio_NEW))   
hist(resid(mod_RSratio_NEW),breaks=50) 
qqnorm(resid(mod_RSratio_NEW)) 
qqline(resid(mod_RSratio_NEW)) 

## HETEROCEDASTICITY
plot(resid(mod_RSratio_NEW) ~ fitted(mod_RSratio_NEW)) # ok
bartlett.test((resid(mod_RSratio_NEW))~interaction(data3$Group, data3$precip_pred, data3$competition), data=data3)
#CONCLUSION: p should be > 0.05 to be a Homocedastic model

# --------------------------------------------------------------------------
# 4: ANOVA 
Anova(mod_RSratio_NEW,type=2) # type = 2 represents the error type 2
# # Group, competition:Group, precip_pred:competition:Group
summary(mod_RSratio_NEW)

# ---------------------------------------------------------------------------
# 5: Post-hoc 

# A Post-hoc test is only needed if there is significant interactions or significant factors with 3 or more levels
# competition:Group 
lsmeans(mod_RSratio_NEW,pairwise ~ competition:Group, adjust="tukey")
#a<-lsmeans(mod_belowground_biomass_NEW, ~ competition:Group) 
#plot(a)

# precip_pred:competition:Group 
lsmeans(mod_RSratio_NEW,pairwise ~ precip_pred:competition:Group, adjust="tukey")
a<-lsmeans(mod_RSratio_NEW, ~ precip_pred:competition:Group) # M & L seem to be significant different in long drought without competition
plot(a)

# ---------------------------------------------------------------------------
#  6: main precipitation predictability effect 
t.test(RSratio ~ precip_pred, data) # compare M and L


t.test(RSratio ~ precip_pred, Sub1) # compare M and L  
t.test(RSratio ~ precip_pred, Sub2) # compare M and L
t.test(RSratio ~ precip_pred, Sub3) # compare M and L
t.test(RSratio ~ precip_pred, SubPlus) # compare M and L
t.test(RSratio ~ precip_pred, SubMinus) # compare M and L

# ---------------------------------------------------------------------------
# 7: Graphics 
summary_RSratio <- data3
levels(summary_RSratio$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_RSratio$precip_pred) <- c("L", "M")
summary_RSratio <- summarySE(summary_RSratio, measurevar="RSratio", groupvars=c("Group", "precip_pred"))
summary_RSratio

dot_RSratio <- ggplot(summary_RSratio, aes(x=Group, y=RSratio, fill=precip_pred)) +
  theme_classic() +
  xlab("") + ylab("Root shoot ratio") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=RSratio-se,ymax=RSratio+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation history") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", "", ""))

plot(dot_RSratio)

#----------------------------------------------------------------------------
#     Complete Graphic
summary_RSratio <- data3
levels(summary_RSratio$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_RSratio$precip_pred) <- c("L", "M")
levels(summary_RSratio$competition) <- c("0","1")
summary_RSratio <- summarySE(summary_RSratio, measurevar="RSratio", groupvars=c("Group", "precip_pred", "competition"))
summary_RSratio

Root_Shoot_Ratio <- ggplot(summary_RSratio, aes(x=competition, y=RSratio, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("") + ylab("Root shoot ratio") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=RSratio-se,ymax=RSratio+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", ""))

ggsave(path = outdir, 
       filename = "Root_Shoot_Ratio.png", width = 5, height = 5)

#---------------------------------------------------------------------------
############################################################################
#              root length
############################################################################

# 1: Exploring data
data4<-data[complete.cases(data[,30]),] # to use only complete cases
boxplot(data4$root_length)

#-----------------------------------------------
# 2: Model building
mod_root_length<- lmer(root_length ~ nb_leavesi  + precip_pred * competition * Group
                    + (1|family), data=data4)
Anova(mod_RSratio, type = 2) #blowground biomass
# Group, competition:Group, precip_pred:competition:Group

#-------------------------------------------------
# 3: Checking Assumptions
## Test NORMALITY of residuals
shapiro.test(resid(mod_root_length))     # p =  0.001638
hist(resid(mod_root_length),breaks=50)   # looks good, but try to improve
qqnorm(resid(mod_root_length)) 
qqline(resid(mod_root_length))           

# NEW model
mod_root_length_NEW <- lmer((root_length)^0.42 ~ nb_leavesi  + precip_pred * competition * Group
                        + (1|family), data=data4) # p= 0.08487 --> not nice but above 0.05

shapiro.test(resid(mod_root_length_NEW))   
hist(resid(mod_root_length_NEW),breaks=50) 
qqnorm(resid(mod_root_length_NEW)) 
qqline(resid(mod_root_length_NEW)) 

## HETEROCEDASTICITY
plot(resid(mod_root_length_NEW) ~ fitted(mod_root_length_NEW)) # good
bartlett.test((resid(mod_root_length_NEW))~interaction(data4$Group, data4$precip_pred, data4$competition), data=data4)

# --------------------------------------------------------------------------
# 4: ANOVA 
Anova(mod_root_length_NEW,type=2) # type = 2 represents the error type 2
# # competition, Group, precip_pred:competition // precip_pred:competition:Group  .
summary(mod_root_length_NEW)

# ---------------------------------------------------------------------------
# 5: Post-hoc 

# A Post-hoc test is only needed if there is significant interactions or significant factors with 3 or more levels
# precip_pred:competition
lsmeans(mod_root_length_NEW,pairwise ~ precip_pred:competition, adjust="tukey")
a<-lsmeans(mod_root_length_NEW, ~ precip_pred:competition) 
plot(a)
lsmeans(mod_root_length_NEW,pairwise ~ precip_pred:competition:Group, adjust="tukey")

# ---------------------------------------------------------------------------
#  6: main precipitation predictability effect 
t.test(root_length ~ precip_pred, data) # compare M and L


t.test(root_length ~ precip_pred, Sub1) # compare M and L  
t.test(root_length ~ precip_pred, Sub2) # compare M and L
t.test(root_length ~ precip_pred, Sub3) # compare M and L
t.test(root_length ~ precip_pred, SubPlus) # compare M and L
t.test(root_length ~ precip_pred, SubMinus) # compare M and L


# ---------------------------------------------------------------------------
# 7: Graphics 
summary_root_length <- data4
levels(summary_root_length$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_root_length$precip_pred) <- c("L", "M")
summary_root_length <- summarySE(summary_root_length, measurevar="root_length", groupvars=c("Group", "precip_pred"))
summary_root_length

dot_root_length <- ggplot(summary_root_length, aes(x=Group, y=root_length, fill=precip_pred)) +
  theme_classic() +
  xlab("") + ylab("Root length") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=root_length-se,ymax=root_length+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", "", ""))

plot(dot_root_length)

#----------------------------------------------------------------------------
#     Complete Graphic
summary_root_length <- data4
levels(summary_root_length$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_root_length$precip_pred) <- c("L", "M")
levels(summary_root_length$competition) <- c("0","1")
summary_root_length <- summarySE(summary_root_length, measurevar="root_length", groupvars=c("Group", "precip_pred", "competition"))
summary_root_length

Root_Length <- ggplot(summary_root_length, aes(x=competition, y=root_length, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("") + ylab("Root length (cm)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=root_length-se,ymax=root_length+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "", ""))

ggsave(path = outdir, 
       filename = "Root_Length.png", width = 5, height = 5)

#---------------------------------------------------------------------------
############################################################################
#              number of leaves final
############################################################################

# 1: Exploring data
data5<-data[complete.cases(data[,28]),] # to use only complete cases
boxplot(data5$nb_leavesf) # no extreme outliers

#-----------------------------------------------
# 2: Model building
mod_nb_leaves<- lmer(nb_leavesf ~ nb_leavesi  + precip_pred * competition * Group
                       + (1|family), data=data5)
Anova(mod_nb_leaves, type = 2) 
# competition, Group, precip_pred:Group, competition:Group, precip_pred:competition:Group

#-------------------------------------------------
# 3: Checking Assumptions
## Test NORMALITY of residuals
shapiro.test(resid(mod_nb_leaves))     # p =  1.347e-05
hist(resid(mod_nb_leaves),breaks=50)  
qqnorm(resid(mod_nb_leaves)) 
qqline(resid(mod_nb_leaves))           

# NEW model
mod_nb_leaves_NEW <- lmer((nb_leavesf)^0.12 ~ nb_leavesi  + precip_pred * competition * Group
                            + (1|family), data=data5) # p= 0.08487 --> not nice but above 0.05

shapiro.test(resid(mod_nb_leaves_NEW))   
hist(resid(mod_nb_leaves_NEW),breaks=50) 
qqnorm(resid(mod_nb_leaves_NEW)) 
qqline(resid(mod_nb_leaves_NEW)) 

## HETEROCEDASTICITY
plot(resid(mod_nb_leaves_NEW) ~ fitted(mod_nb_leaves_NEW)) # good
bartlett.test((resid(mod_nb_leaves_NEW))~interaction(data5$Group, data5$precip_pred, data5$competition), data=data5)

# --------------------------------------------------------------------------
# 4: ANOVA 
Anova(mod_nb_leaves_NEW,type=2) # type = 2 represents the error type 2
# # competition, Group, precip_pred:Group, competition:Group, precip_pred:competition:Group  .
summary(mod_nb_leaves_NEW)

# ---------------------------------------------------------------------------
# 5: Post-hoc 

# A Post-hoc test is only needed if there is significant interactions or significant factors with 3 or more levels
# precip_pred:Group
lsmeans(mod_nb_leaves_NEW,pairwise ~ precip_pred:Group, adjust="tukey")
a<-lsmeans(mod_nb_leaves_NEW, ~ precip_pred:Group) 
plot(a)

# competition:Group
lsmeans(mod_nb_leaves_NEW,pairwise ~ competition:Group, adjust="tukey")
a<-lsmeans(mod_nb_leaves_NEW, ~ competition:Group) 
plot(a)

# precip_pred:competition:Group
lsmeans(mod_nb_leaves_NEW,pairwise ~ precip_pred:competition:Group, adjust="tukey")
a<-lsmeans(mod_nb_leaves_NEW, ~ precip_pred:competition:Group) 
plot(a)

# ---------------------------------------------------------------------------
# 6: Graphics 
summary_nb_leaves <- data5
levels(summary_nb_leaves$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_nb_leaves$precip_pred) <- c("L", "M")
summary_nb_leaves <- summarySE(summary_nb_leaves, measurevar="nb_leavesf", groupvars=c("Group", "precip_pred"))
summary_nb_leaves

dot_nb_leaves <- ggplot(summary_nb_leaves, aes(x=Group, y=nb_leavesf, fill=precip_pred)) +
  theme_classic() +
  xlab("Drought treatment") + ylab("Number of leaves") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=nb_leavesf-se,ymax=nb_leavesf+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation history") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "Control", "Short drought", "Long drought"))

plot(dot_nb_leaves)

# ---------------------------------------------------------------------------
#  main precipitation predictability effect 
t.test(nb_leavesf ~ precip_pred, data) # compare M and L

t.test(nb_leavesf ~ precip_pred, Sub1) # compare M and L  
t.test(nb_leavesf ~ precip_pred, Sub2) # compare M and L
t.test(nb_leavesf ~ precip_pred, Sub3) # compare M and L
t.test(nb_leavesf ~ precip_pred, SubPlus) # compare M and L
t.test(nb_leavesf ~ precip_pred, SubMinus) # compare M and L

#----------------------------------------------------------------------------
#     Complete Graphic
summary_nb_leaves <- data5
levels(summary_nb_leaves$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_nb_leaves$precip_pred) <- c("More", "Less")
levels(summary_nb_leaves$competition) <- c("0","1")
summary_nb_leaves <- summarySE(summary_nb_leaves, measurevar="nb_leavesf", groupvars=c("Group", "precip_pred", "competition"))
summary_nb_leaves

Nb_leaves <- ggplot(summary_nb_leaves, aes(x=competition, y=nb_leavesf, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("Competition") + ylab("Number of leaves") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=nb_leavesf-se,ymax=nb_leavesf+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="top") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "without", "with"))

ggsave(path = outdir, 
       filename = "Nb_leaves.png", width = 5, height = 5)
#---------------------------------------------------------------------------
############################################################################
#              length of longest leaf
############################################################################

# 1: Exploring data
data6<-data[complete.cases(data[,27]),] # to use only complete cases
boxplot(data6$longest_leaff) # good

#-----------------------------------------------
# 2: Model building
mod_longest_leaf<- lmer(longest_leaff ~ nb_leavesi  + precip_pred * competition * Group
                     + (1|family), data=data6)
Anova(mod_longest_leaf, type = 2) 
# precip_pred, competition, Group, competition:Group, precip_pred:competition:Group

#-------------------------------------------------
# 3: Checking Assumptions
## Test NORMALITY of residuals
shapiro.test(resid(mod_longest_leaf))     # p =  0.06367
hist(resid(mod_longest_leaf),breaks=50)  
qqnorm(resid(mod_longest_leaf)) 
qqline(resid(mod_longest_leaf))           

# NEW model
mod_longest_leaf_NEW <- lmer((longest_leaff)^0.8 ~ nb_leavesi  + precip_pred * competition * Group
                          + (1|family), data=data6) 

shapiro.test(resid(mod_longest_leaf_NEW))   
hist(resid(mod_longest_leaf_NEW),breaks=50) 
qqnorm(resid(mod_longest_leaf_NEW)) 
qqline(resid(mod_longest_leaf_NEW)) 


## HETEROCEDASTICITY
plot(resid(mod_longest_leaf_NEW) ~ fitted(mod_longest_leaf_NEW))
bartlett.test((resid(mod_longest_leaf_NEW))~interaction(data6$Group, data6$precip_pred, data6$competition), data=data6)


# --------------------------------------------------------------------------
# 4: ANOVA 
Anova(mod_longest_leaf_NEW,type=2) # type = 2 represents the error type 2
# precip_pred, competition, Group, competition:Group, precip_pred:competition:Group
summary(mod_longest_leaf_NEW)

# ---------------------------------------------------------------------------
# 5: Post-hoc 
# A Post-hoc test is only needed if there is significant interactions or significant factors with 3 or more levels

# competition:Group
lsmeans(mod_longest_leaf_NEW,pairwise ~ competition:Group, adjust="tukey")
a<-lsmeans(mod_longest_leaf_NEW, ~ competition:Group) 
plot(a)


# precip_pred:competition:Group
lsmeans(mod_longest_leaf_NEW,pairwise ~ precip_pred:competition:Group, adjust="tukey")
a<-lsmeans(mod_longest_leaf_NEW, ~ precip_pred:competition:Group) 
plot(a)

# ---------------------------------------------------------------------------
# 6: Significant main precipitation predictability effect 
t.test(longest_leaff ~ precip_pred, data) # compare M and L

t.test(longest_leaff ~ precip_pred, Sub1) # compare M and L  
t.test(longest_leaff ~ precip_pred, Sub2) # compare M and L
t.test(longest_leaff ~ precip_pred, Sub3) # compare M and L
t.test(longest_leaff ~ precip_pred, SubPlus) # compare M and L
t.test(longest_leaff ~ precip_pred, SubMinus) # compare M and L
# ---------------------------------------------------------------------------
# 7: Graphics 
summary_longest_leaf <- data6
levels(summary_longest_leaf$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_longest_leaf$precip_pred) <- c("More", "Less")
summary_longest_leaf <- summarySE(summary_longest_leaf, measurevar="longest_leaff", groupvars=c("Group", "precip_pred"))
summary_longest_leaf

dot_longest_leaf <- ggplot(summary_longest_leaf, aes(x=Group, y=longest_leaff, fill=precip_pred)) +
  theme_classic() +
  xlab("Drought treatment") + ylab("Length of longest leaf (cm)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=longest_leaff-se,ymax=longest_leaff+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "Control", "Short drought", "Long drought"))

plot(dot_longest_leaf)

#----------------------------------------------------------------------------
#     Complete Graphic

summary_longest_leaf <- data6
levels(summary_longest_leaf$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_longest_leaf$precip_pred) <- c("More", "Less")
levels(summary_longest_leaf$competition) <- c("0","1")
summary_longest_leaf <- summarySE(summary_longest_leaf, measurevar="longest_leaff", groupvars=c("Group", "precip_pred", "competition"))
summary_longest_leaf

Longest_leaf <- ggplot(summary_longest_leaf, aes(x=competition, y=longest_leaff, fill=precip_pred)) + theme_bw() + facet_wrap(~ Group, labeller = labeller) +
  xlab("Competition") + ylab("Length of longest leaf (cm)") + theme(text = element_text(size=18)) +
  geom_errorbar(aes(ymin=longest_leaff-se,ymax=longest_leaff+se), position = position_dodge(width=0.3), width=.12, size=1, color="black")+
  geom_point(size=3.5,position = position_dodge(width=0.3),shape=21)+
  scale_fill_manual(values = c("purple","cyan2"), name = "Precipitation predictability") + # change the filled colour
  theme(legend.position="top") + 
  ggtitle("") + theme(plot.title = element_text(size = 12, face = "italic")) + scale_x_discrete(labels=c( "without", "with"))

ggsave(path = outdir, 
       filename = "Longest_leaf.png", width = 5, height = 5)

#----------------------------------------------------------------------------
# Save Graphics
# without competition
ggsave(path = outdir, 
       filename = "dot_belowground.png", width = 5, height = 5)
ggsave(path = outdir, 
       filename = "dot_aboveground.png", width = 5, height = 5)
ggsave(path = outdir, 
       filename = "dot_RSratio.png", width = 5, height = 5)
ggsave(path = outdir, 
       filename = "dot_root_length.png", width = 5, height = 5)
ggsave(path = outdir, 
       filename = "dot_nb_leaves.png", width = 5, height = 5)
ggsave(path = outdir, 
       filename = "dot_longest_leaf.png", width = 5, height = 5)

ML_Plot <- ggarrange(dot_belowground, dot_aboveground, dot_RSratio, dot_root_length, dot_nb_leaves, dot_longest_leaf , 
          labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2, common.legend=T, legend = "bottom" ) +  scale_fill_manual(name = "Precipitation predictability")
ggsave(path = outdir, 
       filename = "ML_Plot.png", width = 10, height = 14) 

# ---------------------------------------------------------------------------
# The CompletePlot plot is the important one, because here we can check all fixed factors together
CompletePlot <- ggarrange(Aboveground_biomass,Belowground_biomass, Root_Shoot_Ratio, Root_Length, Nb_leaves, Longest_leaf,
                          labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2, common.legend=T, legend = "bottom" ) + scale_fill_manual(name = "Precipitation predictability")
CompletePlot
ggsave(path = outdir, 
       filename = "CompletePlot.png", width = 12, height = 14) 






