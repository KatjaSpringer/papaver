## Load Packages
#install.packages("Rmisc")
library(DataExplorer) # to explore the data
library(lme4) # for LMER
library(car) # for Anova
library(lsmeans) # for posthocs (alternative emmeans)
library(ggplot2) # for ggplot2
library(Rmisc) # for summarySE
library(dplyr)
library(ggpubr) # for ggarrange
library(readxl)

setwd("C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment")
data <-read_excel("Measures.xlsx", col_types = c("text", 
                                                 "text", "text", "text", "numeric","numeric",   "numeric", "text", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",   "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

#---------------------------------------------------------------------------
#   CHECKING And calculate root shoot ratio
str(data)

data$competition <- as.factor(data$competition)
data$precip_pred <- as.factor(data$precip_pred)
data$family <- as.factor(data$family)
data$Group <- as.factor(data$Group)
data$edge_effect <- as.factor(data$edge_effect)

data$RSratio <- data$bground_biomass/data$abground_biomass # calculate root shoot ratio

# function for changing panel names later
names <- list("1control" = "Control","2short_drought" = "Short drought","3long_drought" = "Long drought")
labeller <- function(variable,value){
  return(names[value])
}

#---------------------------------------------------------------------------
############################################################################
# Testing whether there is an edge effect 
#     All 6 competition trays

Subcompetition <- subset(data, competition == "1") # subset
Subcompetition <- subset(Subcompetition, !is.na(Subcompetition[,31])) # remmoving died ones (NA everywhere)

str(Subcompetition)
# abovegrounf biomass
t.test( x = Subcompetition$abground_biomass[Subcompetition$edge_effect == '1'], y = Subcompetition$abground_biomass[Subcompetition$edge_effect == '0'])
#     Long drought M
Sub_Comp_Long <- subset(Subcompetition, Group == "long_drought") # subset
Sub_Comp_Long_M <- subset(Sub_Comp_Long, precip_pred == "M")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Long_M)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Long_M)
t.test(root_length ~ edge_effect, Sub_Comp_Long_M)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Long_M)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Long_M)
#     Long drought L
Sub_Comp_Long_L <- subset(Sub_Comp_Long, precip_pred == "L")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Long_L)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Long_L)
t.test(root_length ~ edge_effect, Sub_Comp_Long_L)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Long_L)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Long_L)
#     Short drought M
Sub_Comp_Short <- subset(Subcompetition, Group == "short_drought") # subset
Sub_Comp_Short_M <- subset(Sub_Comp_Short, precip_pred == "M")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Short_M)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Short_M)
t.test(root_length ~ edge_effect, Sub_Comp_Short_M)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Short_M)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Short_M)
#     Short drought L
Sub_Comp_Short_L <- subset(Sub_Comp_Short, precip_pred == "L")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Short_L)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Short_L)
t.test(root_length ~ edge_effect, Sub_Comp_Short_L)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Short_L)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Short_L)
#     Control M
Sub_Comp_Con <- subset(Subcompetition, Group == "control") # subset
Sub_Comp_Con_M <- subset(Sub_Comp_Con, precip_pred == "M")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Con_M)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Con_M)
t.test(root_length ~ edge_effect, Sub_Comp_Con_M)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Con_M)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Con_M)
#     Control L
Sub_Comp_Con_L <- subset(Sub_Comp_Con, precip_pred == "L")
t.test(abground_biomass ~ edge_effect, Sub_Comp_Con_L)
t.test(bground_biomass ~ edge_effect, Sub_Comp_Con_L)
t.test(root_length ~ edge_effect, Sub_Comp_Con_L)
t.test(nb_leavesf ~ edge_effect, Sub_Comp_Con_L)
t.test(longest_leaff ~ edge_effect, Sub_Comp_Con_L)
# checking for outliers --> by removing outlier, no effect
boxplot(Sub_Comp_Con_L$nb_leavesf)
NoOut_Sub_Comp_Con_L<-Sub_Comp_Con_L[!Sub_Comp_Con_L$nb_leavesf>16,] 
t.test(nb_leavesf ~ edge_effect, NoOut_Sub_Comp_Con_L)

#---------------------------------------------------------------------------
############################################################################
#              aboveground biomass
############################################################################

    # 1: Exploring data
data1<-data[complete.cases(data[,32]),] # to use only complete cases
boxplot(data1$abground_biomass)
boxplot.stats(data1$abground_biomass)$out # 
#data_aboveground_biomass<-data1[!data1$abground_biomass>0.7,] # to remove outliers --> only extreme: 0.7 --> not remove!
boxplot(data_aboveground_biomass$abground_biomass) # check if it worked

# ggplot(data_aboveground_biomass, aes(x = competition, y = abground_biomass)) + geom_boxplot(fill = "green") 

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
      # 6: Graphics 
# always in alphabetic order
summary_Aboveground_TEST <- data1
levels(summary_Aboveground_TEST$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_Aboveground_TEST$precip_pred) <- c("L", "M")
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
levels(summary_Aboveground_TEST$precip_pred) <- c("L", "M")
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

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "Aboveground_biomass.png", width = 5, height = 5)

#---------------------------------------------------------------------------
############################################################################
#              belowground biomass
############################################################################

# 1: Exploring data
data2<-data[complete.cases(data[,31]),] # to use only complete cases
boxplot(data2$bground_biomass)
boxplot.stats(data2$bground_biomass)$out # 
#data_belowground_biomass<-data2[!data2$bground_biomass>0.034,] # to remove outliers --> only extreme: dont remove
boxplot(data_belowground_biomass$bground_biomass) # check if it worked

# ggplot(data_belowground_biomass, aes(x = competition, y = bground_biomass)) + geom_boxplot(fill = "green") 

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
bartlett.test((resid(mod_belowground_biomass_NEW))~interaction(data_belowground_biomass$Group, data_belowground_biomass$precip_pred, data_belowground_biomass$competition), data=data_belowground_biomass)
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
# 6: Graphics 
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

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
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
# 6: Graphics 
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

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
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
# 6: Graphics 
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

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
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

#----------------------------------------------------------------------------
#     Complete Graphic

summary_nb_leaves <- data5
levels(summary_nb_leaves$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_nb_leaves$precip_pred) <- c("Less", "More")
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

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
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
# 6: Graphics 
summary_longest_leaf <- data6
levels(summary_longest_leaf$Group) <- c("1control","2short_drought", "3long_drought" )
levels(summary_longest_leaf$precip_pred) <- c("Less", "More")
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
levels(summary_longest_leaf$precip_pred) <- c("Less", "More")
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

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "Longest_leaf.png", width = 5, height = 5)

#----------------------------------------------------------------------------
# Save Graphics
# without competition
ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "dot_belowground.png", width = 5, height = 5)
ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "dot_aboveground.png", width = 5, height = 5)
ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "dot_RSratio.png", width = 5, height = 5)
ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "dot_root_length.png", width = 5, height = 5)
ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "dot_nb_leaves.png", width = 5, height = 5)
ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "dot_longest_leaf.png", width = 5, height = 5)

ML_Plot <- ggarrange(dot_belowground, dot_aboveground, dot_RSratio, dot_root_length, dot_nb_leaves, dot_longest_leaf , 
          labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2, common.legend=T, legend = "bottom" ) +  scale_fill_manual(name = "Precipitation predictability")

ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "ML_Plot.png", width = 10, height = 14) 

# ---------------------------------------------------------------------------

CompletePlot <- ggarrange(Aboveground_biomass,Belowground_biomass, Root_Shoot_Ratio, Root_Length, Nb_leaves, Longest_leaf,
                          labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2, common.legend=T, legend = "bottom" ) + scale_fill_manual(name = "Precipitation predictability")


ggsave(path = "C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment/Graphics", 
       filename = "CompletePlot.png", width = 12, height = 14) 






