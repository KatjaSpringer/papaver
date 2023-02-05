setwd("C:/Users/katja/Desktop/Uni Frankfurt/Semester 2/Evolutionary Ecology of Plants and Global Change/Experiment")

library(readxl)
Soil_humidity <- read_excel("Soil_humidity.xlsx")
Soil_humidity$drought_treatment <- as.factor(Soil_humidity$drought_treatment)
Soil_humidity$prec_pred <- as.factor(Soil_humidity$prec_pred)
Soil_humidity$competition <- as.factor(Soil_humidity$competition)
Soil_humidity$date <- as.factor(Soil_humidity$date)
Soil_humidity$humidity <- as.numeric(Soil_humidity$humidity)

str(Soil_humidity)
#View(Soil_humidity)

aggregate(humidity~drought_treatment + date,data=Soil_humidity,mean)


library(dplyr)
library(ggplot2)
library(Rmisc)


summaryHUM <- summarySE(Soil_humidity, measurevar="humidity", groupvars=c("date","drought_treatment"))
summaryHUM


a <- ggplot(summaryHUM, aes(x=date, y=humidity, group=drought_treatment, colour=drought_treatment)) + 
  geom_line() +
  geom_point() +
  ylim(c(0,60)) +
  scale_color_manual(values = c("darkgreen",
                                "darkred",
                                "gold"), name = "Drought treatment", labels = c("Control", "Long drought", "Short drought")) +
  xlab("Date") + ylab("Soil humidity (%)") +
  theme_bw() +
  ggtitle("Soil humidity during the drought treatment") +
  labs(fill = "Drought treatment") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  geom_errorbar(aes(ymin=humidity-se,ymax=humidity+se), 
                position = position_dodge(width=0), width=.2, size=0.5, color="black")
a
