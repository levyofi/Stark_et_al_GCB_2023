library(ggplot2)
library(lubridate)
library(dplyr)
library(nlme)
options(scipen = 999)
setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Temperature preference experiment/Thermal Pictures and Data of M. bahaeldini')
Data <- read.csv('Stark_et_al_ELE/Data/Data of TPref~origin+season - 12.6.22.csv')
gls.model <- gls(Body_Temperature ~ Temp_Origin*Season, data = Data, weights = varIdent(form =~1|Temp_Origin*Season)) 
summary(gls.model)

e = residuals(gls.model, type="n")
plot(e)
boxplot(e~Data$Season+Data$Temp_Origin)
plot(e~Data$Body_Temperature)

#FIGURES:
###############################################################################################
#############################BOXPLOT - for comparison between field BT and Lab BT##################
library(ggplot2)
Data <- read.csv(file.choose())
Labels_X <- c("Field Summer", "Lab Summer", "Field Winter","Lab Winter")
g <- ggplot(Data, aes(Season,Body_Temperature)) +geom_boxplot(aes(fill=Season),outlier.shape = NA)+
  geom_jitter(width = 0.2, )
g
g1 <- g+theme_bw()+theme(axis.text=element_text(face='bold', size=15), axis.text.x=element_text(size=20),
                         axis.text.y=element_text(color="black",face='bold',size=10)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20),
                         legend.text=element_text(size=20))+
  scale_fill_manual(values = c("Yellow1", "Yellow1","Blue","Blue"))+
  scale_x_discrete(name="Temperature origin|Season",labels=Labels_X)+
  scale_y_continuous(name="Body temperature (°C)")
g1
g2 <- g1+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g2
