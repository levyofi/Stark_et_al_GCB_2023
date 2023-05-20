#######################################################################################################
#Packages:
library(lubridate)
library(ggplot2)
############################################Figure 2-Starts############################################
#Upload datasets:
Data_summer <-read.csv('Data/Summer_microhabitat_selection-For Figure 2.csv')
Data_winter <-read.csv('Data/winter_microhabitat_selection-For Figure 2.csv')
Data_no_bush_summer <- read.csv('Data/No_Bush_Summer_microhabitat_selection-for figure 2.csv')
Data_no_bush_winter <- read.csv('Data/No_Bush_Winter_microhabitat_selection-for figure 2.csv')
Data_no_rock_summer <- read.csv('Data/No_rock_Summer_microhabitat_selection-for figure 2.csv')
Data_no_rock_winter <- read.csv('Data/No_Rock_Winter_microhabitat_selection-for figure 2.csv')

#Visualize data:
tiff(file="Figure 2A-Summer.tiff", width=3800, height=2500, res=300, compression="lzw")
a<-ggplot(data=Data_summer, aes(x=Active_location, y=Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Predicted time spent in location (%)", breaks = seq(0,90,10))+
  scale_fill_manual(values = c("gray26","orangered1","azure4","azure3","azure2",
                               "chartreuse4","chartreuse3","chartreuse2"))
a
a1 <- a+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
a2 <- a1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
a2
dev.off()

tiff(file="Figure 2B-Winter.tiff", width=3800, height=2500, res=300, compression="lzw")
b<-ggplot(data=Data_winter, aes(x=Active_location, y=Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Predicted time spent in location (%)", breaks = seq(0,90,10))+
  scale_fill_manual(values = c("gray26","orangered1"))
b
b1 <- b+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
b2 <- b1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
b2

dev.off()

tiff(file="Figure 2C-no bush summer.tiff", width=3500, height=2800, res=300, compression="lzw")
c<-ggplot(data=Data_no_bush_summer, aes(x=Active_location, y= Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Predicted time spent in location (%)", breaks = seq(0,90,10))+
  scale_fill_manual(values = c("gray26","orangered1","azure4","azure3","azure2"))
c
c1 <- c+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
c2 <- c1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
c2
dev.off()

tiff(file="Figure 2D-no bush winter.tiff", width=3500, height=2800, res=300, compression="lzw")
d<-ggplot(data=Data_no_bush_winter, aes(x=Active_location, y= Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Predicted time spent in location (%)", breaks = seq(0,90,10))+
  scale_fill_manual(values = c("gray26","orangered1"))
d
d1 <- d+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
d2 <- d1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
d2
dev.off()

tiff(file="Figure 2E-no rock summer.tiff", width=3500, height=2800, res=300, compression="lzw")
e<-ggplot(data=Data_no_rock_summer, aes(x=Active_location, y= Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Predicted time spent in location (%)", breaks = seq(0,90,10))+
  scale_fill_manual(values = c("gray26","orangered1","chartreuse4","chartreuse3","chartreuse2"))
e
e1 <- e+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
e2 <- e1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
e2
dev.off()

tiff(file="Figure 2F-no rock winter.tiff", width=3500, height=2800, res=300, compression="lzw")
f<-ggplot(data=Data_no_rock_winter, aes(x=Active_location, y= Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Predicted time spent in location (%)", breaks = seq(0,90,10))+
  scale_fill_manual(values = c("gray26","orangered1"))
f
f1 <- f+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
f2 <- f1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
f2
dev.off()
############################################Figure 2 - ENDS############################################