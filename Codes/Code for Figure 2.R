##################################################
#Figures:
library(lubridate)
library(ggplot2)
############################################Figure 2 START############################################
Data_summer <-read.csv('Data/Summer_microhabitat_selection-For Figure 2.csv')
Data_winter <-read.csv('Data/winter_microhabitat_selection-For Figure 2.csv')

tiff(file="Figure 2A-Summer.tiff", width=3800, height=2500, res=300, compression="lzw")
a<-ggplot(data=Data_summer, aes(x=Active_location, y=Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,40,5))+
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
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,70,5))+
  scale_fill_manual(values = c("gray26","orangered1","azure2"
                               ,"chartreuse2"))
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
############################################Figure 2 END############################################

################################Supplementary figure 3s - habitat loss scenarios################################
Data_no_bush_summer <- read.csv('Data/No_Bush_Summer_microhabitat_selection-for figure 2.csv')
Data_no_bush_winter <- read.csv('Data/No_Bush_Winter_microhabitat_selection-for figure 2.csv')
Data_no_rock_summer <- read.csv('Data/No_rock_Summer_microhabitat_selection-for figure 2.csv')
Data_no_rock_winter <- read.csv('Data/No_Rock_Winter_microhabitat_selection-for figure 2.csv')

tiff(file="Figure 2-no bush summer.tiff", width=3500, height=2800, res=300, compression="lzw")
c<-ggplot(data=Data_no_bush_summer/Data_no_bush_winter/Data_no_rock_summer/Data_no_rock_winter, 
          aes(x=Active_location, y= Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,80,10))+
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

tiff(file="Figure 2-no bush winter.tiff", width=3500, height=2800, res=300, compression="lzw")
c<-ggplot(data=Data_no_bush_winter/Data_no_bush_winter/Data_no_rock_summer/Data_no_rock_winter, 
          aes(x=Active_location, y= Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,80,10))+
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
