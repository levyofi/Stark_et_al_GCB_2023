##################################################
#Figures:
library(lubridate)
library(ggplot2)
############################################Figure 2 START############################################
Data_summer <-read.csv('Data/Summer_microhabitat_selection-For Figure 2.csv')
Data_winter <-read.csv('Data/winter_microhabitat_selection-For Figure 2.csv')

tiff(file="Figure 2A-Summer.tiff", width=3800, height=2500, res=300, compression="lzw")
p<-ggplot(data=Data_summer, aes(x=Active_location, y=Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,40,5))+
  scale_fill_manual(values = c("gray26","orangered1","azure4","azure3","azure2",
                               "chartreuse4","chartreuse3","chartreuse2"))
p
p1 <- p+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
p2 <- p1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
p2
dev.off()

tiff(file="Figure 2B-Winter.tiff", width=3800, height=2500, res=300, compression="lzw")
e<-ggplot(data=Data_winter, aes(x=Active_location, y=Round_Percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,70,5))+
  scale_fill_manual(values = c("gray26","orangered1","azure2"
                               ,"chartreuse2"))
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
############################################Figure 2 END############################################

################################Supplementary figure 2 - habitat loss scenarios################################
Data <- read.csv(file.choose())
#create table with proportions for activity location
new_data <- aggregate(cbind(Proportion = Round_percentage) ~ Active_location,Data,mean)
new_data$Proportion <- new_data$Proportion*100

tiff(file="Figure 3 for suppl-no_rock_winter.tiff", width=3500, height=2800, res=300, compression="lzw")
p<-ggplot(data=new_data, aes(x=Active_location, y= Proportion)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Location of lizard's activity")+
  scale_y_continuous("Percentage of time spent in location", breaks = seq(0,70,10))+
  scale_fill_manual(values = c("gray26","orangered1",
                               "chartreuse4","chartreuse3","chartreuse2"
                               ))
p
p1 <- p+theme_bw()+theme(axis.text=element_text(face='bold', size=22), axis.text.x=element_text(size=22)
                         ,axis.title.x=element_text(face='bold', size=22),
                         axis.title.y=element_text(face='bold', size=22),
                         legend.text=element_text(size=22))
p2 <- p1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
p2
dev.off()
