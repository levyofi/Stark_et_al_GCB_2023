library(nlme)
options(scipen = 999)
Data <- read.csv("../Stark_et_al_ELE/Data/Data of TPref and field body temperatures.txt")
quantile(Data[Data$Temp_Origin=="Lab" & Data$Season=="Summer",]$Body_Temperature)
# 0%      25%      50%      75%     100% 
# 30.21111 34.07222 35.61111 36.99722 38.52222 
quantile(Data[Data$Temp_Origin=="Lab" & Data$Season=="Summer",]$Body_Temperature, c(0.1, 0.9))
# 10%      90% 
#   32.38889 37.84444
mean(Data[Data$Temp_Origin=="Lab" & Data$Season=="Summer",]$Body_Temperature)
# [1] 35.32863
sd(Data[Data$Temp_Origin=="Lab" & Data$Season=="Summer",]$Body_Temperature)

quantile(Data[Data$Temp_Origin=="Lab" & Data$Season=="Winter",]$Body_Temperature)
# 0%      25%      50%      75%     100% 
# 25.32222 30.28611 32.00556 32.86111 36.34444
quantile(Data[Data$Temp_Origin=="Lab" & Data$Season=="Winter",]$Body_Temperature, c(0.1, 0.9))
# 10%      90% 
#   28.70667 35.26889
mean(Data[Data$Temp_Origin=="Lab" & Data$Season=="Winter",]$Body_Temperature)
# [1] 31.71444
sd(Data[Data$Temp_Origin=="Lab" & Data$Season=="Winter",]$Body_Temperature)
# [1] 2.589436

lme.model <- lme(Body_Temperature ~ Temp_Origin*Season, data = Data, weights = varIdent(form =~1|Temp_Origin*Season), random=~1|ID) 
summary(lme.model)
#remove insignificant interaction
lme.model <- lme(Body_Temperature ~ Temp_Origin+Season, data = Data, weights = varIdent(form =~1|Temp_Origin*Season),  random=~1|ID) 
summary(lme.model)

library(plyr)
results  = ddply(Data[Data$Temp_Origin=="Lab",], .(Season), summarise, mean = mean(Body_Temperature), sd=sd(Body_Temperature), CI10 = quantile(Body_Temperature, 0.10), CI90 = quantile(Body_Temperature, 0.90))


#create Figure S3
library(ggplot2)
Labels_X <- c("Field Summer", "Lab Summer", "Field Winter","Lab Winter")
# Create interaction of Season and Temp_Origin
Data$Interaction <- interaction(Data$Season, Data$Temp_Origin)

# Convert Interaction to an ordered factor
Data$Interaction <- factor(Data$Interaction, levels = c("Summer.Field", "Summer.Lab", "Winter.Field", "Winter.Lab"))

ggplot(Data, aes(x = Interaction, y = Body_Temperature, fill = Season), outlier.shape = NA)+
  geom_boxplot() +
  geom_jitter(width = 0.2, ) +
  scale_fill_manual(values = c("Summer" = "yellow1", "Winter" = "blue")) +
  theme_minimal() +
  scale_x_discrete(name="",labels=Labels_X)+
  scale_y_continuous(name="Body temperature (°C)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5, color = "black"), # adjust size and color of the ticks here
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
