library(nlme)
options(scipen = 999)
Data <- read.csv("Stark_et_al_ELE/Data/Data of TPref and field body temperatures.txt")
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

gls.model <- lme(Body_Temperature ~ Temp_Origin*Season, data = Data, weights = varIdent(form =~1|Temp_Origin*Season), random=~1|ID) 
summary(gls.model)
gls.model <- lme(Body_Temperature ~ Temp_Origin+Season, data = Data, weights = varIdent(form =~1|Temp_Origin*Season),  random=~1|ID) 
summary(gls.model)

library(plyr)
results  = ddply(Data[Data$Temp_Origin=="Lab",], .(Season), summarise, mean = mean(Body_Temperature), sd=sd(Body_Temperature), CI10 = quantile(Body_Temperature, 0.10), CI90 = quantile(Body_Temperature, 0.90))
