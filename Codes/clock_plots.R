library(plotrix)
library(lubridate)
plot_clock = function(file, panel_letter, light=5, dark=20, mar=c(0,1,0,1)){
  Data = read.csv(paste0("Stark_et_al_ELE/Data/microhabitat_selection/",file))
  Data = Data[!is.na(Data$de),]
  Data$round_dt = ymd_hms(Data$round_dt)
  Data$hour = hour(Data$round_dt)
  Data$microhabitat = factor(Data$microhabitat, levels = c("open", "burrow", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large"), ordered = T)
  colors_table = data.frame(microhabitat = c("open", "burrow", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large"),
                            color = c("orangered1","brown",
                                      "chartreuse2","chartreuse3","chartreuse4",
                                      "azure3","azure4","grey20"))
  Data = merge(Data, colors_table, by="microhabitat", x.all=T)                      
  Data$hour_jitter = Data$hour+runif(length(Data$round_dt), 0, 0.95)
  Data = Data[order(Data$hour, Data$microhabitat),]
  hours = unique(Data$hour)
  for (ihour in hours){
    Data[Data$hour==ihour,"hour_jitter"] = sort(Data[Data$hour==ihour,"hour_jitter"])
    #Data[Data$hour==ihour,"microhabitat"] = sort(Data[Data$hour==ihour,"microhabitat"])
  }
  h=c(seq(0,light, 0.05), seq(dark,24, 0.05))
  l = rep(4, length(h))
  #par(mar=c(0,0,0,0))
  clock24.plot(c(Data$de,l), c(Data$hour_jitter,h), show.grid.labels=0, labels = 0:23, radial.labels=FALSE, line.col=c(Data$color, rep("tan",length(h))), lwd=2, mar=mar)
  text(0, 2, "night", cex=1.3)
  text(-4, 4, panel_letter, cex=2)
}
#summer 
tiff(filename = "clocks summer.tiff", width=2500, height = 5000, compression = "lzw", res=300)
par(mfrow = c(6,3), oma = c(0,4,4,0) + 0.1,
    mar = c(0,0,2,2) + 0.1)
top=c(1,0,5,0)
plot_clock("microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(a)", mar=top)
mtext(side=3, line=2, "All microhabitats", cex=1.5)
mtext(side=2, "Current climate", line=2, cex=1.5)
plot_clock("microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Bush.csv", ,panel_letter="(b)", mar=top)
mtext(side=3, line=2, "Loss of bushes", cex=1.5)
mtext(side=3, line=4.5, "Summer", cex=2)
plot_clock("microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(c)" , mar=top)
mtext(side=3, line=2, "Loss of rocks", cex=1.5)
middle =c(3,0,3,0)
plot_clock("microhabitat_selection_3_Summer_future_microclimate_and_operative_temperatures_all_habitats.csv", panel_letter="(d)", mar=middle)
mtext(side=2, expression("+3"*~degree*C), line=2, cex=1.5)
plot_clock("microhabitat_selection_3_Summer_future_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(e)", mar=middle)
plot_clock("microhabitat_selection_3_Summer_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(f)",  mar=middle)
bottom =c(5,0,1,0)
plot_clock("microhabitat_selection_6_Summer_future_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(g)", mar=bottom)
mtext(side=2, expression("+6"*~degree*C), line=2, cex=1.5)
plot_clock("microhabitat_selection_6_Summer_future_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(h)", mar=bottom)
plot_clock("microhabitat_selection_6_Summer_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(i)", mar=bottom)

plot_clock("microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(j)", light = 8, dark = 18, mar=top)
mtext(side=3, line=2, "All microhabitats", cex=1.5)
mtext(side=2, "Current climate", line=2, cex=1.5)
plot_clock("microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(k)", light = 8, dark = 18, mar=top)
mtext(side=3, line=2, "Loss of bushes", cex=1.5)
mtext(side=3, line=4.5, "Winter", cex=2)
plot_clock("microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(l)", light = 8, dark = 18, mar=top)
mtext(side=3, line=2, "Loss of rocks", cex=1.5)
plot_clock("microhabitat_selection_3_Winter_future_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(m)", light = 8, dark = 18, mar=middle)
mtext(side=2, expression("+3"*~degree*C), line=2, cex=1.5)
plot_clock("microhabitat_selection_3_Winter_future_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(n)", light = 8, dark = 18, mar=middle)
plot_clock("microhabitat_selection_3_Winter_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(o)", light = 8, dark = 18, mar=middle)
plot_clock("microhabitat_selection_6_Winter_future_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(p)", light = 8, dark = 18, mar=bottom)
mtext(side=2, expression("+6"*~degree*C), line=2, cex=1.5)
plot_clock("microhabitat_selection_6_Winter_future_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(q)", light = 8, dark = 18, mar=bottom)
plot_clock("microhabitat_selection_6_Winter_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(r)", light = 8, dark = 18, mar=bottom)

dev.off()
