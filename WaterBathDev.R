caldf<-cal_diff

library(dplyr)
library(tidyr)
library(ggpubr)
library(viridis)

caldf_l<-caldf%>%select(3,6:32)%>%gather(key=Logger,value=diff,2:28)%>%filter(diff>=-2.5)



caldf_l<-separate(data = caldf_l,
                 col = Logger,
                 into = c("Logger", "Rep","blah"),
                 sep = "\\_")

View(caldf_l)

ggscatter(caldf_l, x = "diff", y = "Logger",
          color = "Instantaneous", fill="Instantaneous",palette="Inferno",shape = 21, size = 3 # Points color, shape and size
)+
  xlab("Deviation from Calibration Temperature (°C)")+
  ylab("")+
  scale_y_discrete(limits = c("Minidot","Odyssey","Tidbit","PendantMX","PendantLT","Pendant","Pro","Nat", "Seabird"),labels=c("Minidot","Odyssey","Tidbit","PendantLT_MX","Pendant_LT","Pendant","Pro","Star Oddi","Seabird"))+
  theme(legend.position="none")


cd<-ggplot(caldf_l, aes(diff, Logger))+
  geom_jitter(width=0.01,height=0.3,size=1, shape=21,colour="black",aes(fill= Instantaneous))+
  scale_fill_gradientn(colours = inferno(256))+
  #scale_color_gradientn(colours = inferno(256))+
  theme_classic()+
  xlab("Deviation from Calibration Temperature (°C)")+
  ylab("")+
  xlim(-2.5,2.5)+
  scale_y_discrete(limits = c("Minidot","Odyssey","Tidbit","PendantMX","PendantLT","Pendant","Pro","Nat", "Seabird"),labels=c("Minidot","Odyssey","Tidbit","PendantLT_MX","Pendant_LT","Pendant","Pro","Star Oddi","Seabird"), guide = guide_axis(angle = 45))+
  theme(legend.position="none")
 # coord_fixed(ratio=.15)

ggsave(
  "caldev.jpg",#file name
  plot = cd,#plot to save, defaults to last plot displayed
  device = "jpg",#file format (jpg, pdf, png, eps, etc)
  path = NULL, #defaults to working directory
  scale = 1,
  width =12,
  height = 10,
  units = "cm", #c("in", "cm", "mm", "px")
  dpi = 300,
  limitsize = TRUE
)  
