library(RColorBrewer)
library(dplyr)
library(ggplot2)
par(mar=c(3,4,2,2))
display.brewer.all()
display.brewer.pal(name="Dark2",n=4)

brewer.pal(4, "Dark2")

ls<-read.csv('ls.csv')

ls$dummy<-1

ls1<-ls %>% 
  # Base the removal on the "Age" column
  distinct(DOI, .keep_all = TRUE)



lsscy<-ls1%>%filter(Calibrated=="Yes" & Shaded=="Yes")
lsscy$grp<-"Both"
lss<-ls1%>%filter(Calibrated=="Not mentioned" & Shaded=="Yes")
lss$grp<-"Shaded"
lsc<-ls1%>%filter(Calibrated=="Yes" & Shaded=="Not mentioned")
lsc$grp<-"Calibrated"
lsscn<-ls1%>%filter(Calibrated=="Not mentioned" & Shaded=="Not mentioned")
lsscn$grp<-"Neither"

lsc<-bind_rows(lsscy,lss,lsc,lsscn) 

lsc1 <- lsc %>%
  group_by(grp) %>%
  summarise(count=sum(dummy))

lsca<-lsc1%>%
  mutate(grp = factor(grp, levels=c("Both","Shaded","Calibrated","Neither"))) %>%
  ggplot(aes(x=grp, y=count)) +
  theme_classic()+
  xlab("")+
  ylab("Number of Studies")+
  ylim(0,329)+
  scale_x_discrete(labels=c("Both"=paste0("Both","\n", "S+C"),"Shaded"=paste0("Shaded","\n", "S"),"Calibrated"=paste0("Calibrated","\n", "C"),"Neither"=paste0("Neither","\n", "S or C")))+
  geom_rect( xmin=0.7,xmax=1.3,ymin=0,ymax=4, color="#1B9E77", fill="#1B9E77") +
  geom_rect( xmin=0.7,xmax=1.3,ymin=6,ymax=10, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=12,ymax=16, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=18,ymax=22, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=24,ymax=28, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=30,ymax=34, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=36,ymax=40, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=42,ymax=46, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=48,ymax=52, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=54,ymax=58, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=60,ymax=64, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=66,ymax=70, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=72,ymax=76, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=78,ymax=82, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=84,ymax=88, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=90,ymax=94, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=96,ymax=100, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=102,ymax=106, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=108,ymax=112, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=114,ymax=118, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=120,ymax=124, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=126,ymax=130, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=132,ymax=136, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=138,ymax=142, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=144,ymax=148, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=150,ymax=154, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=156,ymax=160, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=162,ymax=166, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=168,ymax=172, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=174,ymax=178, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=180,ymax=184, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=186,ymax=190, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=192,ymax=196, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=198,ymax=202, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=204,ymax=208, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=210,ymax=214, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=216,ymax=220, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=222,ymax=226, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=228,ymax=232, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=234,ymax=238, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=240,ymax=244, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=246,ymax=250, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=252,ymax=256, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=258,ymax=262, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=264,ymax=268, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=270,ymax=274, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=276,ymax=280, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=282,ymax=286, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=288,ymax=292, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=294,ymax=298, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=300,ymax=304, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=306,ymax=310, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=312,ymax=316, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=318,ymax=322, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=0.7,xmax=1.3,ymin=324,ymax=328, color="white", fill="darkgrey",alpha=0.5) +
  
  
  
  geom_rect( xmin=1.7,xmax=2.3,ymin=0,ymax=4, color="white", fill="#D95F02" ) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=6,ymax=10, color="white",fill="#D95F02" ) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=12,ymax=16, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=18,ymax=22, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=24,ymax=28, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=30,ymax=34, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=36,ymax=40, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=42,ymax=46, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=48,ymax=52, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=54,ymax=58, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=60,ymax=64, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=66,ymax=70, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=72,ymax=76, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=78,ymax=82, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=84,ymax=88, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=90,ymax=94, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=96,ymax=100, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=102,ymax=106, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=108,ymax=112, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=114,ymax=118, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=120,ymax=124, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=126,ymax=130, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=132,ymax=136, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=138,ymax=142, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=144,ymax=148, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=150,ymax=154, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=156,ymax=160, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=162,ymax=166, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=168,ymax=172, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=174,ymax=178, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=180,ymax=184, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=186,ymax=190, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=192,ymax=196, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=198,ymax=202, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=204,ymax=208, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=210,ymax=214, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=216,ymax=220, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=222,ymax=226, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=228,ymax=232, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=234,ymax=238, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=240,ymax=244, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=246,ymax=250, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=252,ymax=256, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=258,ymax=262, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=264,ymax=268, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=270,ymax=274, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=276,ymax=280, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=282,ymax=286, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=288,ymax=292, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=294,ymax=298, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=300,ymax=304, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=306,ymax=310, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=312,ymax=316, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=318,ymax=322, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=1.7,xmax=2.3,ymin=324,ymax=328, color="white", fill="darkgrey",alpha=0.5) +
  
  
  
  geom_rect( xmin=2.7,xmax=3.3,ymin=0,ymax=4, color="white", fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=6,ymax=10, color="white", fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=12,ymax=16, color="white",fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=18,ymax=22, color="white",fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=24,ymax=28, color="white",fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=30,ymax=34, color="white",fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=36,ymax=40, color="white",fill="darkorchid") +
  geom_rect( xmin=2.7,xmax=3.3,ymin=42,ymax=46, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=48,ymax=52, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=54,ymax=58, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=60,ymax=64, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=66,ymax=70, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=72,ymax=76, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=78,ymax=82, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=84,ymax=88, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=90,ymax=94, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=96,ymax=100, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=102,ymax=106, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=108,ymax=112, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=114,ymax=118, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=120,ymax=124, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=126,ymax=130, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=132,ymax=136, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=138,ymax=142, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=144,ymax=148, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=150,ymax=154, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=156,ymax=160, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=162,ymax=166, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=168,ymax=172, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=174,ymax=178, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=180,ymax=184, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=186,ymax=190, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=192,ymax=196, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=198,ymax=202, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=204,ymax=208, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=210,ymax=214, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=216,ymax=220, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=222,ymax=226, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=228,ymax=232, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=234,ymax=238, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=240,ymax=244, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=246,ymax=250, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=252,ymax=256, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=258,ymax=262, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=264,ymax=268, color="white",fill="darkgrey", alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=270,ymax=274, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=276,ymax=280, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=282,ymax=286, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=288,ymax=292, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=294,ymax=298, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=300,ymax=304, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=306,ymax=310, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=312,ymax=316, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=318,ymax=322, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=2.7,xmax=3.3,ymin=324,ymax=328, color="white", fill="darkgrey",alpha=0.5) +
  
  geom_rect( xmin=3.7,xmax=4.3,ymin=0,ymax=4, color="white", fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=6,ymax=10, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=12,ymax=16, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=18,ymax=22, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=24,ymax=28, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=30,ymax=34, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=36,ymax=40, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=42,ymax=46, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=48,ymax=52, color="white",fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=54,ymax=58, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=60,ymax=64, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=66,ymax=70, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=72,ymax=76, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=78,ymax=82, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=84,ymax=88, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=90,ymax=94, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=96,ymax=100, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=102,ymax=106, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=108,ymax=112, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=114,ymax=118, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=120,ymax=124, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=126,ymax=130, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=132,ymax=136, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=138,ymax=142, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=144,ymax=148, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=150,ymax=154, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=156,ymax=160, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=162,ymax=166, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=168,ymax=172, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=174,ymax=178, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=180,ymax=184, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=186,ymax=190, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=192,ymax=196, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=198,ymax=202, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=204,ymax=208, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=210,ymax=214, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=216,ymax=220, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=222,ymax=226, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=228,ymax=232, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=234,ymax=238, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=240,ymax=244, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=246,ymax=250, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=252,ymax=256, color="white", fill= "#E7298A",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=258,ymax=262, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=264,ymax=268, color="white",fill= "#E7298A", alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=270,ymax=274, color="white",fill= "#E7298A") +
  geom_rect( xmin=3.7,xmax=4.3,ymin=276,ymax=280, color="white", fill="darkgrey",alpha=0.8) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=282,ymax=286, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=288,ymax=292, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=294,ymax=298, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=300,ymax=304, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=306,ymax=310, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=312,ymax=316, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=318,ymax=322, color="white", fill="darkgrey",alpha=0.5) +
  geom_rect( xmin=3.7,xmax=4.3,ymin=324,ymax=328, color="white", fill="darkgrey",alpha=0.5) 

ggsave(
  "shadcalitotals.png",#file name
  plot = lsca,#plot to save, defaults to last plot displayed
  device = "png",#file format (jpg, pdf, png, eps, etc)
  path = NULL, #defaults to working directory
  scale = 1,
  width =10,
  height = 8,
  units = "cm", #c("in", "cm", "mm", "px")
  dpi = 300,
  bg = "transparent",
  limitsize = TRUE)
