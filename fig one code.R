
shade_data<-read.csv("Shading Exp Master.csv")


shade_data$depth_type<-as.character(shade_data$depth_type)

shade_data$hobo_id<-as.factor(shade_data$hobo_id)

shade_data$time<-mdy_hm(shade_data$time, tz = "Asia/Riyadh")

temp_dif_data<-shade_data%>%
  add_column(add_column = "temp_dif")%>%
  mutate(temp_dif = temp - actual_temp)

shade_data_shallow<-shade_data%>%
  filter(depth_type == "shallow" )%>%
  mutate(treat = fct_relevel(treat, 
                             "SBE56", "pvc", "reflective", 
                             "white", "black", "control"))

temp_dif_shallow<-shade_data_shallow%>%
  add_column(add_column = "temp_dif")%>%
  mutate(temp_dif = temp - actual_temp)

data_shallow_day1<-subset(temp_dif_shallow, time >= 	
                            '2023-06-07 06:00:00' & time <= 	
                            '2023-06-07 20:00:00')

data_shallow_day2<-subset(temp_dif_shallow, time >= 	
                            '2023-06-08 06:00:00' & time <= 	
                            '2023-06-08 20:00:00')

shade_data_middle<-shade_data%>%
  filter(depth_type == "middle" )%>%
  mutate(treat = fct_relevel(treat, 
                             "SBE56", "pvc", "reflective", 
                             "white", "black", "control"))

temp_dif_middle<-shade_data_middle%>%
  add_column(add_column = "temp_dif")%>%
  mutate(temp_dif = temp - actual_temp)

data_middle_day1<-subset(temp_dif_middle, time >= 	
                           '2023-06-07 06:00:00' & time <= 	
                           '2023-06-07 20:00:00')

data_middle_day2<-subset(temp_dif_middle, time >= 	
                           '2023-06-08 06:00:00' & time <= 	
                           '2023-06-08 20:00:00')

shallow_midday_data<-shade_data_shallow%>%
  filter( time >= "2023-06-07 10:00:00" & time <= "2023-06-07 16:00:00" | time >= "2023-06-08 10:00:00" & time <= "2023-06-08 16:00:00" )%>%
  filter(treat == "pvc" | treat == "control")

shallow_midday_data_pvc<-shade_data_shallow%>%
  filter( time >= "2023-06-07 10:00:00" & time <= "2023-06-07 16:00:00" | time >= "2023-06-08 10:00:00" & time <= "2023-06-08 16:00:00" )%>%
  filter(treat == "pvc")

shallow_midday_data_control<-shade_data_shallow%>%
  filter( time >= "2023-06-07 10:00:00" & time <= "2023-06-07 16:00:00" | time >= "2023-06-08 10:00:00" & time <= "2023-06-08 16:00:00" )%>%
  filter(treat == "control")

shallow_midday_data_control$pvc_temp<-shallow_midday_data_pvc$temp



middle_midday_data<-shade_data_middle%>%
  filter( time >= "2023-06-07 10:00:00" & time <= "2023-06-07 16:00:00" | time >= "2023-06-08 10:00:00" & time <= "2023-06-08 16:00:00")%>%
  filter(treat == "pvc" | treat == "control")

middle_midday_data_pvc<-shade_data_middle%>%
  filter( time >= "2023-06-07 10:00:00" & time <= "2023-06-07 16:00:00" | time >= "2023-06-08 10:00:00" & time <= "2023-06-08 16:00:00")%>%
  filter(treat == "pvc")

middle_midday_data_control<-shade_data_middle%>%
  filter( time >= "2023-06-07 10:00:00" & time <= "2023-06-07 16:00:00" | time >= "2023-06-08 10:00:00" & time <= "2023-06-08 16:00:00")%>%
  filter(treat == "control")

middle_midday_data_control$pvc_temp<-middle_midday_data_pvc$temp

midday_data<-rbind(shallow_midday_data, middle_midday_data)
midday_data_control<-rbind(shallow_midday_data_control, middle_midday_data_control)



midday_data_control%>%
  add_column(add_column = "temp_offset")%>%
  mutate(temp_offset = temp - pvc_temp)%>%
  filter(time <= '2023-06-07 20:00:00')%>%
  ggplot()+
  geom_density(mapping = aes(x=temp_offset, after_stat(scaled), color=depth_type, fill = depth_type), alpha = 1/10, size = 1.5)+
  theme_classic()+
 # theme_cowplot(font_size = 25)+
  scale_color_manual(values =c("middle" = "#0000FF", "shallow" = "#33FFFF"))+
  scale_fill_manual(values =c("middle" = "#0000FF", "shallow" = "#33FFFF"))  

