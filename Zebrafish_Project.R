library(tidyverse, ggplot2, readxl)
# xlsx files
my_data <- read_excel( file.choose("ZebrafishQuantData.xlsx"))
view(my_data)                      
zebrafish_data<-rename(my_data, total_length = TL, 
              yolk_height = YolkHt, tail_length = "Lttailfromtipmsc(mm)", 
            body_cavity_lenth = BodyCavLen, trunk_total = TrunkTotal, 
            head_depth = HeadDpth, caudal_fin_height = CaudHt, 
            trunk_and_fin = "Trunk+Fin", dorsal_fin_height = DorsalFin, 
            dorsal_fin_length = DorsalFinLen, anal_fin_length = AnalFinLen, eye_height = EyeHt, 
            eye_length = EyeLen, hatch_time_hours = "hatchtime(hours)", average_velocity = VelAvg, 
            max_velocity = VelMax, cue_type = W1A2)
view(zebrafish_data)
ggplot(data = zebrafish_data) + 
  geom_point(mapping = aes(x = total_length, y = yolk_height, color = hatch_time_hours))

             