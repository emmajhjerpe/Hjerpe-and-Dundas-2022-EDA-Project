library(tidyverse, ggplot, readxl)
# xlsx files
my_data <- read_excel( file.choose("ZebrafishQuantData_test.xlsx"))

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
#Comparing Total Length with Yolk Height, added cue and hatch time
ggplot(data = zebrafish_data) + 
  geom_point(mapping = aes(x = total_length, y = yolk_height, color = hatch_time_hours, shape = cue_type))
  


#Comparing Hatch Time and Cue Type
ggplot(data = zebrafish_data) +
  geom_histogram(mapping = aes(x = hatch_time_hours), binwidth = 6)+
  facet_wrap(~ cue_type, scales = "free_y")
#Cue Type and Velocity 
velocity_summary <-
  summarize(
    zebrafish_data, 
    mean_Avel = mean(average_velocity),
    sem = sd(average_velocity) / sqrt(n()),
    ci_upper_limit = mean_Avel + 1.96 * sem,
    ci_lower_limit = mean_Avel - 1.96 * sem, cue_type
  )
ggplot(data = zebrafish_data) +
  geom_jitter(mapping = aes(x = cue_type, y = average_velocity, color = hatch_time_hours))+
  geom_point(
    data = velocity_summary, 
    mapping = aes(x = cue_type, y = mean_Avel, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+
      geom_linerange(
        data = velocity_summary, 
        mapping = aes(x = cue_type, y = mean_Avel, ymax = ci_upper_limit, 
                      ymin = ci_lower_limit),
        color = "red", size=1)

    