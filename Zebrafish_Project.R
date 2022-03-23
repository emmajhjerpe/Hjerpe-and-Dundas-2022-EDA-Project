library(tidyverse, ggplot, readxl)
# xlsx files
my_data <- read_excel( file.choose("ZebrafishQuantData_test.xlsx"))

zf_data<-rename(my_data, total_length = TL, 
              yolk_height = YolkHt, tail_length = "Lttailfromtipmsc(mm)", 
            body_cavity_lenth = BodyCavLen, trunk_total = TrunkTotal, 
            head_depth = HeadDpth, caudal_fin_height = CaudHt, 
            trunk_and_fin = "Trunk+Fin", dorsal_fin_height = DorsalFin, 
            dorsal_fin_length = DorsalFinLen, anal_fin_length = AnalFinLen, eye_height = EyeHt, 
            eye_length = EyeLen, hatch_time_hours = "hatchtime(hours)", average_velocity = VelAvg, 
            max_velocity = VelMax, cue_type = W1A2)
view(zf_data)
#Summarizing / grouping by cue type for future code/error bars
#For error bars, use code in lines 40-49, change y-variable/mean variable as needed
zf_group<-group_by(zf_data, cue_type)
zf_group

#Code for summaries
 #zf_summary <-
  #summarize(
    #zf_group, 
    #mean_length = mean(variable),
    #sem = sd(variable) / sqrt(n()),
    #ci_upper_limit = mean_variable + 1.96 * sem,
    #ci_lower_limit = mean_variable - 1.96 * sem)
#Yolk Height, added cue and hatch time
zf_summary_yolk <-
  summarize(
    zf_group, 
    mean_yolk = mean(yolk_height),
    sem = sd(yolk_height) / sqrt(n()),
    ci_upper_limit = mean_yolk + 1.96 * sem,
    ci_lower_limit = mean_yolk - 1.96 * sem)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = yolk_height, color = hatch_time_hours))+
  geom_point(
    data = zf_summary_yolk, 
    mapping = aes(x = cue_type, y = mean_yolk, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+
  geom_linerange(
    data = zf_summary_yolk, 
    mapping = aes(x = cue_type, y = mean_yolk, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=1)


#Comparing Hatch Time and Cue Type
ggplot(data = zf_data) +
  geom_histogram(mapping = aes(x = hatch_time_hours), binwidth = 6)+
  facet_wrap(~ cue_type, scales = "free_y")

#average velocity graph code + error bars
zf_summaryvel <-
  summarize(
    zf_group, 
    mean_vel = mean(average_velocity),
    sem = sd(average_velocity) / sqrt(n()),
    ci_upper_limit = mean_vel + 1.96 * sem,
    ci_lower_limit = mean_vel - 1.96 * sem)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = average_velocity, color = hatch_time_hours))+
  geom_point(
    data = zf_summaryvel, 
    mapping = aes(x = cue_type, y = mean_vel, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+
      geom_linerange(
        data = zf_summaryvel, 
        mapping = aes(x = cue_type, y = mean_vel, ymax = ci_upper_limit, 
                      ymin = ci_lower_limit),
        color = "red", size=1)
#Average length comparison
zf_summary_tl <-
  summarize(
    zf_group, 
    mean_length = mean(total_length),
    sem = sd(total_length) / sqrt(n()),
    ci_upper_limit = mean_length + 1.96 * sem,
    ci_lower_limit = mean_length - 1.96 * sem)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = total_length, color = hatch_time_hours))+
  geom_point(
    data = zf_summary_tl, 
    mapping = aes(x = cue_type, y = mean_length, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+
  geom_linerange(
    data = zf_summary_tl, 
    mapping = aes(x = cue_type, y = mean_length, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=1)

    