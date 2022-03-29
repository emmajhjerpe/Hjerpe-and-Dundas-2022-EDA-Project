library(tidyverse, ggplot, readxl)
library(wesanderson)
# xlsx files
my_data <- read_excel( file.choose("ZebrafishQuantData_test.xlsx"))
#Color Range
colors_new<-colorRampPalette(c("blue", "red"))
#Renaming
zf_data<-rename(my_data, total_length = TL, 
              yolk_height = YolkHt, tail_length = "Lttailfromtipmsc(mm)", 
            body_cavity_lenth = BodyCavLen, trunk_total = TrunkTotal, 
            head_depth = HeadDpth, caudal_fin_height = CaudHt, 
            trunk_and_fin = "Trunk+Fin", dorsal_fin_height = DorsalFin, 
            dorsal_fin_length = DorsalFinLen, anal_fin_length = AnalFinLen, eye_height = EyeHt, 
            eye_length = EyeLen, hatch_time_hours = "hatchtime(hours)", average_velocity = VelAvg, 
            max_velocity = VelMax, cue_type = W1A2)
view(zf_data)
summary(zf_data)
length(zf_data$max_velocity)
#Summarizing / grouping by cue type for future code/error bars

zf_group<-group_by(zf_data, cue_type)
zf_group
#Calculating T score
alpha = 0.05
degrees.freedom = length(zf_data) - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

#Error is t 
#Yolk Height, added cue and hatch time
zf_summary_yolk <-
  summarize(
    zf_group, 
    mean_yolk = mean(yolk_height),
    sem_yolk = sd(yolk_height) / sqrt(n()),
    ci_upper_limit = mean_yolk + t.score * sem_yolk,
    ci_lower_limit = mean_yolk - t.score * sem_yolk)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = yolk_height, color = hatch_time_hours), width = 0.3)+
  geom_point(
    data = zf_summary_yolk, 
    mapping = aes(x = cue_type, y = mean_yolk, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+ scale_color_gradient(low = "blue", high = "green")+
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
    sem_vel = sd(average_velocity) / sqrt(n()),
    ci_upper_limit = mean_vel + t.score * sem_vel,
    ci_lower_limit = mean_vel - t.score * sem_vel)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = average_velocity, color = hatch_time_hours), width = 0.3)+
  geom_point(
    data = zf_summaryvel, 
    mapping = aes(x = cue_type, y = mean_vel, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+scale_color_gradient(low = "blue", high = "green")+
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
    sem_length = sd(total_length) / sqrt(n()),
    ci_upper_limit = mean_length + t.score * sem_length,
    ci_lower_limit = mean_length - t.score * sem_length)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = total_length, color = hatch_time_hours), width = 0.3)+
  geom_point(
    data = zf_summary_tl, 
    mapping = aes(x = cue_type, y = mean_length, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+scale_color_gradient(low = "blue", high = "green")+
  geom_linerange(
    data = zf_summary_tl, 
    mapping = aes(x = cue_type, y = mean_length, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=1)
#Max Velocity
zf_summary_vmax <-
  summarize(
    zf_group, 
    mean_vmax = mean(max_velocity),
    sem_vmax = sd(max_velocity) / sqrt(n()),
    ci_upper_limit = mean_vmax + t.score * sem_vmax,
    ci_lower_limit = mean_vmax - t.score * sem_vmax)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = max_velocity, color = hatch_time_hours),
              width = 0.3, size=2)+
  geom_point(
    data = zf_summary_vmax, 
    mapping = aes(x = cue_type, y = mean_vmax, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=3)+scale_color_gradient(low = "blue", high = "green")+
  geom_linerange(
    data = zf_summary_vmax, 
    mapping = aes(x = cue_type, y = mean_vmax, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=2)+
  theme_gray(base_size = 24)
ggsave("maxvel_graph.png",height=8,width=12, units = "in", dpi = 400)
    