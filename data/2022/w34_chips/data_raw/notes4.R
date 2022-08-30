setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w34_chips")
library(tidyverse)
library(slider)
data_raw <- read_csv("data_raw/chip_dataset.csv")

#############################################
#############################################

data_raw_1 <- data_raw%>%
  janitor::clean_names()%>%
  mutate(release_date=as.Date(release_date,"%Y-%m-%d"),
         year=lubridate::year(release_date),.after=release_date) %>%
  filter(!is.na(year),
         year>2010,
         vendor%in%c("AMD","Intel")) %>%
  select(release_date,year,type,vendor,product,transistors_million,freq_m_hz) %>%
  group_by(year) %>%
  mutate(transistors_million=ifelse(is.na(transistors_million),
                                    mean(transistors_million,na.rm = T),
                                    transistors_million)) %>%
  ungroup() %>%
  arrange(release_date)


#############################################
#############################################

data_cpu <- data_raw_1 %>%
  filter(type=="CPU")
data_gpu <- data_raw_1 %>%
  filter(type=="GPU")

#############################################
#############################################

data_cpu_amd <- data_cpu %>%
  filter(vendor=="AMD")
data_cpu_intel <- data_cpu %>%
  filter(vendor=="Intel")
data_gpu_amd <- data_gpu %>%
  filter(vendor=="AMD")
data_gpu_intel <- data_gpu %>%
  filter(vendor=="Intel")


#############################################
mean_transistors_million <- function(df) {
  summarize(df, 
            date = min(release_date), 
            mean_transistors_million = mean(transistors_million), 
            n = n())
}
#############################################

new_cpu_amd<- slide_period_dfr(data_cpu_amd, 
                               data_cpu_amd$release_date, 
                               "year", 
                               #.every=2,
                               #.before=1,
                               mean_transistors_million) %>%
  mutate(type="CPU",vendor="AMD")
new_cpu_intel<- slide_period_dfr(data_cpu_intel, 
                                 data_cpu_intel$release_date, 
                                 "year", 
                                 #.every=2,
                                 #.before=1,
                                 mean_transistors_million) %>%
  mutate(type="CPU",vendor="Intel")

new_gpu_amd<- slide_period_dfr(data_gpu_amd, 
                               data_gpu_amd$release_date, 
                               "year", 
                               #.every=2,
                               #.before=1,
                               mean_transistors_million) %>%
  mutate(type="GPU",vendor="AMD")
new_gpu_intel<- slide_period_dfr(data_gpu_intel, 
                                 data_gpu_intel$release_date, 
                                 "year", 
                                 #.every=2,
                                 #.before=1,
                                 mean_transistors_million) %>%
  mutate(type="GPU",vendor="Intel")

#############################################
#############################################

new_df_vendor <- rbind(new_cpu_amd,new_cpu_intel,new_gpu_amd,new_gpu_intel)


new_df_vendor %>%
  mutate(year=lubridate::year(date))%>%
  ggplot(aes(x = year, y = mean_transistors_million,
             color=type)) +
  geom_point(data = data_raw_1%>%
               group_by(year)%>%
               mutate(max=max(transistors_million))%>%
               ungroup(),
             aes(y=max),
             inherit.aes = T,
             size=0.5,alpha=0.5)+
  geom_point(data = data_raw_1%>%
               group_by(year)%>%
               mutate(min=min(transistors_million))%>%
               ungroup(),
             aes(y=min),
             inherit.aes = T,
             size=0.5,alpha=0.5)+
  geom_segment(data = data_raw_1%>%
                 group_by(year)%>%
                 mutate(min=min(transistors_million),
                        max=max(transistors_million),
                        med=mean(transistors_million))%>%
                 ungroup(),
               aes(x = year,xend = year,
                   y = min,yend = med),
               inherit.aes = T) +
  geom_segment(data = data_raw_1%>%
                 group_by(year)%>%
                 mutate(min=min(transistors_million),
                        max=max(transistors_million),
                        med=mean(transistors_million))%>%
                 ungroup(),
               aes(x = year,xend = year,
                   y = med,yend = max),
               inherit.aes = T) +
  geom_line(size = 1, alpha = 0.8)+
  #geom_smooth(size=0.5)+
  facet_wrap(vars(vendor))

