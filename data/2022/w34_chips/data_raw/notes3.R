
setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w34_chips")
data_raw <- read_csv("data_raw/chip_dataset.csv")

data_raw%>%
  janitor::clean_names()%>%
  mutate(release_date=as.Date(release_date,"%Y-%m-%d"),
         year=lubridate::year(release_date),.after=release_date) %>%
  filter(!is.na(year)) %>%
  #select(release_date,year,type,transistors_million,freq_m_hz) %>%
  group_by(year) %>%
  mutate(transistors_million=ifelse(is.na(transistors_million),
                                    mean(transistors_million,na.rm = T),
                                    transistors_million)) %>%
  ungroup() %>%
  arrange(release_date) 


#############################################
#############################################



data_raw_1 <- data_raw%>%
  janitor::clean_names()%>%
  mutate(release_date=as.Date(release_date,"%Y-%m-%d"),
         year=lubridate::year(release_date),.after=release_date) %>%
  filter(!is.na(year),
         vendor%in%c("AMD","Intel")) %>%
  select(release_date,year,type,vendor,product,transistors_million,freq_m_hz) %>%
  group_by(year) %>%
  mutate(transistors_million=ifelse(is.na(transistors_million),
                                    mean(transistors_million,na.rm = T),
                                    transistors_million)) %>%
  ungroup() %>%
  arrange(release_date)
  

data_raw_1 %>%
  ggplot(aes(x=year)) +
  geom_jitter(aes(y=transistors_million,color=type,alpha=freq_m_hz)) +
  scale_y_log10() +
  ggthemes::scale_color_tableau()+
  facet_wrap(vars(vendor))

#############################################
#############################################

library(slider)
slide_period_dbl(data_raw_1,
                 data_raw_1$release_date,
                 .period="year",
                 .every = 2, 
                 ~ mean(.$transistors_million))

mean_transistors_million <- function(df) {
  summarize(df, 
            date = min(release_date), 
            mean_transistors_million = mean(transistors_million), 
            n = n())
}


data_cpu <- data_raw_1 %>%
  filter(type=="CPU")
data_gpu <- data_raw_1 %>%
  filter(type=="GPU")


new_cpu<- slide_period_dfr(data_cpu, 
                           data_cpu$release_date, 
                           .period="year", 
                 mean_transistors_million) %>%
  mutate(type="CPU")
new_gpu<- slide_period_dfr(data_gpu, 
                           data_gpu$release_date, 
                           "year", 
                           mean_transistors_million) %>%
  mutate(type="GPU")


new_df <- rbind(new_cpu,new_gpu)


new_df %>%
  mutate(year=lubridate::year(date))%>%
  ggplot(aes(x = year, y = mean_transistors_million,
             color=type)) +
  geom_line(size = 1.5, alpha = 0.8)


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


new_cpu_amd<- slide_period_dfr(data_cpu_amd, 
                               data_cpu_amd$release_date, 
                               "year", 
                               mean_transistors_million) %>%
  mutate(type="CPU",vendor="AMD")
new_cpu_intel<- slide_period_dfr(data_cpu_intel, 
                                 data_cpu_intel$release_date, 
                                 "year", 
                                 mean_transistors_million) %>%
  mutate(type="CPU",vendor="Intel")

new_gpu_amd<- slide_period_dfr(data_gpu_amd, 
                               data_gpu_amd$release_date, 
                               "year", 
                               mean_transistors_million) %>%
  mutate(type="GPU",vendor="AMD")
new_gpu_intel<- slide_period_dfr(data_gpu_intel, 
                                 data_gpu_intel$release_date, 
                                 "year", 
                                 mean_transistors_million) %>%
  mutate(type="GPU",vendor="Intel")


new_df_vendor <- rbind(new_cpu_amd,new_cpu_intel,new_gpu_amd,new_gpu_intel)


new_df_vendor %>%
  mutate(year=lubridate::year(date))%>%
  ggplot(aes(x = year, y = mean_transistors_million,
             color=type)) +
  geom_line(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(vendor))

#############################################
#############################################


new_cpu_month<- slide_period_dfr(data_cpu, 
                           data_cpu$release_date, 
                           "month", 
                           mean_transistors_million) %>%
  mutate(type="CPU")
new_gpu_month<- slide_period_dfr(data_gpu, 
                           data_gpu$release_date, 
                           "month", 
                           mean_transistors_million) %>%
  mutate(type="GPU")


new_df_month <- rbind(new_cpu_month,new_gpu_month)


new_df_month %>%
  mutate(year=lubridate::year(date))%>%
  ggplot(aes(x = date, y = mean_transistors_million,
             color=type)) +
  geom_line(size = 1.5, alpha = 0.8)


#############################################
#############################################



many_cpu_month <- tibble(.before = 1:24) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_cpu, data_cpu$release_date, "month", mean_transistors_million, .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="CPU")
many_gpu_month <- tibble(.before = 1:24) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_gpu, data_gpu$release_date, "month", mean_transistors_million, .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="GPU")

many_df_month <- rbind(many_cpu_month,many_gpu_month)

many_df_month_1_6 <- many_df_month%>%
  filter(between(.before,1,6))


many_df_month_greater_20 <- many_df_month%>%
  filter(.before>20)

a <- many_df_month %>%
  filter(.before<=20) %>%
  ggplot(aes(date, mean_transistors_million, 
             group = .before)) +
  geom_line(alpha = 0.6, size = 0.5,color="gray") +
  geom_line(data = many_df_month_greater_20,
            color = "darkred",
            inherit.aes = T,
            alpha = 0.6, size = 0.5) +
  geom_line(data = many_df_month_1_6,
            aes(color = .before),
            inherit.aes = T,
            alpha = 0.6, size = 0.5) +
  scale_color_viridis_c() +
  scale_x_date(date_breaks = "2 years",date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, color = "How many months\nin sliding window?")+
  facet_wrap(~type)


#############################################
#############################################



many_cpu_year <- tibble(.before = 1:6) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_cpu, data_cpu$release_date,
                       "year", mean_transistors_million,
                       .before = .x))) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="CPU")
many_gpu_year <- tibble(.before = 1:6) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_gpu, data_gpu$release_date, 
                       "year", mean_transistors_million, 
                       .before = .x))) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="GPU")

many_df_year <- rbind(many_cpu_year,many_gpu_year)



b <- many_df_year %>%
  #mutate(date=as.factor(date)) %>%
  ggplot(aes(date, mean_transistors_million, 
             group = .before,color=.before)) +
  geom_line(alpha = 0.6, size = 0.5) +
  scale_color_viridis_c() +
  scale_x_date(date_breaks = "2 years",date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, color = "How many years\nin sliding window?")+
  facet_wrap(~type)



#############################################
#############################################


library(patchwork)
(a/b) &
  theme_bw()


#############################################
#############################################


label = "Moore's law is the observation that the number of transistors\nin a dense integrated circuit (IC) doubles about every two years"

# GPU stand for Graphics processing unit, designed to accelerate graphics rendering
# CPU stand for Central processing unit, electronic circuitry that executes instructions comprising a computer program (calculates and interprets instructions).
# composed of the main memory, control unit, and arithmetic-logic unit.
# AMD: Advanced Micro Devices
# Intel



