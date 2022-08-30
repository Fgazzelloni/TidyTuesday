# vendors: AMD & Intel CPU vs GPU

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

#############################################
#############################################

new_df_vendor <- rbind(new_cpu_amd,new_cpu_intel,new_gpu_amd,new_gpu_intel)


new_df_vendor %>%
  mutate(year=lubridate::year(date))%>%
  ggplot(aes(x = year, y = mean_transistors_million,
             color=type)) +
  geom_line(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(vendor))




#############################################
#############################################


many_cpu_amd_month <- tibble(.before = 1:12) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_cpu_amd, 
                       data_cpu_amd$release_date, 
                       "month", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="CPU",vendor="AMD")

many_cpu_intel_month <- tibble(.before = 1:12) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_cpu_intel, 
                       data_cpu_intel$release_date, 
                       "month", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="CPU",vendor="Intel")



many_gpu_amd_month <- tibble(.before = 1:12) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_gpu_amd, 
                       data_gpu_amd$release_date, 
                       "month", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="GPU",vendor="AMD")

many_gpu_intel_month <- tibble(.before = 1:12) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_gpu_intel, 
                       data_gpu_intel$release_date, 
                       "month", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="GPU",vendor="Intel")

many_df_vendors_month <- rbind(many_cpu_amd_month,many_cpu_intel_month,
                               many_gpu_amd_month,many_gpu_intel_month)

many_df_vendors_month_1_6 <- many_df_vendors_month%>%
  filter(between(.before,1,6))


many_df_vendors_month_greater_6 <- many_df_vendors_month%>%
  filter(.before>6)

a <- many_df_vendors_month %>%
  filter(.before<=6) %>%
  ggplot(aes(date, mean_transistors_million, 
             group = .before)) +
  geom_line(alpha = 0.6, size = 0.5,color="gray") +
  geom_line(data = many_df_vendors_month_greater_6,
            color = "darkred",
            inherit.aes = T,
            alpha = 0.6, size = 0.5) +
  geom_line(data = many_df_vendors_month_1_6,
            aes(color = .before),
            inherit.aes = T,
            alpha = 0.6, size = 0.5) +
  scale_color_viridis_c() +
  scale_x_date(date_breaks = "2 years",date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, color = "How many months\nin sliding window?")+
  facet_wrap(vars(type,vendor))


a
#############################################
#############################################

many_cpu_amd_year <- tibble(.before = 1:6) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_cpu_amd, 
                       data_cpu_amd$release_date, 
                       "year", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="CPU",vendor="AMD")

many_cpu_intel_year <- tibble(.before = 1:6) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_cpu_intel, 
                       data_cpu_intel$release_date, 
                       "year", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="CPU",vendor="Intel")



many_gpu_amd_year <- tibble(.before = 1:6) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_gpu_amd, 
                       data_gpu_amd$release_date, 
                       "year", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="GPU",vendor="AMD")

many_gpu_intel_year <- tibble(.before = 1:6) %>%
  mutate(mean_transistors_million = map(
    .before, 
    ~ slide_period_dfr(data_gpu_intel, 
                       data_gpu_intel$release_date, 
                       "year", 
                       mean_transistors_million, 
                       .before = .x)
  )) %>%
  unnest(mean_transistors_million) %>%
  mutate(type="GPU",vendor="Intel")

many_df_vendors_year <- rbind(many_cpu_amd_year,many_cpu_intel_year,
                               many_gpu_amd_year,many_gpu_intel_year)



b <- many_df_vendors_year %>%
  #mutate(date=as.factor(date)) %>%
  ggplot(aes(date, mean_transistors_million, 
             group = .before,color=.before)) +
  geom_line(alpha = 0.6, size = 0.5) +
  scale_color_viridis_c() +
  scale_x_date(date_breaks = "2 years",date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, color = "How many years\nin sliding window?")+
  facet_wrap(vars(type,vendor))


b
#############################################
#############################################


library(patchwork)
(a/b) &
  theme_bw()

ggsave("w34_chips.png",
       dpi=320,
       width = 10,
       height = 12)
