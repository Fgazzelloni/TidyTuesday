
# Load packages
library(tidyverse)
library(cowplot)
library(showtext)
showtext_auto()

# Add fonts from Google.
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Special Elite", "Special Elite")

# Set ggplot theme
theme_set(theme_minimal(base_family = "Roboto Mono"))
theme_update(text=element_text(size=14),
  plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 10),
  axis.ticks = element_blank(),
  axis.title.y = element_text(size = 13, margin = margin(r = 10)),
  legend.title = element_text(size = 9),
  plot.caption = element_text(
    family = "Special Elite",
    size = 13,
    color = "grey60",
    face = "bold",
    hjust = .5,
    margin = margin(5, 0, 20, 0)
  ),
  plot.margin = margin(10, 25, 10, 25)
)

# Turn on showtext
showtext_auto()

setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w34_chips")
data_raw <- read_csv("data_raw/chip_dataset.csv")

library(slider)

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
                           .every = 3, 
                           mean_transistors_million) %>%
  mutate(type="CPU")
new_gpu<- slide_period_dfr(data_gpu, 
                           data_gpu$release_date, 
                           "year", 
                           .every = 2, 
                           mean_transistors_million) %>%
  mutate(type="GPU")


new_df <- rbind(new_cpu,new_gpu)


logo <- png::readPNG("logo.png")


title="Twenty years observation of CPU and GPU transistors"
subtitle = "Tendency to increase as stated by the Moore's law confirmed the number of transistors doubles about every two years.
CPU is considered every 3 years while GPU every 2 years. Comparisons between vendors restrict to AMD and Intel."



p <- new_df %>%
  mutate(year=lubridate::year(date),
         type=ifelse(type=="CPU","CPU every 3 Years","GPU every 2 Years")) %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(max= max(mean_transistors_million)) %>%
  ggplot(aes(x = year, y = mean_transistors_million,
             color=type)) +
  geom_line(size = 1.5, alpha = 0.8)+
  geom_point(aes(size=n)) +
  scale_color_manual(
  values = c("#486090", "#D7BFA6"))+
  labs(y="Average n.Transistors (in millions)",
       x="Year",
       color="Type",
       size="Frequency by product",
       title=title,
       subtitle=subtitle,
       caption="DataSource: #TidyTuesday 2022 week 34 Chips | DavaViz: Federica Gazzelloni (@fgazzelloni)")+
  theme(axis.text.x.bottom = element_text(),
        plot.subtitle = element_text(),
        plot.title = element_text(size = 25,face="bold"))


ggdraw(p) +
draw_image(logo, x = -.35, y = -.25, scale = .12)

ggsave("w34_chips.png",
width = 15, height = 9, device = png)

  
