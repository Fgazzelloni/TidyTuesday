# title: "#TidyTuesday week13-2022 Sports"
# author: Federica Gazzelloni

library(tidyverse)
options(scipen = 999)

library(hrbrthemes)
library(viridis)

library(showtext)
library(sysfonts)
library(extrafont)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)

font_add_google(name="Noto Sans",family="notosans")

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

df <-sports%>%
  select(year,ef_total_count,total_exp_menwomen,total_rev_menwomen,sports)%>%
  drop_na() 


ggplot(data = df,
       aes(x=(total_exp_menwomen), y=(total_rev_menwomen), 
           size=ef_total_count, fill=sports)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 9), name="Students total count",
             labels = scales::comma_format(scale = 1/100)) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="B") +
  scale_x_log10(expand = c(0,0.2),labels = scales::dollar_format(scale = 1/100))+
  scale_y_log10(expand = c(0,0.2),labels = scales::dollar_format(scale = 1/100))+
  theme_ipsum() +
  theme(text=element_text(family="notosans"),
        legend.position="bottom") +
  labs(title="How profitable can college football be? - USA Facts",
       subtitle = "data from 2015 to 2019 - in thousands of $",
       caption="DataSource: Equity in Athletics Data Analysis | USA Facts | DataViz: Federica Gazzelloni")+
  ylab("Total Revenue") +
  xlab("Total expenditure") +
  theme(legend.position = c(0.2,0.7),
        plot.title = element_text(size=24),
        plot.title.position = "plot",
        plot.background = element_rect(color="grey86",fill="grey86"),
        panel.background = element_rect(color="grey86",fill="grey86"))


ggsave("w13_sports.png",width = 10, height = 8)



