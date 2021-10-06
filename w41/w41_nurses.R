# TidyTuesday week 41 Nurses

# load libraries
library(ggExtra)
library(xkcd)
library(ggstatsplot)
library(extrafont)
library(extrafont)
fonts()

options(scipen = 999)
library(tidyverse)


nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')


df <- nurses %>%
  janitor::clean_names() %>%
  filter(!is.na(employed_standard_error_percent),!is.na(wage_salary_standard_error_percent))

my_df <- df %>%
  mutate(salary_level = case_when(annual_salary_median < 35000 ~ "low",
                                 annual_salary_median >= 35000 & annual_salary_median < 55000 ~ "medium",
                                 annual_salary_median >= 55000 & annual_salary_median <= 80000 ~ "high",
                                 TRUE ~ "top high"),
         .after = year) %>%
  mutate(salary_level_class = case_when(annual_salary_median < 35000 ~ "<35",
                                       annual_salary_median >= 35000 & annual_salary_median < 55000 ~ "35 to 55",
                                       annual_salary_median >= 55000 & annual_salary_median <= 80000 ~ "56 to 80",
                                       TRUE ~ "<80"),
         .after = salary_level) %>%
  mutate(salary_leg = paste(salary_level,"-",salary_level_class),
         .after = salary_level,
         salary_leg = as.factor(salary_leg))

unique(my_df$salary_leg)
legend_ord <- levels(with(my_df, reorder(annual_salary_median,year)))
my_df$salary_level <- factor(my_df$salary_level, # Relevel group factor
                             levels = c("low","medium","high","top high"))


theme_nurses <- xkcd::theme_xkcd() +
  theme(text = element_text(color = "grey80",family = "Comic Sans MS"),
        plot.title = element_text(hjust = 0.5,size = 22,face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(family = "Comic Sans MS"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 11,family = "Comic Sans MS",face = "bold"),
        legend.text = element_text(size = 9,family = "Comic Sans MS"),
        axis.ticks = element_line(size = 2,color = "orange"),
        axis.line.x = element_line(color = "grey80"),
        axis.title.x = element_text(size = 12,face = "bold"),
        axis.title.y = element_text(size = 12,face = "bold"),
        axis.text.y = element_text(color = "grey80"),
        axis.text.x = element_text(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.background = element_rect(color = "grey40",fill = "grey40"),
        plot.background = element_rect(color = "grey40",fill = "grey40"))


stair_sal <- my_df %>%
  ggplot(aes(factor(year), y = annual_salary_median, group = salary_level, fill = salary_level)) +
  geom_col(width = 1) +
  labs(fill = "Salary level",
       x = "Years",y = "Cumulate annual median salaries by salary level",
       title = "Did the Nurses Annual Salary changed since 1998?",
       subtitle = " ") +
  scale_x_discrete(breaks = seq(1998,2020,3)) +
  scale_y_continuous(labels = scales::number_format(scale = 1/1000000,suffix = "M",accuracy = 1)) +
  scale_fill_manual(limits = c("low", "medium","high","top high"),
                    labels = c("high" = "high from 56,000 to 80,000",
                             "low" = "low less than 35,000",
                             "medium" = "medium from 35,000 to 55,000",
                             "top high" = "top high greater than 80,000"),
                    values = RColorBrewer::brewer.pal(4,"Spectral")) +
  guides(fill = guide_legend(ncol = 4,title.position = "left")) +
  theme_nurses +
  theme(legend.position = c(0.5,1)) +
  annotate("text", x = 5, y = 1100000,label = "Medium Salary from 35,000 to 60,000",family = "xkcd" ) +
  annotate("text", x = 20, y = 300000,label = "Top High up to 118,500",family = "xkcd" ) +
  annotate("text", x = 20, y = 160000,label = "started in 2008",family = "xkcd" ) +
  annotate("text", x = 16, y = 2000000,label = "High Salaries between 56,000 and 80,000",family = "xkcd" ) +
  annotate("curve", x = 3, xend = 5, y = 3600000, yend = 2600000,color = "grey85", curvature = 0.5,
           arrow = arrow(angle = 30, length = unit(0.2, "inches"),ends = "last", type = "open")) +
  annotate("text", x = 4, y = 3700000,label = "Low salaries below 30,000",family = "xkcd" ,color = "red") +
  annotate("text", x = 8, y = 3500000,label = "stopped in 2011\nto reach 35,000",family = "xkcd",color = "red" ) +
  annotate("curve", x = 9, xend = 13, y = 3500000, yend = 3400000,color = "grey85", curvature = -0.5,
         arrow = arrow(angle = 30, length = unit(0.2, "inches"),ends = "last", type = "open"))



salaries_by_year <- my_df %>%
  group_by(year,salary_level) %>%
  summarize(total = sum(annual_salary_median),.groups = "drop") %>%
  pivot_wider(names_from = year,values_from = total,values_fill = 0)

facet_sal <- salaries_by_year %>%
  pivot_longer(cols = "1998":"2020",names_to = "years",values_to = "tot salary by level") %>%
  ggplot(aes(`tot salary by level`,salary_level,group = years)) +
  geom_col(aes(fill = salary_level)) +
  labs(x = "Total amount of annual median salaries by level",
       y = "Salary levels - $10thous.",
       fill = "Salary level",
       caption = "(values are in $,10thous.,M = millions of $)\nDataSource: Registered Nurses,DataWorld,BLS\nTidyTuesday Week41 DataViz: Federica Gazzelloni") +
  scale_x_continuous(labels = scales::number_format(scale = 1/100000,accuracy = 1)) +
  scale_fill_manual(limits = c("low", "medium","high","top high"),
                    labels = c("high" = "high from 56,000 to 80,000",
                             "low" = "low less than 35,000",
                             "medium" = "medium from 35,000 to 55,000",
                             "top high" = "top high greater than 80,000"),
                    values = RColorBrewer::brewer.pal(4,"Spectral")) +
  guides(fill = guide_legend(ncol = 2,title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~years,scale = "free_x") +
  theme_nurses +
  theme(legend.position = c(0.79,0.07),
        strip.background = element_rect(color = "grey40",fill = "grey40"),
        strip.text = element_text(color = "grey80",face = "bold"))




# save final plot
ragg::agg_png(here::here("/Users/federica/Documents/R/R_general_resourses/TidyTuesday/TidyTuesday/w41/w41_nurses.png"),
              res = 320, width = 12, height = 14, units = "in")
stair_sal/facet_sal
dev.off()



#----Tidytuesday logo----
# read the image, attach the Tidytuesday logo and save it
library(magick)
tidy_logo <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")

tidy_final <- image_read("/Users/federica/Documents/R/R_general_resourses/TidyTuesday/TidyTuesday/w41/w41_nurses.png")
attached_logo <- image_composite(tidy_final, tidy_logo,
                                 operator = "atop",
                                 gravity = "southwest")

image_write(attached_logo, path = "w41_nurses.png", format = "png")


