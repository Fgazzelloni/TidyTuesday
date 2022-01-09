# Tidytuesday week 28--------- & future day28 --------------
# inspired by http://applied-r.com/plotting-forecast-data-objects-ggplot/


library(tidyverse)
library(tidytuesdayR)
library(DataExplorer)
library(lubridate)
library(tsibble)
library(ggrepel)
library(corrplot)
library(forecast)
library(patchwork)
library(cowplot)
library(ragg)
library(RColorBrewer)


# load data ###############################
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# check and wrangling #####################
head(departures)
glimpse(departures)
profile_missing(departures)


df <- departures%>%
  select(2,4,6,7,8,11,12,13)%>%
  arrange(leftofc)%>%
  mutate(diff=fyear_gone-fyear)%>%
  drop_na()%>%
  select(coname,exec_fullname,fyear,fyear_gone,diff,leftofc,departure_code,ceo_dismissal,max_tenure_ceodb)%>%
  filter(abs(diff)<6)%>%filter(!diff<0)

range(df$fyear_gone)

# first plot to see the pattern ###############
# excluded raws with more than 6 years difference between fyear and fyear_gone
ggplot(df) + 
  geom_point(aes(x=factor(fyear),y=factor(fyear_gone),color=ifelse(abs(diff)<6 ,"in bound","out of bound")) )+
  labs(color="")+
  theme(axis.text.x = element_text(angle=90))
  
# selecting data for main plot ############################
tot_dismissal <- df %>%
  mutate(y_month=yearmonth(leftofc),month=month(leftofc))%>%
  group_by(y_month,month)%>%
  summarize(tot_dism=sum(ceo_dismissal),tot_posit=sum(max_tenure_ceodb))%>%
  ungroup()


# set the theme and modifications as it is needed --------------------
library(ggthemes)

# theme for forecast data objects
theme.fxdat <- theme_gdocs() +
  theme(plot.title = element_text(size = 25,color="grey45"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "grey50"),
        axis.title.y = element_text(face = "bold", color = "gray30"),
        axis.title.x = element_text(face = "bold", color = "gray30", vjust = -1),
        axis.text.x = element_text(angle=90),
        panel.background = element_rect(fill = "grey95", colour = "grey75"),
        panel.border = element_rect(colour = "grey75"),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.minor.y = element_line(colour = "white", linetype = "dotted"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.minor.x = element_line(colour = "white", linetype = "dotted"),
        strip.background = element_rect(size = 1, fill = "white", colour = "grey75"),
        strip.text.y = element_text(face = "bold"),
        axis.line = element_line(colour = "grey75"),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "bottom")

tot_dismissal$y_month[80]
tot_dismissal$tot_dism+rnorm(tot_dismissal$tot_dism)[1]

# plotting ########################################
library(zoo)
set.seed(345)
plot1 <- ggplot(tot_dismissal,aes(x=y_month,y=tot_dism+rnorm(tot_dism))) + 
  geom_point(aes(color="Number of dismissal"),fill="black")+
  geom_line(aes(color="Dismissal Trend"),size=0.2) +
  geom_smooth(aes(color="Smoothed conditional means"))+
  geom_line(aes(y_month,rollmedian(tot_dism, k = 15, fill = NA, align = "center"),color="Rolling median"),size=0.8)+
  labs(x="Time(Year-Month)",
       y="Normalized total n. of dismissal",
       title="33 Years trend CEO Departures",
       subtitle="1988 - 2021\n Rolling median, smooth variation line and trend",
       caption="",
       color=""
       )+
  scale_color_brewer(palette = "Dark2")+
  theme.fxdat 



# future ----------------------------
x<-tot_dismissal$tot_dism

# create time series data object (ts) using tot_dism
res.gen <- ts(x, frequency = 12, start = c(1992, 6))

xx<-tot_dismissal%>%
  pivot_wider(names_from="month",values_from="tot_dism",values_fill=0)%>%select(-tot_posit,-y_month)

########################
fit.y <- tslm(res.gen ~ trend + season)
fx.y <- forecast(fit.y, h = 17, level = c(80, 95, 99))


source("plot_fx.R")

plot2 <- plot_fx(fx.y,
        PI = TRUE,
        line.cols = NA,
        shade.cols = NA,
        show.gap = TRUE,
        date.breaks = "15 months",
        date.format = "%b-%y",
        main.title = "CEO Departures forecast",
        sub.title = "Transformed 33 years trend in Linear trend with seasonal dummy variables",
        caption = "Viz. @fgazzelloni | DataSource: Gentry et al. & investors.com | TidyTuesday week18",
        x.title = "CEO Departures by Year and Month",
        y.title = "Total numbers of CEO Departures")


final<-plot1+plot2



####### SAVING ######################################
ragg::agg_png(here::here("w18", "tidytuesday_Departures.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()



#### ATTACHING LOGO ############################ 
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("W18/tidytuesday_Departures.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "tidytuesday_Departures.png", format = "png") # save final plot






