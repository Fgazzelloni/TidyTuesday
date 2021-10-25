# final week 43 Pumpkins

rm(list=ls())

# Libraries-----
library(tidyverse)
library(extrafont)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable=T)
font_add_google("Eater","Eater")


# Data-----
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')


# Data wrangling-------
df <- pumpkins%>% #
  filter(!str_detect(state_prov,"Entries")) %>%
  filter(!country=="Unknown country") %>%
  separate(id,into=c("year","type")) %>%
  mutate(#place=as.integer(place),
    year=as.integer(year),
    pct_chart=as.double(pct_chart))%>%
  mutate(type = factor(type,labels=c("F"="Field Pumpkins",
                                     "L"="Long Gourds",
                                     "P"="Pumpkins",
                                     "S"="Squash",
                                     "T"="Tomato",
                                     "W"="Watermelon"))) %>%
  count(place,year,type,country,weight_lbs,ott,est_weight,pct_chart,sort=T) %>%
  mutate(weight_lbs=as.factor(weight_lbs),
         weight_lbs=as.double(weight_lbs),
         est_weight=as.factor(est_weight),
         est_weight=as.double(est_weight),
         ott=as.factor(ott),
         ott=as.numeric(ott)) %>%
  filter(!ott==0) %>%
  mutate(pct_weight=round(sum(weight_lbs),2),
         pct_weight=round(weight_lbs/pct_weight*100,2),
         .after=pct_chart) %>%
  # I added this new information, so I need to re-run all the models
  mutate(base=median(weight_lbs[year==2013]),
         w_ratio=weight_lbs/base) %>% #head
  arrange(year) %>%
  select(-n,-base) # %>%count(year,w_ratio)


# Set the df for plotting
med_ratios <- df%>%
  filter(type=="Pumpkins")%>%
  filter(year%in%c("2013","2020"))%>%
  arrange(w_ratio) %>%
  filter(!country%in%c("The Netherlands","Spain","Belgium","Poland","Portugal"))%>%
  group_by(country,year) %>%
  summarize(median_w_ratio=round(median(w_ratio),2))%>%
  ungroup()%>%
  pivot_wider(names_from=year,values_from=median_w_ratio)%>%
  arrange(2013,2020)

# find the values for the secondary axis
my_y_axis <- df %>%
  left_join(med_ratios,by="country")%>%
  mutate(country_ratio_13=paste0(`2013`,"-",country),
         country_ratio_20=paste0(country,"-",`2020`),
                                 .after="country") 

my_sec_y_axis <- c("Italy-0.78","United Kingdom-0.97",
                   "Germany-0.99","United States-0.84",
                   "Finland-0.96","Japan-1.17",
                   "Austria-0.96","Canada-0.87",
                   "Slovenia-1.21","Switzerland-1.42","France-1.13")

my_sec_y_axis<- as.factor(my_sec_y_axis)
my_sec_y_axis <- rev(my_sec_y_axis)



# violin plot
final <- my_y_axis%>%
  filter(type=="Pumpkins")%>%
  filter(year%in%c("2013","2020"))%>%
  arrange(w_ratio) %>%
  filter(!country%in%c("The Netherlands","Spain","Belgium","Poland","Portugal"))%>%
  ggplot(aes(x=(w_ratio),y=fct_reorder(country_ratio_13,-(w_ratio)),group=country)) +
  geom_jitter(shape=".",color="gold")+
  geom_violin(fill= "darkorange", color="darkgreen",alpha=0.8,size=0.3)+
  geom_boxplot(width=0.1,outlier.colour = NA,fill="sandybrown",color=NA)+
  scale_color_manual(values=c("green","pink"))+
  scale_x_discrete(expand = expansion(mult = c(0, .1)))+
  facet_wrap(~year)+
  # from: https://cran.r-project.org/web/packages/ggh4x/vignettes/PositionGuides.html
  guides(y = guide_axis_manual( label_size = c(12, 8)),
    y.sec = guide_axis_manual(labels = my_sec_y_axis, label_size = c(12,8)))+
  #label_colour = c("gold", "blue")
  labs(title="Pumpkins variability weights",
       subtitle="on selected countries 2013-2020",
       x="Ratios (Base 2013)",y="Selected Countries")+
  theme(axis.text.x = element_text(angle=0))+
  ggthemes::theme_solarized() +
  theme(text = element_text(family="Eater"),
        strip.background = element_blank(),
        strip.text = element_text(color="gold",face="bold",size=16),
        plot.background = element_rect(fill="grey33",color="grey33"),
        plot.title = element_text(color="gold",face="bold",size=34),
        plot.title.position = "plot",
        plot.subtitle = element_text(color="springgreen4"),
        panel.background = element_rect(fill="grey33",color="grey33"),
        axis.text.y = element_text(color="gold",face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="gold",face="bold",family="Eater",hjust=0.5),
        axis.line.x = element_blank(),
        axis.text.x = element_text(color="gold",face="bold",size=8),
        axis.ticks.x = element_line(color="gold",size=2),
        plot.margin = margin(0.5,1,1.2,1,"cm"),
        panel.spacing=unit(0, "lines"))


# frame the plot
library(ggpubr)
graphics <- ggarrange(final)

final_plot <- annotate_figure(graphics,
                              top = text_grob("",
                                              color = c("grey28"), face = "bold", size = 3,
                                              family = "Eater"),
                              left = text_grob(" ",
                                               color = c("grey28"), face = "bold", size = 5,
                                               family = "Eater"),
                              right = text_grob(" ",
                                                color = c("grey28"), face = "bold", size = 10,
                                                family = "Eater"),
                              bottom = text_grob("Infographics Federica Gazzelloni DataSource: BigPumpkins - GPW - TidyTuesday week43\n",
                                                 color = "grey28",family = "Eater",
                                                 hjust = 0.5, x = 0.5, face = "bold.italic", size = 13)
)

#source: BigPumpkins.com	Great Pumpkin Commonwealth

# add annotations
final_plot <- final_plot +
  annotate(geom = "text", label = "talking about Pumpkins",
           x = 0.75, y = 0.1,colour = "gold",size = 6,
           family = "Eater",fontface = "bold") +
  annotate(geom = "text", label = "88% median value",
           x = 0.5, y = 0.85,colour = "gold",size = 6,
           family = "Eater",fontface = "bold") +
  annotate(geom = "curve",curvature=-0.2,
           x = 0.6, xend=0.4, y = 0.7,yend=0.8,
           colour = "gold",size = 1,
           arrow=arrow(length=unit(0.03,"npc")))+
  annotate(geom = "text", label = "decreased in variability",
           x = 0.7, y = 0.7,colour = "gold",size = 3,
           family = "Eater",fontface = "bold")



library(cowplot)
library(ggimage)
library(magick)

# add the images for the legend keys
imgpump <- image_read("/Users/federica/Documents/R/R_general_resourses/TidyTuesday/TidyTuesday/w43/GPCMedium512.png")
imgtt <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>% image_resize("300x300")


# ggdraw from {cowplot} draw the plot for setting the background colors of the side annotations
final <- cowplot::ggdraw(final_plot) +
  draw_image(imgtt, x = 0.8, y = 0.4,width = 0.15) +
  draw_image(imgpump, x = 0.9, y = -0.45,width = 0.06) +
  theme(plot.background = element_rect(fill = "orange",color = "gold"))




# save final plot
ragg::agg_png(here::here("/Users/federica/Documents/R/R_general_resourses/TidyTuesday/TidyTuesday/w43/pumpkins.png"),
              res = 320, width = 12, height = 8, units = "in")
final
dev.off()

