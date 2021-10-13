# WEEK42 SEAFOOD

# Load library and data
library(tidyverse)

farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

# make one dataframe 
df <- farmed %>%
  full_join(captured_vs_farmed,by=c("Entity","Code","Year","Aquaculture production (metric tons)")) %>%
  full_join(captured,by=c("Entity","Code","Year","Capture fisheries production (metric tons)")) %>%
  full_join(consumption,by=c("Entity","Code","Year")) %>%
  full_join(stock,by=c("Entity","Code","Year")) %>%
  full_join(fishery,by=c("Entity","Code","Year")) %>%
  full_join(production,by=c("Entity","Code","Year")) %>%
  janitor::clean_names()

names(df);dim(df)
DataExplorer::profile_missing(df)


# set the dataset for the first plot 
df_group1 <- df %>%
  filter(is.na(code)) %>%
  select(-code) %>%
  filter(str_detect(entity,"income")) %>%
  pivot_longer(cols = 3:19,names_to = "names",values_to = "values") %>%
  mutate(values = ifelse(is.na(values),0,values)) %>%
  filter(values > 0) %>% 
  mutate(entity = gsub("excluding high income","*",entity)) %>% #count(entity)
  mutate(entity = factor(entity, levels = c('High income','Upper middle income','Middle income',
                                          'Lower middle income','Low & middle income','Low income',
                                          'Europe & Central Asia (*)','Latin America & Caribbean (*)',
                                          'East Asia & Pacific (*)','Middle East & North Africa (*)','Sub-Saharan Africa (*)'))) 


# choose the color palette  
# RColorBrewer::brewer.pal.info
# set the text options 
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = T)
font_add_google("Share Tech Mono", "techmono")
   

# make the first plot: a facet_plot of the World Continents by income level
facet_plot <-  ggplot(data = df_group1, aes(x = factor(year), y = values/100000000,group = names,color = names)) +
  geom_line(size = 1.3) +
  guides(color = guide_legend(title = "Production tons sc-%",ncol = 1,title.position = "top", title.hjust = 0.5)) +
  scale_x_discrete(breaks = seq(1960,2018,10)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(limits = c("aquaculture_production_metric_tons","capture_fisheries_production_metric_tons"),
                     labels = c("aquaculture_production_metric_tons" = "Aquaculture",
                                "capture_fisheries_production_metric_tons" = "Capture Fisheries"),
                     values = RColorBrewer::brewer.pal(2,"Reds")) +
  facet_wrap(vars(entity),scales = "free",nrow = 2) +
  labs(subtitle = " ", caption = " ") +
  ggthemes::theme_fivethirtyeight() +
  theme(text = element_text(family = "techmono",color = "#FFF8DC",face = "bold"),
        legend.position = c(0.92,0.3),
        legend.title = element_text(face = "bold",size = 14),
        legend.box.background = element_blank(),
        legend.background = element_rect(fill = "#009ACD",color = "#009ACD"),
        legend.key = element_rect(fill = "#009ACD",color = "#009ACD"),
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 9,face = "bold"),
        strip.background = element_rect(fill = "#009ACD",color = "#009ACD"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = 0.2),
        plot.background = element_rect(fill = "#009ACD",color = "#009ACD"),
        panel.background = element_rect(fill = "#009ACD",color = "#009ACD"),
        plot.title = element_text(face = "bold",size = 30),
        plot.margin = margin(0,0,0,0,unit = "pt")) 



#-------------
# seafood supply time-series plot of all countries and selected countries of seafood supply kg per capita FAO yr 2020


# data to use in the second plot for the first geom_line for all countries (in grey)
df_group2 <- df %>% 
  filter(!is.na(code)) %>%
  pivot_longer(cols = 14:20,names_to = "livestock",values_to = "livestock_commodity") %>%
  select(-4,-5) %>%
  mutate(livestock = gsub("commodity_balances_livestock_and_fish_primary_equivalent_","",livestock),
         livestock = gsub("_production_5510_tonnes","",livestock),
         livestock = gsub("[0-9]","",livestock),
         livestock = gsub("_","",livestock)) %>%
  pivot_longer(cols = 5:6,names_to = "share_of_fish",values_to = "fish_stocks") %>%
  mutate(share_of_fish = recode(share_of_fish,
                                share_of_fish_stocks_that_are_overexploited = "overexploited",
                                share_of_fish_stocks_within_biologically_sustainable_levels_fao_2020 = "sustainable")) %>%
  pivot_longer(cols = c(5,7),names_to = "commercial",values_to = "scale_commercial") %>%
  mutate(commercial = recode(commercial,
                             artisanal_small_scale_commercial = "artisanal",
                             industrial_large_scale_commercial = "industrial")) %>%
  rename(seafood_supply = fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020)


df_group2[is.na(df_group2)] <- 0

df_group2 <- df_group2 %>% 
  filter(seafood_supply > 0) %>% 
  select(year,entity,seafood_supply)



# data to use in the second geom_line for selected countries
df_group3 <- df_group2 %>%
  filter(entity %in% c("Iceland","Maldives","Lesotho","Brazil",'Sri Lanka',"Malaysia","Chile","Tanzania","Japan")) 



# make the second plot: time series of all countries and selected countries of seafood supply kg per capita FAO yr 2020
 seafood_plot <- ggplot() +
  geom_line(data = df_group2, 
            aes(x = factor(year),y = (seafood_supply),group = entity),color = "grey75",size = 0.2) +
  geom_line(data = df_group3 ,
            aes(x = factor(year),y = (seafood_supply),group = entity,color = entity),size = 1.2,
            key_glyph = "timeseries") +
  scale_x_discrete(breaks = seq(1960,2018,5), expand = expansion(add = 0.5)) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_color_manual(values = RColorBrewer::brewer.pal(9,"Set1")) +
  guides(color = guide_legend(title = "Selected Countries seafood production kg per capita FAO yr 2020",
                              nrow = 1,title.position = "top", title.hjust = 0.5,title.vjust = 0.5)) +
  labs(x = "Years",y = "Values in log scale",
       title = " ",
       subtitle = " ") +
  ggthemes::theme_fivethirtyeight() +
  theme(text = element_text(family = "techmono",color = "#FFF8DC",face = "bold"),
        axis.title.x = element_text(vjust = 0.5,face = "bold"),
        axis.title.y = element_text(vjust = 0.5,face = "bold"),
        plot.background = element_rect(fill = "#009ACD",color = "#009ACD"),
        panel.background = element_rect(fill = "#009ACD",color = "#009ACD"),
        panel.grid.major.x = element_line(size = 0.2,linetype = "dashed"),
        legend.background = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "top",
        legend.title = element_text(face = "bold",size = 16),
        legend.key = element_blank(),
        plot.title = element_text(face = "bold",size = 30),
        plot.margin = margin(0,0,0,0,unit = "pt"))


# make one plot
library(patchwork)  
plot <- (seafood_plot / facet_plot ) 
  #theme_update(plot.background = element_rect(fill="#009ACD",color="#009ACD"),
               #panel.background = element_rect(fill="#009ACD",color="#009ACD"),
               #plot.margin = margin(0,0,0,0,unit = "pt"))


# load the libraries for final touches
require(ggpubr)

# ggarrange from {ggpubr} frames the plot to make side annotations
graphics <- ggpubr::ggarrange(plot) 

final_plot <- ggpubr::annotate_figure(graphics,
                              top = text_grob("Global Seafood Supply in 182 countries \n(1960 - 2018)",
                                              color = c("#FFF8DC"), face = "bold", size = 34,
                                              family = "techmono",vjust = 0.8),
                              bottom = text_grob("Infographics Federica Gazzelloni DataSource: OurWorldinData.org - TidyTuesday week42",
                                                 color = "#FFF8DC",family = "techmono",
                                                 hjust = 0.5, vjust = 0.5, x = 0.5, face = "bold.italic", size = 14),
                              left = text_grob(" ", color = c("#778899"), rot = 90,size = 12),
                              right = text_grob(bquote(" "), color = c("#778899"),rot = 90,size = 10),
                              fig.lab = "", fig.lab.face = "bold.italic",fig.lab.size = 8,
                              fig.lab.pos = "bottom.right"
)

final_plot <- final_plot +
  annotate(geom = "text", label = "(*) excluding high income",
         x = 0.91, y = 0.1,colour = "#FFF8DC",size = 4,family = "techmono",fontface = "bold") 
  
  
library(cowplot)  
library(ggimage)
library(magick)

# add the images for the legend keys 

imgOWD <- image_read(here::here("w42/owd.png"))
imgfish <- image_read(here::here("w42/fish.png"))
imgtt <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


# ggdraw from {cowplot} draw the plot for setting the background colors of the side annotations
final <- cowplot::ggdraw(final_plot) + 
  draw_image(imgfish, x = 0.03, y = 0.44,width = 0.07) +
  draw_image(imgfish, x = 0.08, y = 0.37,width = 0.05) +
  draw_image(imgfish, x = 0.12, y = 0.32,width = 0.03) +
  draw_image(imgtt, x = 0.85, y = 0.45,width = 0.12) +
  draw_image(imgOWD, x = 0.9, y = -0.46,width = 0.06) +
  theme(plot.background = element_rect(fill = "#009ACD",color = "#009ACD")) 


# save final plot
ragg::agg_png(here::here("/Users/federica/Documents/R/R_general_resourses/TidyTuesday/TidyTuesday/w42/w42_seafood.png"),
              res = 320, width = 16, height = 14, units = "in")
final
dev.off()



