
# Title: "TidyTuesday Week 40 - NBER Programs Category"
# Author: "Federica Gazzelloni"
# Date: "9/30/2021"
# Datasource: https://www.nber.org/

#----Libraries----
# Load the libraries
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
#library(nberwp)
library(extrafont)
#fonts()
library(RColorBrewer)
library(patchwork)

#----Load Data----
# TidyTuesday week40 datasets
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

#----Data Wrangling----
# Joining sets
authors <- authors %>% select(author,name)
programs <- programs %>% drop_na()

df <- papers %>%  #count(paper)                # 29434     4
  inner_join(paper_authors, by ="paper") %>%   # 67090     2
  full_join(paper_programs, by = "paper") %>%  # 53996     2
  inner_join(authors, by = "author") %>%       # 15437     2
  full_join(programs, by = "program") %>%      #    20     3
  drop_na() 

# Make a dataframe with programs category and proportions
df_cat <- df %>%
  count(program_desc,program_category,sort=T) %>%
  mutate(prop = n/sum(n)*100) %>%
  pivot_wider(names_from = program_category, values_from = program_desc)

df_cat

# Set the data ready to use in the plot function

df_plot <- df %>% count(year,program_category,program_desc) 

plot_fin_df <- df_plot %>% filter(program_category == "Finance")
plot_mic_df <- df_plot %>% filter(program_category == "Micro")
plot_mac_df <- df_plot %>% filter(program_category == "Macro/International")

#----Plot features----
# Set all the specifications for the plot function to build
# Make three tibbles as to be used in the legends
leg_fin <- tibble("Finance"=paste(df_cat$Finance,"-",round(df_cat$prop,2),"%"))%>%filter(!str_detect(Finance,"NA"))
leg_mic <- tibble("Micro"=paste(df_cat$Micro, "-",round(df_cat$prop,2),"%"))%>%filter(!str_detect(Micro,"NA"))
leg_mac <- tibble("Macro/International"=paste(df_cat$`Macro/International`,"-",round(df_cat$prop,2),"%"))%>%filter(!str_detect(`Macro/International`,"NA"))

leg_fin;leg_mic;leg_mac

#Set the `color` option for the plot function
require(RColorBrewer)
# color
cut_colors1 <- setNames(brewer.pal(2, "Set1"), levels(plot_fin_df$program_desc))
cut_colors2 <- setNames(brewer.pal(4, "Paired"), levels(plot_mac_df$program_desc))
cut_colors3 <- setNames(c(brewer.pal(name = "Set3", n = 12), brewer.pal(name = "Pastel1", n = 2)), levels(plot_mic_df$program_desc))

# Unlist legends-dataframe to be used in the legends
# leg_lab
leg_fin <- unlist(leg_fin$Finance)
leg_mac <- unlist(leg_mac$`Macro/International`)
leg_mic <- unlist(leg_mic$Micro)

# leg_pos  
set1 = c(0.73,0.78)
set2 = c(0.7,0.8)
set3 = c(0.55,0.8)

#----ggcombo-------
# Make a `ggcombo()` plot building a function for plotting the program categories

ggcombo <- function(data1,data2,data3){
  
  ggbar_cat <- function(data,leg_pos,leg_lab,leg_col,color){
    
    data %>%
      ggplot(aes(x = year,y = n,group = program_desc,fill = program_desc)) +
      geom_col() +
      facet_wrap(vars(program_category), ncol = 1, strip.position = "right") +
      
      scale_fill_manual(values = color, label = leg_lab, name = paste(data[[1,2]],"category Impact proportion")) +
      scale_y_continuous(position = "right") +
      guides(fill = guide_legend(ncol = leg_col,title.position = "top", title.hjust = 0.5)) +
      ggthemes::theme_fivethirtyeight() +
      theme(text = element_text(family = "Roboto Condensed"),
            axis.text.x = element_text(face = "bold",size = 8),
            axis.text.y = element_text(),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.3, 'cm'),
            legend.title = element_text(face = "bold"),
            
            legend.position = leg_pos,
            legend.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(face = "bold",size = 14))
  }
  
  plot_fin <- ggbar_cat(data1,set1,leg_fin,1,cut_colors1)
  plot_mac <- ggbar_cat(data2,set2,leg_mac,1,cut_colors2)
  plot_mic <- ggbar_cat(data3,set3,leg_mic,2,cut_colors3)
  
  require(patchwork)
  plot_fin <- plot_fin +
    labs(title = "\n",subtitle = "\n")
  
  plot_fin/plot_mac/plot_mic
}


# Assign a name to the ggcombo 
plot <- ggcombo(plot_fin_df,plot_mac_df,plot_mic_df)


#----Pie chart----
# Make a pie_chart logo
pie_colors <- brewer.pal(name = "Set2", n = 3)

pie_df <- df %>%
  count(program_desc,program_category,sort = T) %>%
  mutate(prop = n/sum(n)*100)

par_prop <- pie_df %>%
  group_by(program_category) %>%
  summarize(par_prop = round(sum(prop),0))

pie_plot <- pie_df %>%
  left_join(par_prop,by = "program_category") %>%
  ggplot(aes(x = "", y = prop, fill = program_category)) +
  geom_col(width = 1, stat = "identity") +
  scale_fill_manual(values = pie_colors,name = "NBER Programs Category") +
  guides(fill = guide_legend(ncol = 1,title.position = "top", title.hjust = 0.5)) +
  ggthemes::theme_fivethirtyeight() +
  theme(text = element_text(family = "Roboto Condensed"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(face = "bold",size = 16),
        legend.position = "none",#c(0.21,0.9),
        legend.background = element_blank(),
        strip.placement = "outside") +
  annotate(geom = "text", label = "Micro\n62%", x = 1.1, y = 20, colour = "grey20", size = 14, family = "Roboto Condensed") +
  annotate(geom = "text", label = "Macro\n28%", x = 1.2, y = 80, colour = "grey20",size = 14, family = "Roboto Condensed") +
  annotate(geom = "text", label = "Finance\n10%", x = 1.2, y = 95, colour = "grey20", size = 11,family = "Roboto Condensed") +
  coord_polar("y", start = 0) 

# save the pie_chart_logo
ragg::agg_png(here::here("w40/w40_pie_ep.png"),
              res = 320, width = 8, height = 8, units = "in")
pie_plot
dev.off()  

#----Annotations----
# Annotate the figure first with adding top and bottom information to have it framed with `ggarrange()`
library(ggimage)
library(magick)
library(cowplot)
library(ggpubr)

graphics <- ggarrange(plot)

final_plot <- annotate_figure(graphics,
                              top = text_grob("NBER National Bureau of Economic Research",
                                              color = c("grey28"), face = "bold", size = 34,
                                              family = "Roboto Condensed"),
                              bottom = text_grob("Infographics Federica Gazzelloni DataSource: NBER - TidyTuesday week40\n",
                                                 color = "grey28",family = "Roboto Condensed",
                                                 hjust = 0.5, x = 0.58, face = "bold.italic", size = 16)
)


# Finally, add some other extra information with more annotations
library(gridExtra)

final_plot <-
  final_plot +
  
  annotate(geom = 'segment',y = 0.87, yend = 0.93, x = 0.9,xend = 0.9, color = "#1E90FF", size = 10) +
  
  annotate(geom = "text", label = "All three Program Categories reached the top level in 2020 with 
           the highest number of paper publications due to Covid19",
           x = 0.58, y = 0.90,colour = "grey20",size = 6,family = "Roboto Condensed",fontface = "bold") +
  
  annotate(geom = "text", label = "Finance topic started in 1978 
           but with lack of success since late 1990 
           when started its continuous growth",
           x = 0.25, y = 0.7,colour = "grey20",size = 5,family = "Roboto Condensed") +
  
  annotate(geom = "text", label = "Macro/International topic started in 1975 
           reaching the highest level among the other 
           categories, after the first decrease in early 1990 decade, 
           most probably for the increased interest in other topics, 
           maintained a steady growth along the years",
           x = 0.28, y = 0.5,colour = "grey20",size = 4,family = "Roboto Condensed") +
  
  annotate(geom = "text", label = "Micro topic is the most varied one, 
           and maintained little but steady increase 
           along the whole period",
           x = 0.24, y = 0.15,colour = "grey20",size = 5,family = "Roboto Condensed") 

#----Logos----
# add the logos 
img_pie <- image_read(here::here("w40/w40_pie_ep.png"))
imglogo <- image_read(here::here("w40/w40_nber-logo.png"))

final <- ggdraw() +
  draw_plot(final_plot) +
  draw_image(img_pie, x = 0.05, y = 0.35,width = 0.22) +
  draw_image(imglogo, x = 0.01, y = -0.48,width = 0.2)


#----Save final plot----
ragg::agg_png(here::here("w40/w40_ep.png"),
              res = 320, width = 10, height = 12, units = "in")
final
dev.off()

#----Tidytuesday logo----
# read the image, attach the Tidytuesday logo and save it
tidy_logo <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")

tidy_final <- image_read(here::here("w40/w40_ep.png"))
attached_logo <- image_composite(tidy_final, tidy_logo,
                                 operator = "atop",
                                 gravity = "southeast")

image_write(attached_logo, path = "w40_ep.png", format = "png")





