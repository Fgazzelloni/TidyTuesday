# inspired by https://github.com/cararthompson/30DayChartChallenge/blob/main/scripts/2.2_animals.R
rm(list = ls())
## Load libraries ----
library(tidyverse)
library(extrafont)
library(cowplot)
library(ggExtra)
extrafont::fonts()

## Read in data ----
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

#-----------

df <- lemurs %>%
  filter(!sex == "ND") %>%
  mutate(year_dob = lubridate::year(dob),
         month_dob = lubridate::month(dob),
         .after = dob) %>%
  mutate(year_dod = lubridate::year(dod),
         month_dod = lubridate::month(dod),
         .after = dod) %>%
  group_by(taxon,sex,birth_type, dob,year_dob,month_dob,dod,year_dod,month_dod,age_max_live_or_dead_y,age_category) %>%
  summarize(mean_weight = round(mean(weight_g)/1000,2)) %>%
  ungroup() %>%
  filter(!mean_weight == 0) %>%
  filter(!is.na(age_max_live_or_dead_y)) %>%
  filter(!birth_type == "unknown") %>%
  rename(mum_age_category = age_category,
         max_age = age_max_live_or_dead_y) %>% #
  mutate(status = if_else(is.na(dod),"alive","dead"),
         .after = dod) %>%
  mutate(status_id = case_when(
    status == "alive" ~ 1,
    status == "dead" ~ 0),
    .after = status )  %>% # DataExplorer::profile_missing()
  mutate(birth_type = case_when(
    birth_type == "CB" ~ "captive-born",
    birth_type == "WB" ~ "wild-born",
    TRUE ~ "unknown")) %>%
  mutate(mum_age_category = case_when(
    mum_age_category == "IJ" ~ "infant",
    mum_age_category == "young_adult" ~ "young",
    TRUE ~ mum_age_category)) %>%
  select(-status) %>%
  mutate(birth_type_id = if_else(birth_type == "captive-born",1,0),
         sex_id = if_else(sex == "F",1,0),
         mum_age_category_id = case_when(mum_age_category == "infant" ~ 1,
                                         mum_age_category == "young" ~ 2,
                                         mum_age_category == "adult" ~ 3)) %>%
  mutate(sex = if_else(sex == "F","Female","Male"))


df <- df %>%
  mutate(mean_weight_class = case_when(mean_weight<=0.2 ~ "0.01 - 0.1",
                                       mean_weight>0.1 & mean_weight<=0.8 ~ "0.11 - 0.8",
                                       mean_weight>0.8 & mean_weight<=1.45 ~ "0.81 - 1.45",
                                       mean_weight>1.45 & mean_weight<=2.3 ~ "1.46 - 2.3",
                                       mean_weight>2.3 ~ "2.3 +")) %>%
  mutate(mean_weight_class = factor(mean_weight_class))

df_taxon_id <- df %>% count(taxon,sort = TRUE) %>% mutate(taxon_id = row_number()) %>% select(-n)
df_weight_class_id <- df %>% count(mean_weight_class) %>% mutate(weight_class_id = row_number()) %>% select(-n)

df <- df %>%
  inner_join(df_weight_class_id, by = "mean_weight_class") %>%
  inner_join(df_taxon_id, by = "taxon") %>%
  select(last_col(),everything())
# %>% select(-taxon,-mean_weight_class)

head(df,3)
#--------

# Create label data
labels <- tibble(mum_age_category = c("Infant", "Young", "Adult"),
                 mum_age_category_x = c(0, 0, 0),
                 mum_age_category_y = c(1, 1, 1))




## Create colour scheme and theme ----
lemurs_hues <- c("#d2dbe4", "#8a5d24", "#646376", "#192029", "#acb3bf", "#596e94")

theme_lemurs_light <- function() {
  theme_minimal() %+replace%
    theme(text = element_text(colour = lemurs_hues[4]),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.ticks = element_blank())
}

## Plot it ----



# infant = grey
# young = light brown
# adult = light grey

p <- ggplot(df) +
  scale_colour_manual(values = c(lemurs_hues[c(3, 2, 5)]),
                      labels = c("Infant", "Young", "Adult")) +
  scale_fill_manual(values = c(lemurs_hues[c(3, 2, 5)]),
                      labels = c("Infant", "Young", "Adult")) +
  geom_point(aes(x = dob,y = dod, colour = mum_age_category,size = mean_weight),alpha = 0.7) +
  geom_smooth(aes(x = dob,y = dod, colour = mum_age_category), se = FALSE) +
      labs(x = "Year of birth",
           y = "Year of death",
           size = "Years") +
      guides(colour = "none", size = "none") +
      theme_lemurs_light() +
  theme(text = element_text(family = "xkcd"))


marg <- ggMarginal(p, type = "densigram", groupColour = T, groupFill = T, alpha = 0.7)

bm <- ggplot(df, aes(x = mean_weight)) +
  geom_histogram(aes(y = stat(count),fill = mum_age_category, colour = factor(mum_age_category)),
                 position = position_dodge(width = 0.3),
                 bins = 50,
                 alpha = 0.7, show.legend = T) +
  guides(color = "none") +
  facet_wrap(vars(sex)) +
  labs(y = "N.",
       x = "Lemurs' weight in kg",
       fill = "Lemurs' mother age category") +
  scale_fill_manual(values = c(lemurs_hues[c(3, 2, 5)]),
                    labels = c("Infant", "Young", "Adult")) +
  scale_colour_manual(values = c(lemurs_hues[c(3, 2, 5)]),
                      labels = c("Infant", "Young", "Adult")) +
  theme_lemurs_light() +
  theme(text = element_text(family = "xkcd"),
        legend.position = c(0.5,1.1),
        legend.justification = "center",
        legend.direction = "horizontal"
        )

title <- ggdraw() +
  draw_label("Is Lemurs life expectancy distribution dependent from their mums status at pregnancy?",
             fontfamily = "xkcd",
             colour = lemurs_hues[6],
             hjust = 0.5,
             size = 22)

subtitle <- ggdraw() +
  draw_label("Baby lemurs stay with their mothers for about two years. In this time span, the baby lemurs are nursed and protected by their mother.
             When it grows up the lemur stays in the troop, if it is a female, or otherwise it joins another group. The life span of a lemur is approximately eighteen years",
             fontfamily = "xkcd",
             colour = lemurs_hues[4],
             hjust = 0.5,
             size = 12)

caption <- ggdraw() +
  draw_label("TidyTuesday week35 - InfoGraphic: Federica Gazzelloni - Source: Lemurs,Kaggle,Zehr et al, 2014 - Nature",
             fontfamily = "xkcd",
             colour = lemurs_hues[4],
             hjust = 0.5,
             size = 9)

combined_p <- plot_grid(title,
                        subtitle,
                        marg,
                        bm,
                        caption,
                        ncol = 1,
                        rel_heights = c(0.05, 0.1, 0.6, 0.2, 0.05))





library(ggpubr)
graphics <- ggarrange(combined_p)

final_plot <- annotate_figure(graphics,
                              top = text_grob("",
                                              color = c("#8a5d24"), face = "bold", size = 24,
                                              family = "xkcd"),
                              bottom = text_grob("",
                                                 color = "#6C7B8B",family = "xkcd",
                                                 hjust = 0.5, x = 0.5, face = "bold.italic", size = 10),
                              left = text_grob("", color = c("#778899"), rot = 90,size = 10),
                              right = text_grob(bquote(""), color = c("#778899"),rot = 90,size = 10),
                              fig.lab = "TidyTuesday week35", fig.lab.face = "bold.italic",fig.lab.size = 8,
                              fig.lab.pos = "bottom.right"
)

final_plot <- 
  final_plot +
  
  annotate(geom = "text", label = "In the wild, ring-tailed lemurs can live about 20 years. 
           They are the most commonly found species of lemur in zoos, 
           where they can live up to a decade longer.",
           x = 0.15, y = 0.65,colour = "black",size = 3,family = "xkcd") +
  
  annotate(geom = "text", label = "",
           x = 0.15, y = 0.65,colour = "black",size = 3,family = "xkcd") +
  
  annotate(geom = "text", label = "The Indri,also known as the Babakoto,is the biggest living lemur. 
           A tree-dwelling Madagascar species, the Indri is known to grow as tall as 3 feet, 
           and weigh as much as 10 pounds.",
           x = 0.77, y = 0.18,colour = "black",size = 3,family = "xkcd") +
  
  annotate(geom = "text", label = "Adult",x = 0.83, y = 0.78, colour = "#FF7F00", size = 5,family = "xkcd") +
  annotate(geom = "text", label = "Young",x = 0.73, y = 0.58, colour = "#FF7F00", size = 5,family = "xkcd") +
  annotate(geom = "text", label = "Infant",x = 0.1, y = 0.5, colour = "#FF7F00", size = 5,family = "xkcd") +
  annotate(geom = "curve", x = 0.81, xend = 0.76, y = 0.78, yend = 0.72, colour = "#FF7F00", curvature = .3, arrow = arrow(length = unit(2, "mm")),family = "xkcd") +
  annotate(geom = "curve", x = 0.72, xend = 0.68, y = 0.6, yend = 0.65, colour = "#FF7F00", curvature = .3, arrow = arrow(length = unit(2, "mm")),family = "xkcd") +
  annotate(geom = "curve", x = 0.12, xend = 0.2, y = 0.5, yend = 0.43, colour = "#FF7F00", curvature = -.3, arrow = arrow(length = unit(2, "mm")),family = "xkcd") +

  annotate(geom = "text", label = "Comparing lemurs' life expectancy and weight",
         x = 0.5, y = 0.84,colour = "#FF7F00",size = 3,family = "xkcd") +
  annotate(geom = "text", label = "Male and female ring-tailed lemurs are similar physically.
They are roughly the same size, measuring about 42.5 cm or 1.4 ft.
           from head to rump and weighing roughly 2.25 kg or 5 lb.",
           x = 0.67, y = 0.38,colour = "black",size = 3,family = "xkcd") 



library(ggimage)
library(magick)
library(cowplot)

lemur_img <- image_read("image.png")
#logo_file <- system.file("extdata", "logo.png", package = "cowplot")
  
final <- ggdraw() +
  draw_plot(final_plot) +
  draw_image(lemur_img, x = 0.04, y = 0.3,width = 0.15)
  

final


## Save plot ----

ragg::agg_png(here::here("w35/w35_lemurs.png"),
              res = 320, width = 12, height = 14, units = "in")
final

dev.off()



# read the image, attach the Tidytuesday logo and save it --------------------------
library(ggimage)
library(magick)


tidy_logo <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")

img <- image_read("image.png")


tidy_final <- image_read("w35_lemurs.png")
attached_logo <- image_composite(tidy_final, tidy_logo,
                                 operator = "atop",
                                 gravity = "southwest")

image_write(attached_logo, path = "w35_lemurs.png", format = "png")


