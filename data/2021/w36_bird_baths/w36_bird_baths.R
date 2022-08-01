library(tidyverse)
library(ggdendro)
library(dendextend)
library(ggraph)
library(tidygraph)
library(purrr)
library(rlang)


tuesdata <- tidytuesdayR::tt_load(2021, week = 36)
bird_baths <- tuesdata$bird_baths

bird_baths <- bird_baths %>%
  drop_na() %>%
  filter(bird_count>0)  # %>% count(bird_type,sort=TRUE) %>% View()

survey_year_id <- bird_baths %>% count(survey_year) %>%
  mutate(survey_year_id = row_number()) %>% select(-n)
urban_rural_id <- bird_baths %>% count(urban_rural) %>%
  mutate(urb_rul_id = row_number()) %>% select(-n)
bioregions_id <- bird_baths %>% count(bioregions) %>% 
  mutate(bioregions_id = row_number()) %>% select(-n)
bird_type_id <- bird_baths %>% count(bird_type) %>% 
  mutate(bird_type_id = row_number()) %>% select(-n)


bird_baths_numeric <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) %>%
  arrange(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) 


bird_baths_half_numeric <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year,survey_year_id,
        urban_rural,urb_rul_id,
        bioregions,bioregions_id,
        bird_type,bird_type_id) %>%
  arrange(survey_year) 

bird_baths_numeric_short <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) %>% #View()
  filter(n<=20)


bird_baths_half_numeric_short <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year,survey_year_id,
        urban_rural,urb_rul_id,
        bioregions,bioregions_id,
        bird_type,bird_type_id) %>% #View()
  filter(n<=20)



#-------- data to use


bb_piv_w <- bird_baths_half_numeric %>%
  count(survey_year,bird_type,urban_rural,bioregions) %>%
  pivot_wider(names_from=c(survey_year,urban_rural,bioregions),values_from=n,values_fill = 0)


# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html#the-3-clusters-from-the-complete-method-vs-the-real-species-category
birds <- column_to_rownames(bb_piv_w,var = "bird_type")



# View(birds)

# build a dendrogram

dend_r <- birds %>%
  dist(method = "man") %>%
  hclust(method = "ward.D2") %>%
  as.dendrogram %>%
  ladderize %>%
  color_branches(k=4)

dend_c <- t(birds) %>%
  dist(method = "man") %>%
  hclust(method = "com") %>%
  as.dendrogram %>%
  ladderize%>%
  color_branches(k=3)

# set the colors
some_col_func <- function(n) (
  colorspace::diverge_hcl(n, h = c(246, 40), c = 96, l = c(65, 90)))


# plot the heatmap with the dendrograms
par(mar = c(5,5,5,5))

library(gplots)
set_graph_style(plot_margin = margin(1,1,1,1))

plot <- gplots::heatmap.2(as.matrix(birds),
                          main = "Bird types concentration",
                          srtCol = 35,
                          Rowv = dend_r,
                          Colv = dend_c,
                          trace="row", hline = NA, tracecol = "darkgrey",
                          margins =c(11,8),
                          key.xlab = "spotted/unspotted",
                          denscol = "grey",
                          density.info = "density",
                          col = some_col_func
)

## Save the plot as an image ----

ragg::agg_png(here::here("w36/heat_map.png"),
              res = 320, width = 15, height = 8, units = "in")
final

dev.off()

#------------finish touches

library(ggimage)
library(magick)
library(cowplot)

heat_map <- image_read(here::here("w36/heat_map.png"))

plot <- ggdraw() +
  draw_image(heat_map)

library(ggpubr)
graphics <- ggarrange(plot)

final_plot <- annotate_figure(graphics,
                              top = text_grob("",
                                              color = c("#8a5d24"), face = "bold", size = 24,
                                              family = "xkcd"),
                              bottom = text_grob("Infographics Federica Gazzelloni DataSource: Cleary et al, 2016 TidyTuesday week36",
                                                 color = "black",family = "xkcd",
                                                 hjust = 0.5, x = 0.5, face = "bold.italic", size = 10),
                              left = text_grob("", color = c("#778899"), rot = 90,size = 10),
                              right = text_grob(bquote(""), color = c("#778899"),rot = 90,size = 10),
                              fig.lab = "TidyTuesday week36", fig.lab.face = "bold.italic",fig.lab.size = 8,
                              fig.lab.pos = "bottom.right"
)

final_plot <-
  final_plot +

  annotate(geom = "text", label = "While wild bird feeding is recognised as one of
the most popular forms of
human-wildlife interaction, almost nothing is known
           about the use of bird baths.",
           x = 0.15, y = 0.25,colour = "black",size = 3,family = "xkcd") +

  annotate(geom = "text", label = "",
           x = 0.15, y = 0.65,colour = "black",size = 3,family = "xkcd") +

  annotate(geom = "text", label = "Urbanisation is one of the leading causes of species extinction
           due to extensive habitat alteration",
           x = 0.82, y = 0.04,colour = "black",size = 3,family = "xkcd") +

  annotate(geom = "text", label = "Bioregions",x = 0.85, y = 0.88, colour = "#FF7F00", size = 5,family = "xkcd") +
  annotate(geom = "text", label = "Rural",x = 0.73, y = 0.58, colour = "white", size = 5,family = "xkcd") +
  annotate(geom = "text", label = "Urban",x = 0.1, y = 0.5, colour = "#FF7F00", size = 5,family = "xkcd") +
  annotate(geom = "curve", x = 0.82, xend = 0.76, y = 0.88, yend = 0.72, colour = "#FF7F00", curvature = .3, arrow = arrow(length = unit(2, "mm")),family = "xkcd") +
  annotate(geom = "curve", x = 0.72, xend = 0.68, y = 0.6, yend = 0.65, colour = "#FF7F00", curvature = .3, arrow = arrow(length = unit(2, "mm")),family = "xkcd") +
  annotate(geom = "curve", x = 0.12, xend = 0.2, y = 0.5, yend = 0.43, colour = "#FF7F00", curvature = -.3, arrow = arrow(length = unit(2, "mm")),family = "xkcd") +

  annotate(geom = "text", label = "Comparing Bird types' while enjoing a bird bath",
           x = 0.4, y = 0.84,colour = "#FF7F00",size = 3,family = "xkcd") +
  annotate(geom = "text", label = "Avian assemblages at urban and rural bird baths
           differed between bioregions with aggressive
           nectar-eating species",
           x = 0.67, y = 0.38,colour = "black",size = 3,family = "xkcd")



rainbow_lorikeet_img <- image_read(here::here("w36/parrot2.png"))

final <- ggdraw() +
  draw_plot(final_plot) +
  draw_image(rainbow_lorikeet_img, x = 0.89, y = 0.4,width = 0.12)


final


## Save final plot ----

ragg::agg_png(here::here("w36/w36_bird_baths.png"),
              res = 320, width = 15, height = 8, units = "in")
final

dev.off()



# read the image, attach the Tidytuesday logo and save it --------------------------


tidy_logo <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")

img <- image_read("image.png")


tidy_final <- image_read("w35_lemurs.png")
attached_logo <- image_composite(tidy_final, tidy_logo,
                                 operator = "atop",
                                 gravity = "southwest")

image_write(attached_logo, path = "w35_lemurs.png", format = "png")




