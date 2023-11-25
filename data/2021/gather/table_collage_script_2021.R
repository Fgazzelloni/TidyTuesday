# Building a collage of all 2021 images

# select all images
dat.files  <- list.files(path="data/2021",
                         recursive=T,
                         pattern="^w.*png$",
                         full.names=T)
dat.files

library(tidyverse)
library(fs)
all_images<-as_tibble(path(dat.files))
all_images

all_images1<-all_images %>%
  mutate(id=parse_number(value),
         id=trimws(id,which = "both"),
         id=as.numeric(id))

files_png_git <- list.files(path = path_github, 
                            pattern=c("*.png"), 
                            full.names=TRUE)

library(gt)
library(gtExtras)  

tb_images <- matrix(all_images1$value,ncol=4,byrow = TRUE)%>%
  as.data.frame() %>% 
  gt::gt()%>%
  gt_theme_538() %>%
  tab_header(
    title = html("<strong>#TidyTuesday 2021</strong>"),
    subtitle = md("weekly appointment for practicing making #DataVisualization")) %>%
  tab_source_note(source_note = md("datasets are provided by the #R4DS Online Learning Community"))%>%
  tab_options(
    column_labels.hidden = TRUE
  ) %>%
  gtExtras::gt_img_rows(columns=V1, img_source = "local", height = 200)%>%
  gtExtras::gt_img_rows(columns=V2, img_source = "local", height = 200)%>%
  gtExtras::gt_img_rows(columns=V3, img_source = "local", height = 200)%>%
  gtExtras::gt_img_rows(columns=V4, img_source = "local", height = 200) 



tb_images%>%
  gtsave_extra("data/2021/gather/table_collage_2021.png",
               vwidth = 1600)
