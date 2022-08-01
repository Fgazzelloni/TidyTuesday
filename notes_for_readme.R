


#-------MAKES THE GIF-------

library(fs)
library(magick)
library(tidyverse)
# path = "/Users/federica/Documents/R/R_general_resources/TidyTuesday/data/2022/images"
path_github = "data/2022/images"

list_png <- dir_ls(path = path_github,
                   glob = "*.png")
list_gif <- dir_ls(path = path_github,
                   glob = "*.gif")


list_png %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_resize("600x600!")%>% # force resizing
  #image_scale("50%") %>% 
  #image_resize("600x600") %>%
  image_animate(fps = .5) %>% # animates, can opt for number of loops
  image_write(here::here("data/2022/gather", "gather.gif")) # write the gif to file

#------GATHER ALL IMAGES------

library(tidyverse)

path = "/Users/federica/Documents/R/R_general_resources/TidyTuesday/data/2022/images"
path_github = "data/2022/images"


files_png <- list.files(path = path, 
                        pattern=c("*.png"))
files_gif <- list.files(path = path, 
                        pattern=c("*.gif"))

library(fs)
t1<-as_tibble(path(files_png))
t2<-as_tibble(path(files_gif))
all_images<-rbind(t1,t2)


all_images1<-all_images %>%
  mutate(id=parse_number(value),
         id=trimws(id,which = "both"),
         id=as.numeric(id))
  
files_png_git <- list.files(path = path_github, 
                    pattern=c("*.png"), 
                    full.names=TRUE)
files_gif_git <- list.files(path = path_github, 
                    pattern=c("*.gif"), 
                    full.names=TRUE)


t1_git<-as_tibble(path(files_png_git))
t2_git<-as_tibble(path(files_gif_git))
all_images_git<-rbind(t1_git,t2_git)


files_png_full <- list.files(path = path, 
                             pattern=c("*.png"), 
                             full.names=TRUE)
files_gif_full <- list.files(path = path, 
                             pattern=c("*.gif"), 
                             full.names=TRUE)


t1_full<-as_tibble(path(files_png_full))
t2_full<-as_tibble(path(files_gif_full))
all_images_full<-rbind(t1_full,t2_full)


all_images2 <-all_images1 %>%
  cbind(git=all_images_git$value,full=all_images_full$value)%>%
  arrange(id) %>%
  mutate(full=as.character(full))

  
library(gt)
library(gtExtras)  

tb_images <- matrix(all_images2$full,ncol=4,byrow = TRUE)%>%
  as.data.frame() %>% 
    gt::gt()%>%
    gt_theme_538() %>%
    tab_header(
      title = html("<strong>#TidyTuesday 2022</strong>"),
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
      gtsave_extra("data/2022/gather/puzzle.png",vwidth = 1600)




#----EXTRA INFO---
    path = "/Users/federica/Documents/R/R_general_resources/TidyTuesday/data/2022/images"
    path_github = "data/2022/images"
    images_list<- list.files(path = "/Users/federica/Documents/R/R_general_resources/TidyTuesday/data/2022/images",
                             pattern=c("*.png","*.gif"))
    library(tidyverse)
    library(fs)
     images<-paste0(path_github,images)
    # as_tibble(path_rel(images)) 
    as_tibble(path(images)) 
    library(imager)
    files <- list.files(path = path_github, 
                        pattern=c("*.png","*.gif"), 
                        full.names=TRUE)
    all_im <- lapply(files, load.image )
#--------

    
    list.files("data/2021", pattern = "w*/w*.*pdf", full.names = TRUE)
  
    list.files("data/2021", pattern = "w*/w*.*png", full.names = TRUE)
    
    # https://stackoverflow.com/questions/30054765/how-to-get-r-to-read-in-files-from-multiple-subdirectories-under-one-large-direc
    dat.files  <- list.files(path="data/2021",
                             recursive=T,
                             pattern="*.png",
                             full.names=T)
