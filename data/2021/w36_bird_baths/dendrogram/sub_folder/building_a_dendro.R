library(tidyverse)
library(ggdendro)
library(dendextend)

tuesdata <- tidytuesdayR::tt_load(2021, week = 36)
bird_baths <- tuesdata$bird_baths

bird_baths <- bird_baths %>%
  drop_na() %>%
  filter(bird_count>0) #%>% count(bird_type,sort=TRUE)


urban_rural_id <- bird_baths %>% count(urban_rural) %>%
  mutate(urb_rul_id = row_number()) %>% select(-n)
bioregions_id <- bird_baths %>% count(bioregions) %>% mutate(bioregions_id = row_number()) %>% select(-n)
bird_type_id <- bird_baths %>% count(bird_type) %>% mutate(bird_type_id = row_number()) %>% select(-n)

bird_baths_numeric <- bird_baths %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year,bird_type_id,urb_rul_id,bioregions_id) %>%
  arrange(survey_year)


bird_baths_half_numeric <- bird_baths %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>% 
  inner_join(bioregions_id,
             by="bioregions") %>% 
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year,bird_type,bird_type_id,urban_rural,urb_rul_id,bioregions,bioregions_id) %>%
  arrange(survey_year)



bb_piv_w <- bird_baths_half_numeric %>% count(survey_year,bird_type,urban_rural,bioregions) %>%
  pivot_wider(names_from=c(survey_year,urban_rural,bioregions),values_from=n,values_fill = 0) 


bb_piv_w<- column_to_rownames(bb_piv_w, var = "bird_type")
# View(bb_piv_w)




# bird_baths_numeric

dd <- dist(scale(bb_piv_w), method = "manhattan")

hc <- hclust(dd, method = "ward.D2")

dend<- as.dendrogram(hc)

dend_data <- ggdendro::dendro_data(dend, type = "rectangle")
# View(dend_data)

plot(hc)

dend_0 <- bb_piv_w %>%
  scale %>%
  dist(method="manhattan") %>%
  hclust(method = "ward.D2") %>%
  as.dendrogram() %>%
  set("nodes_pch", c(19,1,4)) %>%
  set("nodes_cex", c(1,0.5,1)) %>%
  set("nodes_col", c(3,4)) %>%
  hang.dendrogram %>% # hang the leaves
  hang.dendrogram(hang = -1) %>%
  plot(main = "(1) Show (larger)\n nodes")




dend_data$leaf_labels <- list(leafs=bird_baths_half_numeric$bird_type)
dend_data$bioregions_labels <- list(bioregions=bird_baths_half_numeric$bioregions)


# View(dend_data)

leafs <- unlist(dend_data$leaf_labels)
class(leafs)

unique_leafs <-unique(leafs)



family <- "Arial Narrow"

p <- ggplot() +
  geom_segment(data=dend_data$segments,
               aes(x = x, y = y, xend = xend, yend = yend),color="grey80")+

  geom_point(data=dend_data$segments,aes(x = x, y = y),color="midnightblue") +
  
  #geom_text(data=dend_data$segments,aes(x = x, y = y,label=),color="red")+
  
  #geom_segment(data=my_seg,aes(x=x,y=y,xend=x,yend=yend-0.1),color="red")+

  #geom_text(data = my_leaf_label, aes(x, y, label = leafs),hjust = 1, angle = 0, size = 1,family=family)+

  #ylim(-3, 15) +
  #coord_polar(theta="y") +
  #coord_flip() +
  
  # dendextend
  ggdendro::theme_dendro()
  #theme(plot.background = element_rect(fill="lightgreen",color="midnightblue",size=2),
        #panel.background = element_rect(fill="steelblue",color="midnightblue",size=2),
        #plot.margin = margin(1,1,1,1,unit = "lines"))
print(p)






# https://rdrr.io/cran/ggdendro/man/dendro_data.html

if (require(cluster)) {
  model <- agnes(bb_piv_w, metric = "manhattan", stand = TRUE)
  dg <- as.dendrogram(model)
  ggdendrogram(dg)
}


if (require(cluster)) {
  model <- diana(bb_piv_w, metric = "manhattan", stand = TRUE)
  dg <- as.dendrogram(model)
  ggdendrogram(dg)
}








