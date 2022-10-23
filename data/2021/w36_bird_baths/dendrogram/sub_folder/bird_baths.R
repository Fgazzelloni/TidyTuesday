iris[1:30,-5]
dend <- iris[1:30,-5] %>% scale %>% dist %>%
  hclust %>% as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
# plot the dend in usual "base" plotting engine:
plot(dend)

# bioregions = 10

bird_baths_numeric%>%count(bird_type)
dend <-bird_baths_numeric %>% drop_na() %>% #count(bioregions_id)
  #select(-2) %>%
  group_by(survey_year,urb_rul_id,bioregions_id) %>%
  summarise(tot_bird_count=sum(bird_count),.groups="drop") %>%

  scale %>%
  dist %>%
  hclust %>%
  as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))


plot(dend)
ggd1 <- as.ggdend(dend)
ggplot(ggd1)

ggplot(ggd1, labels = TRUE) +
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

den2 <- bird_baths_numeric %>%
  drop_na() %>%
  group_by(bird_type) %>%
  summarize(tot_bird_count=sum(bird_count),.groups="drop") %>%
  arrange(-tot_bird_count)

den2 <- den2[1:30,] %>%
  left_join(bird_baths_numeric,by="bird_type") %>%
  select(bird_type_id.y,tot_bird_count) %>%
  scale %>%
  dist %>%
  hclust %>%
  as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))









