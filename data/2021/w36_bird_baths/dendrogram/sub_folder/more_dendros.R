
#----------
library(tidymodels)
tidymodels_prefer()
set.seed(123)
split <- initial_split(bird_baths_numeric,strata=bird_type_id)
train <- training(split)
test <- testing(split)

# test%>%count(urb_rul_id)

dd <- dist(scale(test), method = "manhattan")
hc <- hclust(dd, method = "ward.D2")
plot(hc)

dend <- as.dendrogram(hc)
plot(dend)

dend_data <- dendro_data(dend, type = "rectangle")
#dend_data$segments
#dend_data$labels
#---------
set.seed(345)
split2 <- initial_split(bird_baths_half_numeric,strata=bird_type_id)
train2 <- training(split2)
test2 <- testing(split2)

#-----------------
set.seed(444)
split <- initial_split(bird_baths_numeric,
                       prop = 1/2)
train <- training(split);dim(train)
test <- testing(split);dim(test)

train <- train %>% dist %>% hclust %>% as.dendrogram
test <- test %>% dist %>% hclust %>% as.dendrogram
d_train_test <- dendlist(train = train, test = test)

d_train_test %>% cor.dendlist

d_train_test %>% cor.dendlist(method_coef = "spearman")

Bk_plot(train, test, k = 2:30, xlim = c(2,30))

require(dendextend)
pre_tang_d_train_test <- d_train_test %>%
  ladderize %>% # untangle %>%
  set("branches_k_color", k = 7)


train_branches_colors <- get_leaves_branches_col(pre_tang_d_train_test$train)

pre_tang_d_train_test %>%
  tanglegram(fast = TRUE, color_lines = train_branches_colors)

d_train_test_common <- d_train_test %>% prune_common_subtrees.dendlist

d_train_test_common %>% untangle %>%
  tanglegram(common_subtrees_color_branches = TRUE)

#----------

#-------
set.seed(555)
split <- initial_split(bird_baths_numeric,
                       prop = 3/4,
                       strata = survey_year_id)
train <- training(split);dim(train)
test <- testing(split);dim(test)

#-----
train <- train %>% dist(method="manhattan") %>% hclust(method = "ward.D") %>% as.dendrogram
test <- test %>% dist(method="manhattan") %>% hclust(method = "ward.D") %>% as.dendrogram
d_train_test <- dendlist(train = train, test = test)

d_train_test %>% cor.dendlist

d_train_test %>% cor.dendlist(method_coef = "spearman")

Bk_plot(train, test, k = 2:30, xlim = c(2,30))

require(dendextend)
pre_tang_d_train_test <- d_train_test %>%
  ladderize %>% # untangle %>%
  set("branches_k_color", k = 10)


train_branches_colors <- get_leaves_branches_col(pre_tang_d_train_test$train)

pre_tang_d_train_test %>%
  tanglegram(fast = TRUE, color_lines = train_branches_colors)
d_train_test_common <- d_train_test %>% prune_common_subtrees.dendlist
d_train_test_common %>% untangle %>%
  tanglegram(common_subtrees_color_branches = TRUE)

set_graph_style(plot_margin = margin(1,1,1,1))


hierarchy0 <-
  as_tbl_graph(
    hclust(
      dist(bird_baths_numeric,method = 'manhattan'),method = 'ward.D2')) %>%

  # To map over the nodes in the original direction use map_bfs()
  mutate(Class = map_bfs_back_chr(
    node_is_root(),
    .f = function(node, path, ...) {
      if (leaf[node]) {
        as.character(bird_baths_half_numeric$bioregions[as.integer(label[node])])
      } else {
        bioregions <- unique(unlist(path$result))
        if (length(bioregions) == 1) {
          bioregions
        } else {
          NA_character_
        }
      }
    }))


ggraph(hierarchy0, "dendrogram", height = height) +
  geom_edge_elbow()

ggraph(hierarchy, "dendrogram", height = height) +
  geom_edge_elbow2(aes(color = node.Class))

#coord_polar(theta="y")

# ggraph(hierarchy, "dendrogram", height = height) +
# geom_edge_elbow2(aes(color = Class))


hierarchy <- test %>%
  dist(method="manhattan") %>%
  hclust(method = "ward.D2") %>%
  as_tbl_graph() %>%  #activate(nodes) %>%
  mutate(survey_year = sample(test2$survey_year, n(), TRUE),
         urban_rural = sample(test2$urban_rural, n(), TRUE),
         bioregions = sample(test2$bioregions, n(), TRUE),
         bird_type = sample(test2$bird_type, n(), TRUE)
  ) %>%
  activate(edges) %>%
  mutate(survey_year = sample(test2$survey_year, n(), TRUE),
         urban_rural = sample(test2$urban_rural, n(), TRUE),
         bioregions = sample(test2$bioregions, n(), TRUE),
         bird_type = sample(test2$bird_type, n(), TRUE)
  )


hierarchy2 <- bird_baths_numeric_short %>%
  dist(method="manhattan") %>%
  hclust(method = "ward.D2") %>%
  as_tbl_graph() %>%  #activate(nodes) %>%
  mutate(survey_year = sample(bird_baths_half_numeric_short$survey_year, n(), TRUE),
         urban_rural = sample(bird_baths_half_numeric_short$urban_rural, n(), TRUE),
         bioregions = sample(bird_baths_half_numeric_short$bioregions, n(), TRUE),
         bird_type = sample(bird_baths_half_numeric_short$bird_type, n(), TRUE)
  ) %>%
  activate(edges) %>%
  mutate(survey_year = sample(bird_baths_half_numeric_short$survey_year, n(), TRUE),
         urban_rural = sample(bird_baths_half_numeric_short$urban_rural, n(), TRUE),
         bioregions = sample(bird_baths_half_numeric_short$bioregions, n(), TRUE),
         bird_type = sample(bird_baths_half_numeric_short$bird_type, n(), TRUE)
  )

#  color_branches(dend, k=10)


ggraph(hierarchy2, "dendrogram", height = height) +
  geom_edge_elbow2(aes(color = node.urban_rural))+
  geom_edge_elbow(aes(alpha = stat(index)))

geom_edge_elbow2(aes(edge_colour = urban_rural))
# geom_edge_point(aes(label = survey_year))
# geom_edge_link(aes(label = survey_year))
# geom_edge_elbow0(aes(label = survey_year))
# coord_polar()



hierarchy


ggraph(hierarchy, layout = "partition") +
  geom_edge_diagonal() +
  geom_node_point(aes(size = leaf),show.legend = FALSE) +
  geom_node_text(aes(label = bird_type),position = "stack",vjust=-0.9)

#-----

ggraph(hierarchy2, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal() +
  geom_node_point(aes(filter = leaf)) +
  geom_node_label(aes(label = bird_type,color=factor(bioregions)), repel = TRUE) +
  coord_fixed()

#------

ggraph(hierarchy, layout = 'graphopt') +
  geom_edge_link(aes(start_cap = label_rect(node1.bioregions),
                     end_cap = label_rect(node2.bioregions)),
                 arrow = arrow(length = unit(4, 'mm'))) +
  geom_node_text(aes(label = bird_type, color=bioregions))+
  geom_node_text(aes(label = bioregions),col="red",vjust=1.8)


hierarchy3 <- bb_piv_w %>%
  dist(method="manhattan") %>%
  hclust(method = "ward.D2") %>%
  as_tbl_graph() %>%  #activate(nodes) %>%
  mutate(
    bird_type = sample(bb_piv_w$bird_type, n(), TRUE)
  ) %>%
  activate(edges) %>%
  mutate(bird_type = sample(bb_piv_w$bird_type, n(), TRUE)
  )





