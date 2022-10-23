library(tidyverse)

# libraries for dendrograms
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
bioregions_id <- bird_baths %>% count(bioregions) %>% mutate(bioregions_id = row_number()) %>% select(-n)
bird_type_id <- bird_baths %>% count(bird_type) %>% mutate(bird_type_id = row_number()) %>% select(-n)



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
  arrange(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) #%>% #View()
#mutate(n_prop = log(round(n/sum(n)*100,2)))


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
        bird_type,bird_type_id) %>% #,sort=TRUE) %>% View()
  arrange(survey_year) #%>% #View()
#filter(n>=27)

#------

matrix <- bird_baths_half_numeric[1:5,c(7,9)]

matrix <- column_to_rownames(matrix, var = "bird_type")

matrix %>%
  scale() %>%
  dist() %>%
  hclust() %>%
  plot()

#--------

# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html#the-3-clusters-from-the-complete-method-vs-the-real-species-category


bird_baths_bio <- bird_baths_numeric[,-3]

bioregions_labels<- data.frame(bird_baths_half_numeric) %>%
  mutate(bioregions=as.factor(bioregions)) 

bioregions_labels <- bioregions_labels[,5]

class(bioregions_labels)

library(colorspace) # get nice colors

bioregions_col <- rev(rainbow_hcl(10))[as.numeric(bioregions_labels)]


pairs(bird_baths_bio, col = bioregions_col,
      lower.panel = NULL,
      cex.labels=2, pch=19, cex = 1.2)
# Add a legend
par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
       legend = as.character(levels(bioregions_labels)),
       fill = unique(bioregions_col))
par(xpd = NA)



# http://blog.safaribooksonline.com/2014/03/31/mastering-parallel-coordinate-charts-r/
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = .8)
MASS::parcoord(bird_baths_bio, col = bioregions_col, 
               var.label = TRUE, lwd = 2)




d_bird <- dist(bird_baths_bio,method="man") # method="man" # is a bit better
hc_bird <- hclust(d_bird, method = "ward.D2")
bioregions_labels_leg <- rev(levels(bioregions_labels))

library(dendextend)
dend <- as.dendrogram(hc_bird)
plot(dend)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=10) #, groupLabels=bioregions)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(10)[sort_levels_values(
    as.numeric(bioregions_labels)[order.dendrogram(dend)]
  )]

# type to the labels:
labels(dend) <- paste(as.character(bioregions_labels)[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.1)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered bird data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = 0.05))
legend("topleft", legend = bioregions_labels_leg, fill = rainbow_hcl(10))

#----

hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", 
                    "median", "centroid", "ward.D2")

d_bird <- dist(bird_baths_bio,method="man") # method="man" # is a bit better
hc_bird <- hclust(d_bird, method = "ward.D2")
bioregions_labels_leg <- rev(levels(bioregions_labels))


bird_dendlist <- dendlist()

for(i in seq_along(hclust_methods)) {
  hc_bird <- hclust(d_bird, method = hclust_methods[i])   
  bird_dendlist <- dendlist(bird_dendlist, as.dendrogram(hc_bird))
}
names(bird_dendlist) <- hclust_methods
bird_dendlist

bird_dendlist_cor <- cor.dendlist(bird_dendlist)
bird_dendlist_cor

bird_dendlist_cor_spearman <- cor.dendlist(bird_dendlist, method_coef = "spearman")
corrplot::corrplot(bird_dendlist_cor_spearman, "pie", "lower")

#-------








