
## TidyTuesday US Post Office & 30 Rstats challenge - Space #################


### loading libraries ##################

library(tidytuesdayR)
library(tidyverse)
library(maptools)
library(data.table)

## loading data #####################################

tuesdata <- tidytuesdayR::tt_load(2021, week = 16)
post_offices <- tuesdata$post_offices

##### wrangling ##################################

head(post_offices)
summary(my_df)
glimpse(my_df)

plyr::count(df$stamp_index)

my_df<-post_offices%>%filter(established>=1639,established<=2000,
                          discontinued>=1775 ,discontinued<=2002,
                          !is.na(stamp_index),
                          !duration<0,
                          !is.na(gnis_dist),
                          !stamp_index==55,
                          !is.na(county1))%>%
  select("duration","gnis_dist","stamp_index")%>%
  group_by(stamp_index)%>%
  summarize(avg_duration=mean(duration),avg_dist=mean(gnis_dist))


############# setting data ready for plotting ######################

library(igraph)
mat <- cor(t(my_df))
mat[mat<0.995] <- 0
# Make an Igraph object from this matrix:
network <- graph_from_adjacency_matrix( mat, weighted=T, mode="undirected", diag=F)

# Basic chart
plot(network)

# color palette
library(RColorBrewer)
coul <- brewer.pal(nlevels(as.factor(my_df$stamp_index)), "Set3")

# Map the color to cylinders
my_color <- coul[as.numeric(as.factor(my_df$stamp_index))]

#### setting for saving plot ##################################

ragg::agg_png(here::here("US_Post_office_space.png"),
              res = 320, width = 14, height = 8, units = "in")

# plotting ############################################

par(bg="mediumblue", mar=c(1,1,1,1))
set.seed(4)
plot(network, 
     vertex.label.family="Georgia", 
     edge.curved=0.08,
     edge.width=2,                                 
     edge.arrow.size=1,                       
     edge.arrow.width=1,                          
     edge.lty="solid",
     vertex.size=12,
     vertex.shape=c("raster","sphere"), 
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="blue",
     vertex.frame.color="transparent"
)

op <- par(family = "Luminari")

#### legend and titles ########################

legend(x=1.3, y=0.7, 
       legend=paste( levels(as.factor(my_df$stamp_index)), "stamp*", sep="*"), 
       col = coul , 
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="white" , horiz = F)
text(-1.4,1.1,"US Post Offices",col="white", cex=1.5)
text(-1.3,1,"Stamps index by distance",col="white", cex=1.2)
text(-1.1,0.9,"visualization of the distance between offices by different stamps index",col="white", cex=0.8)

text(-1,-1.1,"Viz @fgazzelloni | #TidyTueasday Week 16 | Space Day 14 | DataSource: Harvard Dataverse",col="white", cex=0.8)
par(op)


## imaging ######################################

library(png)
library(grid)
library(magick)

tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("400x400")

mypng2<-readPNG("satellite2.png")
mypng3<-readPNG("satellite3.png")


grid.raster(tidy_logo, x=0.9, y=0.2, width=.08)
grid.raster(mypng2, x=.09, y=.7, width=.25)
grid.raster(mypng3, x=.9, y=.9, width=.25)

####### final ###################

dev.off()

################################################# end




