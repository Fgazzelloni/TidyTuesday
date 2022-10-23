#TidyTuesday 2022 week41
# Author Federica Gazzelloni


# load the libraries
library(tidyverse)
library(igraph)
library(ggraph)
library(RColorBrewer)
library(showtext)
library(sysfonts)
library(extrafont)


# set the fonts
showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
font_add_google(name="Pangolin",family="pangolin")

# read the data
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# tidy textures
df <- yarn%>%
  mutate(texture_clean=case_when(str_detect(texture_clean,"merino")~"merino",
                                 str_detect(texture_clean,"ply|plied|play|plies")~"ply",
                                 str_detect(texture_clean,"acrylique|acrylic|polyacryl|acrilyc|acryt")~"acrylic",#TRUE~texture_clean))%>%count(texture_clean)%>%filter(str_detect(texture_clean,"acr|ply"))
                                 str_detect(texture_clean,"nylon")~"nylon",
                                 str_detect(texture_clean,"cotton")~"cotton",
                                 str_detect(texture_clean,"wool")~"wool",
                                 str_detect(texture_clean,"polyamide|polyamid")~"polyamid",
                                 str_detect(texture_clean,"angora")~"angora",
                                 str_detect(texture_clean,"cashmere")~"cashmere",
                                 str_detect(texture_clean,"aran")~"aran",
                                 str_detect(texture_clean,"silk")~"silk",
                                 str_detect(texture_clean,"jersey")~"jersey",
                                 TRUE~texture_clean))%>%#count(texture_clean)
  filter(str_detect(texture_clean,
                    c("merino|ply|acrylic|nylon|cotton|wool|angora|cashmere|aran|silk|jersey")))%>%
  count(texture_clean,yarn_weight_name,yardage,grams) %>%
  mutate(yarn_weight_name=case_when(yarn_weight_name=="Aran / Worsted"~"Aran",
                                    yarn_weight_name=="DK / Sport"~"DK",
                                    yarn_weight_name=="Light Fingering"~"Fingering",
                                    yarn_weight_name=="Super Bulky"~"Bulky",
                                    TRUE~yarn_weight_name))%>%
  filter(!yarn_weight_name=="No weight specified",!is.na(yarn_weight_name))%>%
  filter(!is.na(yardage),!is.na(grams))%>%
  select(-n)%>%
  group_by(yarn_weight_name,texture_clean)%>%
  summarise_all(.funs=mean)%>%
  select(yarn_weight_name,texture_clean,yardage)


# build the data ready for the graph
d1<- df%>%
  mutate(from="YARN")%>%
  relocate(from)%>%
  rename(to=yarn_weight_name)%>%
  select(-texture_clean,-yardage)%>%distinct()
d2 <- df%>%
  rename(from=yarn_weight_name, 
         to=texture_clean)%>%
  select(-yardage)%>%distinct()

hierarchy <- rbind(d1, d2)
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), 
                                       as.character(hierarchy$to))) ) 


# make the graph_from_data_frame
mygraph <- graph_from_data_frame(hierarchy, vertices=vertices )

# see the elements of the graph
df1 <- create_layout(mygraph, layout = 'dendrogram')


# make a function for node angle adj
# node_angle(df1$x,df1$y,degrees = T)
node_ang_adj <- function(x,y) {
  ifelse(node_angle(x,y) > 90 & node_angle(x,y) < 270 , 
         node_angle(x,y) + 180, node_angle(x,y))
  }

# make a function for hjust
node_hjust_adj <- function(x,y) {
  ifelse(node_angle(x,y) > 90 & node_angle(x,y) < 270 , 1,0)
}

# make the graph
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(aes(color=factor(x)),
                     alpha=0.9,
                     show.legend = F) +
  geom_node_point(aes(color=factor(x)),
                  size=10,
                  show.legend = F)+
  geom_node_point(aes(color=factor(x)),
                  size=10,
                  shape=8,
                  show.legend = F)+
  geom_node_label(aes(filter=!leaf,label=name,color=factor(x)),
                  label.padding = unit(0.1, "lines"),
                  label.r = unit(0.1, "lines"),
                  label.size = 0.1,
                  family = "pangolin",
                  fontface="bold",
                  show.legend = F,
                  size=4, 
                  alpha=1)+
  geom_node_text(aes(x = x*1.1, 
                     y=y*1.1, 
                     hjust = node_hjust_adj(x,y),
                     angle=node_ang_adj(x,y),
                     filter = leaf, 
                     label=name,
                     color=factor(x)),
                 family = "pangolin",
                 fontface="bold",
                 show.legend = F,
                 size=4, 
                 alpha=1)+
  scale_color_manual(values = rep(RColorBrewer::brewer.pal(10,"Paired"),10))+
  scale_x_discrete(expand = c(0,0.3))+
  scale_y_discrete(expand = c(0,0.3))+
  coord_fixed()+
  labs(caption="What's inside your YARN?\ntextures for each type\n\nDataSource: #TidyTuesday 2022 week41 Ravelry data\nDataViz: Federica Gazzelloni (FG) Twitter: @fgazzelloni\n",
       alt="Infographics") +
  theme_graph()+
  theme(plot.margin = margin(5,5,5,5,unit = "pt"),
        plot.caption = element_text(face="bold",family="pangolin"))



#  ggsave("w41_yarn.png",
#        dpi=280,
#        bg="white",
#        width = 9,height = 9)

