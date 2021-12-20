library(tidyverse)

studio_album_tracks <- readr::read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/studio_album_tracks.csv")


studio_album_tracks%>%select(contains("key"))%>%distinct()%>%arrange(key)

df<-studio_album_tracks%>%select(danceability,
                                 year=album_release_year,
                                 #mode,
                                 energy,
                                 loudness,
                                 speechiness,
                                 acousticness,
                                 instrumentalness,
                                 liveness,valence,duration_ms,tempo)

ugly <- ggthemr::define_palette(
  swatch = c('black', 'red', 'green', 'blue', 'brown', 'purple', 'yellow'), 
  gradient = c(lower = 'red', upper = 'green')
)

ggthemr::ggthemr(ugly)

library(extrafont)
loadfonts()
family="Impact"

violins<-df%>%
  select(-year,-instrumentalness)%>%
  #recipe()%>%
  #step_normalize(all_numeric())
  scale()%>%as.data.frame()%>%mutate(year=df$year)%>%
  #mutate(duration_ms=duration_ms/sum(duration_ms))%>%#count(duration_ms)
  pivot_longer(cols=c(1:8),names_to="variables",values_to="values")%>%
  #pull(values)%>%summary()
  mutate(variables=tools::toTitleCase(variables),
         variables=case_when(variables=="Duration_ms"~"Duration in ms",
                             TRUE~variables))%>%
  #mutate(across(variables, factor, levels=c("")
  ggplot(aes(x=factor(year),y=values,fill=variables))+
  geom_boxplot(alpha=0.7)+
  geom_violin(alpha=0.5)+
  geom_point(color="gold",size=0.2)+
  facet_wrap(vars(variables),scales="free",nrow = 2)+
  scale_fill_brewer(type = "seq", palette = "Spectral")+
  guides(fill="none")+
  labs(x="",title="")+
  theme(text = element_text(family=family),
        axis.title.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(size=0.3),
        strip.text = element_text(color="hotpink",size=12),
        strip.background = element_rect(color="black",fill="black"),
        axis.line = element_line(color="hotpink",size=2),
        axis.text.x = element_text(color="gold",size=10),
        axis.text.y = element_text(color="darkolivegreen2",size=10))

#violins
bg<-ggplot()+
  geom_blank()+
  theme_void()+
  theme(plot.background = element_rect(color="black",fill="black"))

library(cowplot)

final<-ggdraw()+
  draw_plot(bg)+
  draw_image("w51/spice_bg.png",scale = 1,
             y=0.2) +
 # draw_image("w51/spices_image.jpg",scale = 1.2) +
            # scale=0.4,x=0.25,y=-0.28)+
  draw_plot(violins,height = 0.55,y=-0.04)+
  draw_label("What Makes a Song Likeable?",
             x=0.5,y=0.95,size=42,color="gold",
             fontfamily = family,fontface = "bold")+
  draw_label("What Makes a Song Likeable?",
             x=0.5,y=0.95,size=41,color="hotpink",
             fontfamily = family,fontface = "bold")+
  draw_label("DataViz: Federica Gazzelloni",
             angle=90,size=9,x=0.02,y=0.8,
             fontfamily=family)+
  draw_label("DataSource: Spice Girls by Jacquie Tran",
             angle=0,size=9,x=0.8,y=0.49,
             fontfamily=family)+
  
  draw_label("1996",
             angle=15,size=25,x=0.5,y=0.87,
             fontfamily=family)+
  draw_label("1997",
             angle=15,size=23,x=0.65,y=0.83,
             fontfamily=family)+
  draw_label("2000",
             angle=15,size=27,x=0.8,y=0.85,
             fontfamily=family)+
  
  
  draw_label("1996",color="white",
             angle=15,size=24,x=0.5,y=0.87,
             fontfamily=family)+
  draw_label("1997",color="white",
             angle=15,size=22,x=0.65,y=0.83,
             fontfamily=family)+
  draw_label("2000",color="white",
             angle=15,size=26,x=0.8,y=0.85,
             fontfamily=family) +
  draw_label("scaled values",color="white",
             angle=0,size=6,x=0.96,y=0.46,
             fontfamily=family)


####### SAVING ######################################
ragg::agg_png(here::here("w51/spicegirls.png"),
              res = 320, width = 8, height = 8, units = "in")
final

dev.off()

