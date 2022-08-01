# TidyTuesday week 24 -

# Data Source: Great Lakes Fish,	Great Lakes Database,	Detroit Free Press
#database of all fish stocked from artificial propagation into the Great Lakes

##### libraries #######################
library(tidytuesdayR)
library(tidyverse)
library(stringr)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggridges)
library(patchwork)

# load data ##############################
tuesdata <- tidytuesdayR::tt_load(2021, week = 24)


stocked <- tuesdata$stocked
fishing <- tuesdata$fishing

# check #################################
head(stocked);dim(stocked)
head(fishing);dim(fishing)

library(DataExplorer)
profile_missing(fishing)

fishing$grand_total[is.na(fishing$grand_total)]<-0
fishing$values[is.na(fishing$values)]<-0

# wrangling ###########################
my_df <- fishing%>%
  arrange(year)%>%
  mutate(
    species=case_when(species=="Amercian Eel"~"American Eel",
              species=="Bullhead"~"Bullheads",
              species=="Channel catfish"~"Channel Catfish",
              species=="Cisco and chubs"~"Cisco and Chubs",
              species=="Cisco and Chub"~"Cisco and Chubs",
              species=="Crappie"~"Crappies",
              species=="Drum"~"Freshwater Drum",
              species=="Lake Trout - siscowet"~"Lake Trout",
              species=="Pacific salmon"~"Pacific Salmon",
              species=="White bass"~"White Bass",
              TRUE~species))


# plotting #################################

fig1 <- my_df %>%
  filter(!str_detect(species,"and")) %>%
  arrange(year)%>%
  mutate(text = fct_reorder(species, year )) %>%
  ggplot( aes(y=text, x=year,fill=species)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_x_continuous(breaks=seq(1867, 2015, 8))+
  labs(title="All Fisheries from the Great Lakes Fishery Commission",
       subtitle="from 1867 to 2015",
       tag = "Fig1")+
  xlab("") +
  ylab("Assigned Probability (%)")+
  theme_ft_rc()+
  theme(axis.text.x = element_text(angle=90),
    legend.position="none",
    plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8))


fig2 <- my_df%>%
  filter(!str_detect(species,"and")) %>%
  group_by(year,species)%>%
  summarize(total_number=sum(grand_total),total_production=sum(values))%>%
  ungroup()%>%
  arrange(total_number)%>%
  filter(!total_number==0)%>%
  mutate(percent=total_production/total_number)%>%
  filter(!percent>1)%>%
  arrange(year,percent)%>%
  filter(str_detect(species,("Lake")))%>%
  ggplot(aes(x=year,y=reorder(species,-total_production),group=species,color=species))+
  geom_line(aes(size=total_production))+
  labs(title="Lake Fisheries by Year",
       subtitle="sized by total value of production",
       tag="Fig2",
       size="Total value of production in $",
       x="",
       y="Species"
       #color="Species",#size="Percent",
       #caption="Viz @fgazzelloni, DataSource: TidyTuesday Week24 - Great Lakes Fish,Great Lakes Database,Detroit Free Press"
       )+
 scale_color_viridis(discrete = TRUE)+
  guides(color = FALSE)+
   scale_x_continuous(breaks=seq(1867, 2015, 18))+
  theme_ft_rc()+
  theme(axis.text.x = element_text(angle=0),
        legend.position = "top",
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

fig3 <- my_df%>%
  filter(!str_detect(species,"and")) %>%
  arrange(year)%>%
  filter(str_detect(species,("Lake")))%>%
  ggplot(aes(x=species, y=values, fill=species)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(colour=values), size=0.3, alpha=0.5) +
  scale_colour_viridis_c()+
  labs(tag = "Fig3",
       x="",y="Values",
       caption="Viz @fgazzelloni, DataSource: TidyTuesday Week24 - Great Lakes Fish,Great Lakes Database,Detroit Free Press")+
  theme_ft_rc() +
  theme(
    legend.position="none",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())







final <- fig1+(fig2/fig3)

final






###################### SAVING ############################


ragg::agg_png(here::here("w24","w24_fisheries.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()



#### ATTACHING LOGO ############################
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("w24/w24_fisheries.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "w24/w24_fisheries.png",
            format = "png") # save final plot



##############################################################











