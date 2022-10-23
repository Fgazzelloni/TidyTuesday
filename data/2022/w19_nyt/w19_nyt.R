
# TidyTuesday 2022 week 19 - NYT
# source: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-10/readme.md

# Housekeeping: clean the space before to start and set the working dorectory to your .R file source
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the libraries
library(tidyverse)
library(forcats)
library(ggridges)
library(showtext)
library(cowplot)

# set the font
showtext_auto(enable = T)
sysfonts::font_families_google()
sysfonts::font_add_google("Abril Fatface", "Abril Fatface")


# load data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# look at the data
nyt_titles%>%head
nyt_full%>%head

# a bit of wrangling
df <- nyt_titles %>%
  full_join(nyt_full,by=c("year","title","author"))

df2 <- df %>%
  mutate(year_fct = fct_rev(as.factor(year))) %>% # 
  filter(!str_detect(author,"Edited|edited|created|compiled|Completed|NO AUTHOR| and |Illustrated| with |translated"))%>%
  mutate(author=gsub("! by |? by |?by |\\?|\"|, Jr| Jr|\\.$| writing as.*","",author)) %>%
  group_by(author) %>%
  summarise(year_fct,
            avg_rank=mean(rank),
            scale=scale(rank,center = F),
            scale_pct=scale/sum(scale),
            avg_rank_pct=avg_rank/sum(avg_rank),
            id=n()) %>%
  ungroup()

# set the dataset for the geom_text labels
side_labels <- df2 %>%
  group_by(year_fct,author)%>%
  summarize(top=max(scale),scale_pct=mean(scale_pct))%>%
  distinct()%>%
  filter(top==max(top))%>%
  mutate(lab=paste(author,"in",year_fct),
         lab2=paste(author,"ranked",round(top),"on avg in",year_fct))%>%
  ungroup() %>%
  select(year_fct,lab,scale_pct)%>%
  arrange(desc(year_fct))



# make the plot
df2 %>% 
  # reorder different authors within the same year along with the percentages values
  # this will reorder the density courves for each year 
  mutate(author=fct_reorder(author,scale_pct)) %>%
  ggplot(aes(x=scale_pct, y=year_fct)) +
  geom_density_ridges(aes(fill=author),
                      show.legend = F,
                      size=0.3,
                      scale=1,
                      alpha = .8, 
                      color = "grey25", 
                      from = 0, to = 1) +
  geom_label(data=side_labels,
             aes(x=0.5,y=year_fct,label=lab),
             label.padding = unit(0.05, "lines"),
             label.r = unit(0.5, "lines"),
             label.size = 0,
             family="Abril Fatface",size=10,
             inherit.aes = F,hjust=0,vjust=0)+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = expansion(mult=c(0,-0.35),
                                        add=c(0, -0.02))) +
  scale_fill_grey(
    start = 0.2,
    end = 0.9,
    na.value = "red",
    aesthetics = "fill") +
  labs(title="The New York Times",
       subtitle="Solo author ranks from 1931 to 2020",
       caption="DataSource: Post45 Data Collective NYT HARDCOVER FICTION BESTSELLERS\nDataViz: Federica Gazzelloni (@fgazzelloni)",
       x="Rank density",y="Year") +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)+
  theme(text = element_text(family="Abril Fatface",size=45),
        plot.title = element_text(size=90),
        plot.caption = element_text(hjust=1),
        axis.text.y = element_text(size=30,hjust=0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(color="grey90",fill="grey90"),
        panel.background = element_rect(color="grey90",fill="grey90"))

# save first partial version
ggsave("partial.png",
       dpi=320,
       height = 14,
       width =  10)

# frame the graphics and add a notation with {cowplot}
# it helps reducing time when setting the text position.
ggdraw()+
  draw_image("partial.png") +
  draw_label("How to read it: 
On average, authors rank 7.6 based on weekly frequencies on NYT, 
which corresponds to 3.4% of the total scaled avg-ranks.
Each year shown in the graph represents the density curve of the 
ranks for the NYT's solo authors in that year.
The density curves are ordered by total percentage of scaled ranks.
On the right is the author with the avg-weekly highest rank for the year.",
             lineheight = 0.25,hjust=0,
              x=0.04,y=0.05,fontfamily="Abril Fatface",size=25)

# save the final version
ggsave("w19_nyt.png",
       dpi=320,
       height = 12,
       width =  9)

#####--------#####
