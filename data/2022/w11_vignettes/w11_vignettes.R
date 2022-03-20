
library(tidyverse)
library(ggridges)
library(showtext)
library(sysfonts)
library(extrafont)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)

font_families_google()

font_add_google(name="Overpass",family="overpass")

bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')


head(bioc)
head(cran)

bioc2 <-bioc%>%mutate(date=as.Date(date,"%Y-%m-%d"))

cran_a <-cran%>%
  mutate(date=format(as.Date(date,"%a %b %d %H:%M:%S %Y"), format="%Y-%m-%d"),
         date=as.Date(date,"%Y-%m-%d")) %>%
  drop_na()

cran_b <-cran%>%
  mutate(date=as.Date(date,"%Y-%m-%d")) %>%
  drop_na()


class(cran_a$date)
class(cran_b$date)

cran2 <- rbind(cran_a,cran_b) %>%
  select(-version)

cran2%>%DataExplorer::profile_missing()

head(cran2)
head(bioc2)

cran2 <- cran2 %>% 
  mutate(source = "Cran\n") %>%
  relocate(date,rnw,rmd,package,source)

bioc2 <- bioc2 %>%
  mutate(source = "Bioconductor\n")

dim(cran2);dim(bioc2)


df <- rbind(cran2,bioc2) %>%
  distinct()


df %>%
  mutate(year=lubridate::year(date)) %>%
  distinct() %>%
  count(year,source,sort=T) %>%
  arrange(year) %>%
  ggplot(aes(x=n, y=factor(year),# height = ..density..,
             fill=factor(year))) +
  geom_density_ridges(scale = 0.8, alpha=0.6, 
                      #rel_min_height = 0.1,
                      stat="binline", bins=30) +
  facet_wrap(~source,scales = "free")+
  labs(caption="DataSource: CRAN/BIOC Vignettes | Robert Flight GitHub | #TidyTuesday week 11 2022 | DataViz: Federica Gazzelloni") +
  theme_ridges() +
  theme(text = element_text(family="overpass",size=15),
        plot.caption = element_text(size=8,hjust = 0.3),
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 28),
        strip.background = element_rect(fill="grey86"),
        plot.background = element_rect(color="grey86",fill="grey86"),
        panel.background = element_rect(color="grey86",fill="grey86")) +
  xlab("CRAN/BIOC Vignettes: number of releases per Year") +
  ylab("Assigned Probability (%)")


ggsave("w11_vignettes.png",width = 9.79 , height =  9.46)
