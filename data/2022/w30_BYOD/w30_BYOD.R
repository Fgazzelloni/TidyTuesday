
## TidyTuesday 2022 week30 BYOD 
# Rollback #TidyTuesday 2021 week33 BEA Infrastructure Investment 
# @BEA_News

# more in this folder: https://github.com/Fgazzelloni/TidyTuesday/tree/main/data/2021/w33_bea_Infrastructure_Investment

library(tidyverse)

investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')


investment1 <- investment %>%
  mutate(meta_cat = case_when(meta_cat=="Total basic infrastructure" ~ "Basic",
                              TRUE~meta_cat )) %>%
  mutate(category = case_when(
    category=="Private communications equipment in NAICS 515, 517, 518, and 519"~"Private communications equipment",
    category=="Private computers in NAICS 515, 517, 518, and 519"~"Private computers",
    category=="Office buildings, NAICS 518 and 519"~"Office buildings",
    category=="Private software in NAICS 515, 517, 518, and 519"~"Private software",
    TRUE~category)) %>%
  filter(gross_inv>=500) %>%
  group_by(category) %>%
  summarize(tot_gross_inv=round(sum(gross_inv)))%>%
  ungroup() %>%
  arrange(-tot_gross_inv) 
  
investment1 %>%
  ggplot(aes(x=fct_reorder(category,tot_gross_inv),y=(tot_gross_inv),group=category)) +
  geom_histogram(aes(fill=category),stat = "identity", position=position_dodge(width=0.8),size=0.8,alpha=0.5,bins = 50)+ 
  geom_text(aes(label=scales::dollar(tot_gross_inv)),size=1.5,hjust=0) +
  guides(color="none",fill="none") +
  labs(title="US Investment categories based on total gross investments", 
       subtitle="70-year period from 1947 to 2017\n",
       caption="\nBEA: measurement of infrastructure in the U.S. National Economic Accounts (NEAs)\n \nInfographic: @fgazzelloni\n DataSource: TidyTuesday Week33: BEA Infrastructure Investment",
       x="Investment Categories",y="Total Gross Investment (log10 tranformation)") +
  coord_flip() +
  scale_y_log10(labels=scales::dollar,expand = expansion(mult = c(0, .3))) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle=0,size=8,hjust=0),
        axis.text.y = element_text(size=5,hjust=1),
        axis.title.y = element_text(vjust=4),
        axis.title.x = element_text(vjust=-2),
        axis.ticks.x = element_line(size=1,color="darkred"),
        axis.ticks.y = element_line(size=0.2,color="darkred"),
        axis.ticks.length=unit(.5, "cm"),
        plot.title.position = "plot",
        plot.caption = element_text(vjust=-5,size=6),
        plot.caption.position = "plot",
        plot.subtitle = element_text(vjust=-2,hjust=0),
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        plot.background = element_rect(fill = "grey90",colour = "grey",size = 2)
  ) +
  annotate("text",label="The Private sector\nshows the highest level of \nincrease in investments \nwithin the last 70 years.\n Private total gross investments is \nfollowed by S&L pensions,\n investments in basic, \nand social infrastructures. \nDigital infrastructure and transports \nare before Power, Health , \nHighways and finally Education \nwith about 2 000 000 millions $ total investment.",x=25,y=100000000,size=2)

