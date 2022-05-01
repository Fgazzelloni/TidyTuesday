# TidyTuesday

TidyTuesday project is a weekly appointment that happens on every Tuesday for practicing making #DataVisualization with datasets provided by the #`R4DS Online Learning Community`

Several TidyTuesday interesting examples can be found in the main repository:

- https://github.com/rfordatascience/tidytuesday

***
#### How to make a #TidyTuesday (more info at the bottom of this page) 

***
### My contributions are posted on: 

#### Twitter @[fgazzelloni](https://twitter.com/fgazzelloni) and collected in this repository with related code.

***
#### Other #DataViz projects I contribute to:

##### - [30DayChartChallenge-2021](https://github.com/Fgazzelloni/rstats-chart-challenge-2021)
##### - [30DayChartChallenge-2022](https://github.com/Fgazzelloni/30DayChartChallenge)
##### - [30DayMapChallenge-2021](https://github.com/Fgazzelloni/30DayMapChallenge)


***
# My #TidyTuesdays
## [2021](data/2021) | [2022](data/2022)

| Week | Date | Data | my contribution 
| :---: | :---: | :--- | :--- 
| 1 | `2021-01-04` | Bring your own data to start 2022!() | ![png](data/2022/w1_your_own_data/your_own_data.png)
| 2 | `2022-01-11` | [Bee Colony losses](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md)|![png](data/2022/w2_bees/w2_bees.png)
| 3 | `2022-01-18` | [Chocolate Bar ratings](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-18/readme.md) |![png](data/2022/w3_chocolate/w3_chocolate.png)
| 4 | `2022-01-25` | [Board games](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-25/readme.md)|![png](data/2022/w4_board_games/w4_board_games.png)
| 5 | `2022-02-01` | [Dog breeds](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-01/readme.md)|![png](data/2022/w5_dogs/dog_prints_plot.png)
| 6 | `2022-02-08` | [Tuskegee Airmen](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-08/readme.md)|![png](data/2022/w6_airforce/w6_airforce.png)
| 7 | `2022-02-15` | [`#DuBoisChallenge2022`](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-15/readme.md)|![png](data/2022/w7_dubois/w7_number6.png)
| 8 | `2022-02-22` | [World Freedom index](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md)|![png](data/2022/w8_wfi/freedom.png)
| 9 | `2022-03-01` | [Alternative Fuel Stations](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md)|![png](data/2022/w9_stations/w9_stations.png)
| 10 | `2022-03-08` | [Erasmus student mobility](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-08/readme.md) |![png](data/2022/w10_erasmus/er-network.png)
| 11 | `2022-03-15` | [CRAN/BIOC Vignettes](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-15/readme.md)|![png](data/2022/w11_vignettes/w11_vignettes.png)
| 12 | `2022-03-22` | [Baby names](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-22/readme.md)|![png](data/2022/w12_babynames/w12_wordcloud.png)
| 13 | `2022-03-29` | [Collegiate Sports Budgets](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-29/readme.md)|![png](data/2022/w13_sports/w13_sports.png)
| 14 | `2022-04-05` | [Digital Publications](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-04-05/readme.md)|![png](data/2022/w14_digital_pub/w14_digital_publications_v2.png)
|15	| `2022-04-12` | [Indoor Air Pollution](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-04-12/readme.md)|![png](data/2022/w15_indoor_pollution/day12_theme_day.png)
|16	| `2022-04-19` | [Crossword Puzzles and Clues](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-04-19/readme.md)|![png](data/2022/w16_crossword/w16_crossword.png/day22_animate.gif)
|17	| `2022-04-26` | [Kaggle Hidden Gems](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-04-26/readme.md)| ![png](data/2022/w17_hidden_gems/day28_deviations.png)

***
## INFO: How to make a #TidyTuesday

- Go to [R4DataScience GitHub repository](https://github.com/rfordatascience/tidytuesday)
- import data found in the [README](https://github.com/rfordatascience/tidytuesday/blob/master/README.md) at the middle bottom of the page is a table with the most updated data provided for the year/week
- click on the corresponding data tab in the table 
- load the data, two options are available: 

    1. Install {tidytuesdayR} package from CRAN via: `install.packages("tidytuesdayR")`, then load the data as suggested assigning a tuesdata variable name using the `tt_load()` function:
    
        `tuesdata <- tidytuesdayR::tt_load("date")`
        
        `tuesdata <- tidytuesdayR::tt_load(year, week)`
    
    2. Import the data directly from the .csv file provided 
