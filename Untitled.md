p8105\_hw3\_ay2452
================
Alicia Yang
10/14/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
 
data("instacart")

instacart %>%
  janitor::clean_names() %>%
  group_by(aisle) %>%
  summarize( 
    n_obs = n()) %>% view
    
##The "instacart" dataset has 1384617 observations and 15 variables. Key variables include the order id, user id, time of order (both by day of the week and hour of day), product ordered, product department, aisle, and their associated ids. As is, the dataset is structured so that items in an order are grouped together.
##There are 134 total aisles. Aisles from which the most items are ordered from are (in descending order): fresh veggies, fresh fruits, packaged vegatble fruits, yogurt, and packaged cheese.
```

``` r
instacart %>%
  group_by(aisle) %>%
  summarize( 
    n_obs = n()) %>%
  filter(
    n_obs > 10000
  ) %>%
  ggplot(aes(x = reorder(aisle, n_obs), y = n_obs)) + 
  geom_bar(stat = "identity") + coord_flip() + labs(
    title = "Number of Items Ordered from Aisles in Instacart", 
    x = "Aisle", 
    y = "Observations"
  ) %>% view
```

![](Untitled_files/figure-gfm/Problem%201.2-1.png)<!-- -->

``` r
##The plot shows the different aisles and the number of items ordered from each aisle in descending order from the top. Fresh vegetables and fresh fruits are very close and greatly outnumber the other aisles. 
```
