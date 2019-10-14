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

``` r
instacart %>%
  filter(aisle == c("baking_ingredients","dog_food_care","packaged_vegetable_fruits")) %>%
  group_by(aisle, product_name) %>%
  summarize( 
    n_obs = n()) %>%
  filter(min_rank(desc(n))<4) %>%
  knitr::kable(col.names = c("Aisle", "Product", "Number"))
```

| Aisle | Product | Number |
| :---- | :------ | -----: |

``` r
##Created table looking at three most popular items in each of the specified aisls.  
```

``` r
instacart %>%
  filter(product_name %in% c("Pink Lady Apple", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(
    mean_hr_day = mean(order_hour_of_day)) %>%
  pivot_wider(
    names_from = "order_dow", 
    values_from = "mean_hr_day") %>%
  knitr::kable(col.names = c("Product", 
                             "Sunday", 
                             "Monday", 
                             "Tuesday", 
                             "Wednesday", 
                             "Thursday", 
                             "Friday", 
                             "Saturday"))
```

| Product          |   Sunday |   Monday |  Tuesday | Wednesday | Thursday |   Friday | Saturday |
| :--------------- | -------: | -------: | -------: | --------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 |  15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apple  | 14.40000 | 14.20000 | 13.20000 |   8.00000 | 11.00000 | 16.00000 | 13.00000 |

``` r
##The 2x7 table shows the mean hour of day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week. Coffee Ice Cream seems to be ordered most between 1-3 pm and Pink Lady Apples mostly between 1-4 pm (although Wednesday is a bit different with a mean of 8 am).
```

``` r
data("brfss_smart2010")

brfss_smart2010 %>%
  filter(Response == c("Poor", "Fair", "Good", "Very Good", "Excellent")) %>%
  mutate(Response = ordered(Response, c("Poor", "Fair", "Good", "Very Good", "Excellent"))) %>%
  janitor::clean_names()
```

    ## Warning in Response == c("Poor", "Fair", "Good", "Very Good", "Excellent"):
    ## longer object length is not a multiple of shorter object length

    ## # A tibble: 1,693 x 23
    ##     year locationabbr locationdesc class topic question response
    ##    <int> <chr>        <chr>        <chr> <chr> <chr>    <ord>   
    ##  1  2010 AL           AL - Jeffer… Heal… Over… How is … Good    
    ##  2  2010 AL           AL - Mobile… Heal… Over… How is … Fair    
    ##  3  2010 AL           AL - Tuscal… Heal… Over… How is … Poor    
    ##  4  2010 AZ           AZ - Marico… Heal… Over… How is … Excelle…
    ##  5  2010 AZ           AZ - Pima C… Heal… Over… How is … Fair    
    ##  6  2010 AZ           AZ - Pinal … Heal… Over… How is … Good    
    ##  7  2010 AR           AR - Benton… Heal… Over… How is … Fair    
    ##  8  2010 AR           AR - Pulask… Heal… Over… How is … Poor    
    ##  9  2010 AR           AR - Washin… Heal… Over… How is … Excelle…
    ## 10  2010 CA           CA - Alamed… Heal… Over… How is … Good    
    ## # … with 1,683 more rows, and 16 more variables: sample_size <int>,
    ## #   data_value <dbl>, confidence_limit_low <dbl>,
    ## #   confidence_limit_high <dbl>, display_order <int>,
    ## #   data_value_unit <chr>, data_value_type <chr>,
    ## #   data_value_footnote_symbol <chr>, data_value_footnote <chr>,
    ## #   data_source <chr>, class_id <chr>, topic_id <chr>, location_id <chr>,
    ## #   question_id <chr>, respid <chr>, geo_location <chr>

``` r
##Cleaning the brfss_smart2010 dataset by focusing on the "Overall Health" topic and including responses from "Excellent" to "Poor". Responses were also ordered from "Poor" to "Excellent" and variable names are formatted appropiately.
```
