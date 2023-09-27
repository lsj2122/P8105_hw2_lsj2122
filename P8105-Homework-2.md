P8105 Homework 2
================
Laylah Jones
2023-09-27

# Problem 1

Importing libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Part 1. First we will read and clean the pols-month.csv data.

``` r
pols_df = 
  read.csv("./fivethirtyeight_datasets/pols-month.csv") |>
  janitor::clean_names() |>
  separate(mon, c("year", "month", "day"), convert = TRUE) |>
  mutate(
    month = case_when(
      month == 1 ~ "January",
      month == 2 ~ "February",
      month == 3 ~ "March",
      month == 4 ~ "April",
      month == 5 ~ "May",
      month == 6 ~ "June",
      month == 7 ~ "July",
      month == 8 ~ "August",
      month == 9 ~ "September",
      month == 10 ~ "October",
      month == 11 ~ "November",
      month == 12 ~ "December"
    )
  ) |>
  mutate(
    president = case_when(
      prez_dem == 1 ~ "dem",
      prez_gop == 1 ~ "gop"
    )
  )
```
