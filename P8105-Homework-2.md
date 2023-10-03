P8105 Homework 2
================
Laylah Jones
2023-09-28

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

``` r
library(readxl)
library(dplyr)
library(tidyr)
```

### Part 1. First we will read and clean the pols-month.csv data.

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
  ) |>
  select(-prez_dem, -prez_gop, -day)
```

### Part 2. Now we will read and clean snp.csv data.

``` r
snp_df = 
  read.csv("./fivethirtyeight_datasets/snp.csv") |>
  janitor::clean_names() |>
  separate(date, c("month", "day", "year"), convert = TRUE) |>
  mutate(
    year = as.numeric(year),
    year=if_else(year<=20, year+2000, year+1900),
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
  )|>
  select(-day) |>
  select(year, month, close)
```

### Part 3. Lastly we will read and clean unemployment.csv data.

``` r
unemployment_df = 
  read.csv("./fivethirtyeight_datasets/unemployment.csv") |>
  janitor::clean_names() |>
  pivot_longer(
    jan:dec,
    names_to = "month",
    values_to = "unemployment rate"
  ) |>
  mutate(
    month = case_when(
      month == "jan" ~ "January",
      month == "feb" ~ "February",
      month == "mar" ~ "March",
      month == "apr" ~ "April",
      month == "may" ~ "May",
      month == "jun" ~ "June",
      month == "jul" ~ "July",
      month == "aug" ~ "August",
      month == "sep" ~ "September",
      month == "oct" ~ "October",
      month == "nov" ~ "November",
      month == "dec" ~ "December"
    )
  ) 
```

### Part 4. Merge data sets

``` r
pols_snp_merge = 
  left_join(pols_df, snp_df, by = c("year", "month"))

merge_df = 
  left_join(pols_snp_merge, unemployment_df, by = c("year", "month"))
```

### Part 5. Dataset explaination and description

The pols_df originally contained information about the presidency and
the number of national politicians who were either democratic or
republican at the time from 1947 to 2015, and was structured with a
variable labelled “mon” signifying the date in the format of
“YYYY-MM-DD”. By cleaning the data, the “mon” variable was separated
into integer variables for “year”, “month”, and “day”. The “month”
variable was then mutated from a numeric variable to the corresponding
month name. Additionally, a “president” variable was created using the
variables “gop” for Republican presidents and “dem” for Democratic
presidents, while removing the “prez_dem” and “prex_gop” variables. The
resulting pols_df dataset includes dimensions of 822 observed rows by 9
columns and key variables including “year”, “month”, “president” and
other variables related to presidency.

The snp_df originally contained information about the Standard & Poor
stock market index, with columns for “year”, “month”, “day”, and “close”
values at the time from 1950 to 2015. Cleaning consisted of arranging
the data by “year” and “month” to align with the other datasets, and
changing the year from “YY” to “YYYY”. The dataset includes dimensions
of 787 observed rows and 3 columns.

The unemployment_df originally contained information about the
percentage of unemployment from 1948 to 2015 in a “wide” format.
Cleaning involved converting the dataset from a “wide” to “long” format.
Key variables were mutated by changing it from lowercase to uppercase to
align with the pols_df. The dataset includes dimensions of 610 observed
rows by 3 columns. (6 rows were NA).

The merge_df dataset was created by merging the three datasets above,
using the left_join function. It includes dimensions of 822 observed
rows and 11 columns. Variables include month, year, information of the
presidency, number of national politicians who are democratic or
republican, closing value of s&p stock index, and the percentage of
unemployment at a given time, the entire dataset ranges from 1947 to
2015. Key variables include “President” that signifies the presidents’
political party, “Close”, and “Unemployment rate”.

# Problem 2

### Part 1. Read and clean Mr. Trash Wheel data

``` r
Mr_TrashWheel_df = 
  read_excel("./New Trash Wheel Data.xlsx", range = cell_cols("A:N"), sheet = "Mr. Trash Wheel") |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(name = "Mr Trash Wheel") |>
  mutate(homes_powered, homes_powered = weight_tons*500/30)
```

### Part 2. Read and clean Professor. Trash Wheel data

``` r
Prof_TrashWheel_df = 
  read_excel("./New Trash Wheel Data.xlsx", range = cell_cols("A:N"), sheet = "Professor Trash Wheel") |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(name = "Professor Trash Wheel") |>
  mutate(homes_powered, homes_powered = weight_tons*500/30)
```

    ## New names:
    ## • `` -> `...14`

### Part 3. Read and clean Gwynnda data

``` r
Gwynnda_TrashWheel_df = 
  read_excel("./New Trash Wheel Data.xlsx", range = cell_cols("A:N"), sheet = "Gwynnda Trash Wheel") |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(name = "Gwynnda Trash Wheel") |>
  mutate(homes_powered, homes_powered = weight_tons*500/30)
```

    ## New names:
    ## • `` -> `...13`
    ## • `` -> `...14`

### Part 4. Combine datasets

``` r
Combined_TrashWheel_df = 
  full_join(Prof_TrashWheel_df, Gwynnda_TrashWheel_df)
```

    ## Joining with `by = join_by(dumpster, month, year, date, weight_tons,
    ## volume_cubic_yards, plastic_bottles, polystyrene, cigarette_butts,
    ## plastic_bags, wrappers, homes_powered, x14, name)`

### Part 5. Describing the data

The Combined Trash Wheel data set is made up of the Mr. Trash Wheel,
Professor Trash Wheel, and Gwynnda Trash Wheel data sets. It consists of
261 rows and 16 columns. The total weight of the trash that was
collected by Professor Trash Wheel was \_\_\_\_ tons. And the total
weight of the trash that was collected by Professor Trash Wheel was
\_\_\_ tons.

# Problem 3

### Part 1. Import, clean, and tidy dataset

``` r
Baseline_df = 
  read_csv("./data_mci/mci_baseline.csv", skip = 1) |>
  janitor::clean_names() |>
  mutate(
    sex = case_match(
      sex,
      1 ~ "male",
      0 ~ "female"
    ),
    apoe4 = case_match(
      apoe4,
      1 ~ "carrier",
      0 ~ "non-carrier"
    )
  ) |>
  filter(age_at_onset == "." | age_at_onset > current_age)
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Age at onset
    ## dbl (5): ID, Current Age, Sex, Education, apoe4
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
