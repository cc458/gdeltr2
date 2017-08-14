gdeltr2::load_needed_packages(c("tidyr", "dplyr", "rlang", "highcharter", "trelliscopejs", "anytime", "lubridate", "purrr", "purrrlyr", "tibble", "glue", "stringr", "jsonlite"))

terms = c('"Pierre Gooding"', 'Pierre Gooding', "Bill Perkins", '"Bill Perkins"',
          'Harlem District 9 city council', '"Marvin Holland"', '"District 9" Harlem')
domains = c("gothamgazette.com", "cityandstateny.com/")
source('~/Desktop/r_packages/gdeltr2/R/gdelt_free_text_api.R')
setwd("~")
base_path = "Desktop/abresler.github.io/trelliscopes/pierre/"
