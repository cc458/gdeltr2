gdeltr2::load_needed_packages(c("tidyr", "dplyr", "rlang", "highcharter", "trelliscopejs", "anytime", "lubridate", "purrr", "purrrlyr", "tibble", "glue", "stringr", "jsonlite"))

terms = c('"Pierre Gooding"', 'Pierre Gooding', "Bill Perkins", '"Bill Perkins"',
          'Harlem District 9 city council', '"Marvin Holland"', '"District 9" Harlem')
domains = c("gothamgazette.com", "cityandstateny.com/")
source('~/Desktop/r_packages/gdeltr2/R/gdelt_free_text_api.R')
setwd("~")
gdeltr2::generate_trelliscope_bundle(
  include_image_panel = TRUE,
  include_timeline_info = TRUE,
  include_timeline_tone = TRUE,
  include_sentiment_bin = TRUE,
  timespans = "12 weeks",
  base_path =  "Desktop/abresler.github.io/trelliscopes/pierre/test2",
  terms =  c(
    '"Pierre Gooding"',
    'Pierre Gooding',
    "Bill Perkins",
    '"Bill Perkins"',
    'Harlem District 9 city council',
    '"Marvin Holland"',
    'Harlem "Job Growth"',
    'Harlem Unemployment',
    'Harlem Schools',
    'Harlem Crime',
    '"District 9" Harlem'
  ),
  domains =  c("gothamgazette.com", "cityandstateny.com")
)


gdeltr2::get_data_ft_v2_api(
  terms = c("Charlottesville False Flag", 'Charlottesville "False Flag"'),
  timespans = "5 days",
  trelliscope_parameters = list(
    path = NULL,
    rows = 1,
    columns = 2,
    id_columns = list(
      is_except = F,
      columns = c(
        "datetimeArticle",
        "domainArticle",
        "titleArticle",
        "urlArticle",
        "termSearch"
      )
    ),
    group_columns = NULL
  )
)
