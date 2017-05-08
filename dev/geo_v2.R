gdeltr2::load_needed_packages(c(
  'glue',
  'readr',
  'stringr',
  'purrr',
  'dplyr',
  'curl',
  'tibble',
  'tidyr',
  'httr'
))


generate_slug <-
  function(parameter = 'geore',
         sep = '=',
         value_options = 0:2,
         value = NULL) {
  if (value %>% is_null()) {
    return('')
  }
  slug <-
    str_c('&', parameter, sep, value, sep = '')

  if (!value %in% value_options) {
    return('')
  }
  slug
  }



c('image_tag', 'imagewebtag')

parse_query <-
  function(query_parameters) {
  df_query_params <- data_frame(nameFunction = c('term', 'domain', 'image_face_tone', 'iamge_num_faces',
                                                 'image_ocr', 'image_tag',  'image_web_count',
                                                 'image_web_tag',  'location_name',
                                                 'location_adm1', 'location_country',
                                                 'near', 'source_country', 'source_language', 'gkg_theme',
                                                 'tone', 'tone_absolute_value'
  ) ,
  nameSlug = c('', 'Domain', 'ImageFaceTone' ,'ImageNumFaces', 'ImageOCRMeta', 'ImageTag', 'ImageWebCount', 'ImageWebTag',
               'Location', 'LocationADM1', 'LocationCC', "Near", 'SourceCountry', 'SourceLang', 'Theme', "Tone", 'ToneAbs'),
  typeSep = c('', ':',  '', '', ':', ':', '', ':', ':', ':', ":", ":", ':', ':', ":", '', ''))

  df_call <-
    1:length(query_parameters) %>%
    map_df(function(x){
      function_param <-
        names(query_parameters[x])
      value <-
        query_parameters[x][[1]]
      if (value %>% purrr::is_null()) {
        return(data_frame())
      }
      value <- value %>% curl::curl_escape()
      has_or <-
        value %>% length() > 1
      is_quoted <-
        function_param %in% c('image_tag' ,'image_web_tag')
      if (is_quoted) {
        value <-
          str_c('%22', value, '%22')
      }

      if (has_or) {
        value <-
          str_c("(", value %>% str_c(collapse = "%20OR%20"), ")")
      }

      param <-
        df_query_params %>%
        filter(nameFunction == function_param) %>%
        unite(param, nameSlug, typeSep, sep = '') %>%
        .$param

      data_frame(nameCall = str_c(param, value, collapse = ''))

    })

  if (df_call %>% nrow() > 1) {

  }

  df_call$nameCall
}

#' Genere
#'
#' @param query_parameters
#' @param mode
#' @param format
#' @param timespan
#' @param max_points
#' @param geore
#' @param sort
#' @param browse_url
#'
#' @return
#' @export
#' @import purrr glue readr stringr dplyr curl tibble tidyr httr
#' @examples
generate_geo_query <-
  function(query_parameters = list(
    term = NULL,
    domain = NULL,
    image_face_tone = NULL,
    iamge_num_faces = NULL,
    image_ocr = NULL,
    image_tag = NULL,
    # quoted
    image_web_count = NULL,
    image_web_tag = NULL,
    # quoted
    location_name = NULL,
    location_adm1 = NULL,
    location_country = NULL,
    near = NULL,
    source_country = NULL,
    source_language = NULL,
    gkg_theme = "TOURISM",
    tone = NULL,
    tone_absolute_value = NULL
  ),
  mode = 'adm1',
  format = 'ImageHTML',
  timespan = NULL,
  max_points = NULL,
  geore = NULL,
  sort =  NULL,
  browse_url = TRUE) {
    base <-
      "http://api.gdeltproject.org/api/v2/geo/geo?query="

    query_slug <-
      parse_query(query_parameters)

    mode_options <-
      c(
        'PointData' ,
        'ImagePointData',
        'PointHeatmap',
        'ImagePointheatmap',
        'PointAnimation',
        'ImagePointAnimation',
        'Country',
        'ImageCountry',
        'SourceCountry',
        'ImageSourceCountry',
        'ADM1',
        'ImageADM1'
      ) %>% str_to_lower()

    mode <-
      mode %>% str_to_lower()

    mode_slug <-
      generate_slug(
        parameter = 'mode',
        sep = '=',
        value = mode,
        value_options = mode_options
      )

    if (!mode_slug == '' &&
        mode %in% c("pointheatmap",
                    "imagepointheatmap",
                    "pointanimation",
                    "imagepointanimation")) {
      format <-
        'geojson'
    }

    format_options <-
      c('html', 'ImageHTML', 'GeoJSON', 'ImageGeoJSON') %>% str_to_lower()
    format_slug <-
      generate_slug(
        parameter = 'format',
        sep = '=',
        value = format %>% str_to_lower(),
        value_options = format_options
      )

    timespan_options <- 15:1440
    timespan_slug <-
      generate_slug(
        parameter = 'timespan',
        sep = '=',
        value = timespan,
        value_options = timespan_options
      )

    geore_options <- 0:2
    geore_slug <-
      generate_slug(
        parameter = 'geore',
        sep = '=',
        value = geore,
        value_options = geore_options
      )
    sort_options <-
      c('date', 'ToneDesc', 'ToneAsc') %>% str_to_lower()

    sort_slug <-
      generate_slug(
        parameter = 'sortby',
        sep = '=',
        value = sort,
        value_options = sort_options
      )

    maxpoint_slug = ''

    url_api <-
      glue(
        "{base}",
        "{query_slug}",
        '{mode_slug}',
        "{format_slug}",
        "{timespan_slug}",
        '{maxpoint_slug}',
        "{geore_slug}",
        "{sort_slug}"
      )

    if (browse_url) {
      url_api %>% httr::BROWSE()
    }
    data <-
      data_frame(urlAPI = url_api)
    is_geo <-
      format %>% str_detect("json")
    if (is_geo) {
      df_geo <-
        url_api %>%
        jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE) %>%
        .$features %>%
        as_data_frame()
      df_geo <-
        df_geo %>%
        mutate(idRow = 1:n())
      df_lat_lon <-
        1:length(df_geo$geometry.coordinates) %>%
        map_df(function(x) {
          data_frame(
            item = c('longitudeArticle', 'latitudeArticle'),
            value = df_geo$geometry.coordinates[[x]]
          ) %>%
            spread(item, value) %>%
            mutate(idRow = x) %>%
            dplyr::select(idRow, everything())
        })

      df_geo <-
        df_geo %>%
        select(-c(geometry.coordinates)) %>%
        purrr::set_names(c('typeMap', 'nameProperty', 'countProperty',
                           'propertiesShareImage', 'htmlArticle',
                           'typeGeometry', 'idRow')) %>%
        left_join(df_lat_lon) %>%
        suppressMessages()
      data <-
        data %>%
        mutate(dataGeoJSON = list(df_geo))
    }

    data
  }
