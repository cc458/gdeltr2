
# utilities ---------------------------------------------------------------

remove_full_na_column <-
  function(data) {
    data <-
      data %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))
    return(data)
  }



# trelliscope -------------------------------------------------------------

check_for_trelliscope_js <-
  function() {
    missing <-
      installed.packages() %>% dplyr::as_data_frame() %>%
      dplyr::filter(Package == 'trelliscopejs') %>%
      nrow() == 0
    if (missing) {
      devtools::install_github("hafen/trelliscopejs")
    }
  }


# parse -------------------------------------------------------------------


parse_source <-
  function(source = "netsdaily.com - writedate('06/02/2016 12:00 UTC'); (English / United States)") {
    source_df <-
      data_frame(source) %>%
      tidyr::separate(source,
                      sep = '\\ - ',
                      into = c('source', 'date.language')) %>%
      tidyr::separate(date.language,
                      sep = '\\;',
                      into = c('date', 'language')) %>%
      mutate(
        language = language %>% str_replace('\\(', '') %>% str_replace('\\)', '')  %>% str_replace('\\ /', ','),
        date = date %>% gsub('\\writedate', '', .) %>% str_replace_all('\\(', '') %>% str_replace_all('\\)', '') %>%
          str_replace_all("\\'", '')
      ) %>%
      mutate(dateTime = date %>% lubridate::mdy_hm() %>% lubridate::with_tz(Sys.timezone())) %>%
      mutate(date = dateTime %>% as.Date()) %>%
      tidyr::separate(language,
                      into = c('language', 'country'),
                      sep = '\\, ') %>%
      suppressWarnings() %>%
      suppressMessages()

    source_df <-
      source_df %>%
      mutate_if(is.character,
                str_trim) %>%
      suppressWarnings() %>%
      suppressMessages()

    source_df
  }


# terms -------------------------------------------------------------------
get_data_ft_api_term <-
  function(term = "'Donald Trump'",
           domain = NA,
           dedeup_results = T,
           restrict_to_usa = F,
           last_minutes = NA,
           max_rows = 1000,
           only_english = T,
           return_image_url = F,
           tone_less_than = NA,
           tone_more_than = NA,
           search_language = 'English',
           source_language = 'English',
           sort_by = 'date',
           return_message = T) {
    url_base <-
      'http://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

    if (term %>% is.na()) {
      term_slug <-
        ''
      term_word <-
        'all words'
    } else {
      term_slug <-
        term %>%
        str_to_lower() %>%
        URLencode()
      term_word <-
        term
    }

    if (term %>% is.na() & !domain %>% is.na()) {
      term_word <-
        domain
    }

    if (!domain %>% is.na()) {
      domain_slug <-
        '%20domain:' %>%
        paste0(domain %>% URLencode())
    } else {
      domain_slug <-
        ''
    }

    if (!search_language %>% is.na()) {
      search_lang_slug <-
        '&searchlang:' %>%
        paste0(search_language %>% str_to_lower())
    } else {
      search_lang_slug <-
        ''
    }

    if (!source_language %>% is.na()) {
      source_lang_slug <-
        '&sourcelang:' %>%
        paste0(source_language %>% str_to_lower())
    } else {
      source_lang_slug <-
        ''
    }

    if (!last_minutes %>% is.na()) {
      last_minute_slug <-
        '%20lastminutes:' %>%
        paste0(last_minutes)
    } else {
      last_minute_slug <-
        ''
    }

    if (!tone_more_than %>% is.na()) {
      if (tone_more_than >= 100) {
        stop("Tone can't be over 100")
      }
      tone_more_slug <-
        '%20tonemorethan:' %>%
        paste0(tone_more_than)
    } else {
      tone_more_slug <-
        ''
    }

    if (!tone_less_than %>% is.na()) {
      if (tone_less_than >= 100) {
        stop("Tone can't be under 100")
      }
      tone_less_slug <-
        '%20tonelessthan:' %>%
        paste0(tone_less_than)
    } else {
      tone_less_slug <-
        ''
    }

    term_slug <-
      term_slug %>%
      paste0(
        domain_slug,
        last_minute_slug,
        tone_more_slug,
        tone_less_slug,
        search_lang_slug,
        source_lang_slug
      )

    sort_df <-
      data_frame(
        sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
        sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
      )

    if (sort_by %in% sort_df$sort_term == F) {
      stop("Sorry sort terms can only be\n" %>%
             paste0(paste0(sort_df$sort_term, collapse = '\n')))
    }

    slug_sort <-
      sort_df %>%
      dplyr::filter(sort_term == sort_by) %>%
      .$sort_slug

    slug_sort <-
      '&sort=' %>%
      paste0(slug_sort)

    if (!max_rows %>% is.na()) {
      max_row_slug <-
        '&maxrows=' %>%
        paste0(max_rows)

    } else {
      max_row_slug <-
        ''
    }

    if (dedeup_results) {
      dup_slug <-
        '&dropdup=true'
    } else {
      dup_slug <-
        ''
    }

    if (return_image_url) {
      image_slug <-
        '&output=artimglist'
    } else {
      image_slug <-
        '&output=artlist'
    }

    url <-
      url_base %>%
      paste0(term_slug,
             image_slug,
             dup_slug,
             last_minute_slug,
             slug_sort,
             max_row_slug)

    page.has.content <-
      url %>%
      httr::GET()

    page_size_df <-
      page.has.content$headers  %>%
      flatten_df()

    if (!page.has.content$status_code == 200) {
      stop("Seaerch has no data")
    } else {
      if ('content-length' %in% names(page_size_df)) {
        page_size_df <-
          page_size_df %>%
          mutate(`content-length` = `content-length` %>% as.numeric)

        if (page_size_df$`content-length` <= 41) {
          stop("This search has no data")
        }
      }
    }

    page <-
      url %>%
      xml2::read_html()

    if (page %>%
        rvest::html_nodes(xpath = '//b') %>%
        rvest::html_text() %>%
        str_trim() %>% length == 0) {
      stop("This search has no data")
    }

    titleArticle <-
      page %>%
      rvest::html_nodes(xpath = '//b') %>%
      rvest::html_text() %>%
      str_trim()

    if (return_image_url) {
      url.source <-
        page %>%
        rvest::html_nodes(xpath = '//a') %>%
        rvest::html_attr('href') %>%
        .[c(T, F)]
    } else {
      url.source <-
        page %>%
        rvest::html_nodes(xpath = '//a') %>%
        rvest::html_attr('href')
    }

    url.source <-
      url.source %>%
      str_split('\\javascript:window.open') %>%
      flatten_chr %>%
      str_replace_all('\\);', '')

    url.source <-
      url.source[!url.source == '']

    if (!return_image_url) {
      url.source <-
        1:length(url.source) %>%
        map_chr(function(x) {
          char_url <-
            url.source[x] %>% nchar()

          url.source[x] %>% substr(start = 3, stop = char_url - 1)
        })
    }

    sources <-
      page %>%
      rvest::html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "sourceinfo", " " ))]') %>%
      rvest::html_text()
    wrong_length <-
      url.source %>% length() > sources %>% length()
    if (wrong_length) {
      url.source <-
        url.source[1:length(sources)]
    }
    parse_source_safe <-
      purrr::possibly(parse_source, data_frame())
    url_df <-
      data_frame(
        term,
        titleArticle,
        urlArticle = url.source,
        dateTimeData = Sys.time(),
        urlSearch = url
      ) %>%
      mutate(idRow = 1:n())

    df_sources <-
      sources %>%
      parse_source_safe() %>%
      mutate(idRow = 1:n())

    if (df_sources %>% nrow() > 0) {
      url_df <-
        url_df %>%
        left_join(df_sources) %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    url_df <-
      url_df %>%
      dplyr::select(-idRow)


    if (return_image_url) {
      urlThumbnail <-
        page %>%
        rvest::html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "thumbimg", " " ))]') %>%
        rvest::html_attr('src')

      urlThumbnail[urlThumbnail == ''] <-
        NA

      url_df <-
        url_df %>%
        mutate(urlThumbnail)

    }

    if (!domain %>% is.na()) {
      url_df <-
        url_df %>%
        mutate(domainSearch = domain) %>%
        dplyr::select(term, domainSearch, everything())
    }

    if (!tone_more_than %>% is.na()) {
      url_df <-
        url_df %>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na()) {
      url_df <-
        url_df  %>%
        mutate(tone_less_than)
    }

    if (only_english) {
      url_df <-
        url_df %>%
        dplyr::filter(language == 'English')
    }

    url_df <-
      url_df %>%
      mutate(domainArticle = urltools::domain(urlArticle) %>% str_replace_all('www.', '')) %>%
      dplyr::select(term:urlArticle, domainArticle, everything()) %>%
      dplyr::rename(
        dateTimeArticle = dateTime,
        dateArticle = date,
        countryArticle = country,
        languageArticle = language,
        sourceArticle = source
      )

    if (term %>% is.na()) {
      url_df <-
        url_df %>%
        dplyr::select(-term)
    }

    if (restrict_to_usa) {
      url_df <-
        url_df %>%
        dplyr::filter(countryArticle == 'United States')
    }

    if (return_message) {
      "You got " %>%
        paste0(url_df %>% nrow(), ' urls for ', term_word, ' at ', Sys.time()) %>%
        message()
    }

    return(url_df)

  }

#' Returns GDELT free text API results for multiple terms
#'
#' @param terms vector of words to search
#' @parram
#' @param visualize_results if \code{TRUE} returns an interactive trelliscope
#' @param trelliscope_parameters list of parameters to pass along to trelliscope \itemize{
#' \item path: if not \code{NULL} the path to save the trelliscope
#' \item rows: rows for trelliscope
#' \item columns: columns for trelliscope
#' \item id_columns: initial columns
#' }
#' @param domain domains you wish to restrict the search to
#' @param return_image_url if \code{TRUE} returns only articles with photos
#' @param last_minutes restrict to last x minutes
#' @param max_rows maximum rows
#' @param search_language article language to search
#' @param source_language source article search
#' @param sort_by sort by
#' @param dedeup_results if \code{TRUE} remove duplicate results
#' @param only_english if \code{TRUE} returns only english results
#' @param nest_data if \code{TRUE} retrns a nested data frame
#' @param return_message if \code{TRUE} return a messag
#' @import tidyr stringr rvest tidyverse dplyr trelliscopejs devtools
#' @importFrom lubridate with_tz
#' @importFrom lubridate mdy_hm
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom purrr flatten_df
#' @importFrom xml2 read_html
#' @return if \code{visualize_results} an interactive trelliscope else a \code{data_frame}
#' @export
#'
#' @examples
#' \donotrun{
#' get_data_ft_v1_api_terms(terms = c('"Kevin Durant"','"Stephen Curry"', "Donald Trump", '"Blackstone Real Estate"', "'Cap Rate'"), only_english = T)
#' }
get_data_ft_v1_api_terms <-
  function(terms = c('"Kevin Durant"', '"Stephen Curry"'),
           visualize_results = TRUE,
           trelliscope_parameters = list(
             path = NULL,
             rows = 1,
             columns = 2,
             id_columns = NULL
           ),
           domain = NA,
           dedeup_results = TRUE,
           restrict_to_usa = F,
           only_english = F,
           return_image_url = TRUE,
           last_minutes = NA,
           max_rows = 1000000,
           search_language = 'English',
           source_language = 'English',
           sort_by = 'date',
           nest_data = FALSE,
           return_message = TRUE) {
    var_matrix <-
      expand.grid(
        term = terms,
        domain = domain,
        restrict_to_usa = restrict_to_usa,
        only_english = only_english,
        return_image_url = return_image_url,
        last_minutes = last_minutes,
        max_rows = max_rows,
        search_language = search_language,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_data_frame() %>%
      suppressWarnings()
    get_data_ft_api_term_safe <-
      purrr::possibly(get_data_ft_api_term, data_frame())
    all_data <-
      1:nrow(var_matrix) %>%
      purrr::map_df(function(x) {
        get_data_ft_api_term_safe(
          term = var_matrix$term[x],
          domain = var_matrix$domain[x],
          return_image_url = var_matrix$return_image_url[x],
          last_minutes = var_matrix$last_minutes[x],
          max_rows = var_matrix$max_rows[x],
          search_language = var_matrix$search_language[x],
          source_language = var_matrix$source_language[x],
          sort_by = sort_by,
          restrict_to_usa = var_matrix$restrict_to_usa[x],
          only_english = var_matrix$only_english[x],
          dedeup_results = dedeup_results
        )
      }) %>%
      arrange(desc(dateTimeArticle))

    if (visualize_results) {
      check_for_trelliscope_js()
      title <-
        list("GDELT Term Search for ", Sys.Date()) %>%
        purrr::reduce(paste0)

      df_parameters <- trelliscope_parameters %>% flatten_df()

      if (!df_parameters %>% has_name('id_columns')) {
        id_columns <-
          c('dateTimeArticle',
            'domainArticle',
            "term",
            "titleArticle",
            "urlArticle")
      } else {
        id_columns <- df_parameters$id_columns
      }

      if (!df_parameters %>% has_name('rows')) {
        rows <-
          1
      } else {
        rows <- df_parameters$rows
      }

      if (!df_parameters %>% has_name('columns')) {
        columns <-
          2
      } else {
        columns <- df_parameters$columns
      }

      has_path <-
        df_parameters %>% has_name('path')

      all_data <-
        all_data %>%
        mutate(
          idArticle = 1:n(),
          panel = trelliscopejs::img_panel(urlThumbnail),
          urlArticle = trelliscopejs::cog_href(urlArticle)
        ) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        mutate_at(c('dateTimeArticle', 'dateArticle'),
                  funs(. %>% as.character()))
      if (has_path) {
        path_loc <-
          df_parameters$path
        viz <-
          all_data %>%
          trelliscopejs::trelliscope(
            name = title,
            nrow = rows,
            ncol = columns,
            path = path_loc,
            state = list(labels = c(id_columns))
          )
        return(viz)
      }
      viz <-
        all_data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(id_columns))
        )
      return(viz)

    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }


# domains -----------------------------------------------------------------

#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param return_message if \code{TRUE} return a message
#' Returns GDELT free text API results for multiple webdomains
#'
#' @param terms vector of words to search
#' @param visualize_results
#' @param trelliscope_parameters list of parameters to pass along to trelliscope \itemize{
#' \item path: if not \code{NULL} the path to save the trelliscope
#' \item rows: rows for trelliscope
#' \item columns: columns for trelliscope
#' \item id_columns: initial columns
#' }
#' @param domain domains you wish to restrict the search to
#' @param return_image_url if \code{TRUE} returns only articles with photos
#' @param last_minutes restrict to last x minutes
#' @param max_rows maximum rows
#' @param search_language article language to search
#' @param source_language source article search
#' @param sort_by sort by
#' @param dedeup_results if \code{TRUE} remove duplicate results
#' @param only_english if \code{TRUE} returns only english results
#' @param return_message if \code{TRUE} return a message
#' @param nest_data if \code{TRUE} return a nested data frame
#' @import tidyr stringr rvest tidyverse trelliscopejs purrr dplyr devtools
#' @importFrom xml2 read_html
#' @return
#' @export
#'
#' @examples
#' \donotrun{
#' get_data_ft_v1_api_domains(domains = c('realdeal.com', 'pehub.com', 'sbnation.com', 'wsj.com', 'seekingalpha.com', 'washingtonpost.com', 'nytimes.com'))
#' }

get_data_ft_v1_api_domains <-
  function(domains = c('washingtonpost.com', 'nytimes.com'),
           visualize_results = TRUE,
           trelliscope_parameters = list(
             path = NULL,
             rows = 1,
             columns = 2,
             id_columns = NULL
           ),
           use_exact_domains = F,
           term = NA,
           return_image_url = T,
           last_minutes = NA,
           max_rows = 1000,
           search_language = 'English',
           source_language = 'English',
           sort_by = 'date',
           restrict_to_usa = F,
           dedeup_results = T,
           only_english = F,
           nest_data = F,
           return_message = T) {
    get_data_ft_api_term_safe <-
      purrr::possibly(get_data_ft_api_term, data_frame())

    var_matrix <-
      expand.grid(
        term = term,
        domain = domains,
        restrict_to_usa = restrict_to_usa,
        only_english = only_english,
        return_image_url = return_image_url,
        last_minutes = last_minutes,
        max_rows = max_rows,
        search_language = search_language,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_data_frame() %>%
      suppressWarnings()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      purrr::map_df(
        function(x)
          get_data_ft_api_term_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            return_image_url = var_matrix$return_image_url[x],
            last_minutes = var_matrix$last_minutes[x],
            max_rows = var_matrix$max_rows[x],
            search_language = var_matrix$search_language[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            restrict_to_usa = var_matrix$restrict_to_usa[x],
            only_english = var_matrix$only_english[x],
            dedeup_results = dedeup_results
          ) %>%
          suppressWarnings()
      ) %>%
      arrange(desc(dateTimeArticle)) %>%
      suppressWarnings()

    all_data <- all_data %>%
      mutate_if(is.character,
                str_trim)

    if (term %>% is.na()) {
      all_data <-
        all_data %>%
        dplyr::select(-term)
    }

    if (use_exact_domains) {
      all_data <-
        all_data %>%
        dplyr::filter(domainArticle %in% domains)
    }

    if (visualize_results) {
      check_for_trelliscope_js()

      title <-
        list("GDELT Domain Search at ", Sys.Date()) %>%
        purrr::reduce(paste0)

      df_parameters <- trelliscope_parameters %>% flatten_df()

      if (!df_parameters %>% has_name('id_columns')) {
        id_columns <-
          c('dateTimeArticle',
            "domainSearch",
            "titleArticle",
            "urlArticle")
      } else {
        id_columns <- df_parameters$id_columns
      }

      if (!df_parameters %>% has_name('rows')) {
        rows <-
          1
      } else {
        rows <- df_parameters$rows
      }

      if (!df_parameters %>% has_name('columns')) {
        columns <-
          2
      } else {
        columns <- df_parameters$columns
      }

      has_path <-
        df_parameters %>% has_name('path')

      all_data <-
        all_data %>%
        mutate(
          idArticle = 1:n(),
          panel = trelliscopejs::img_panel(urlThumbnail),
          urlArticle = trelliscopejs::cog_href(urlArticle)
        ) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        mutate_at(c('dateArticle', 'dateTimeArticle'),
                  funs(. %>% as.character()))

      if (has_path) {
        path_loc <-
          df_parameters$path
        viz <-
          all_data %>%
          trelliscopejs::trelliscope(
            name = title,
            nrow = rows,
            ncol = columns,
            path = path_loc,
            state = list(labels = c(id_columns))
          )
        return(viz)
      }
      viz <-
        all_data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(id_columns))
        )


      return(viz)

    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('domainSearch'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }

# word clouds -------------------------------------------------------------


get_data_wordcloud_ft_api <-
  function(term = '"Brooklyn Nets"',
           domain = NA,
           last_minutes = NA,
           search_language = 'English',
           tone_more_than = NA,
           tone_less_than = NA,
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           return_message = T) {
    url_base <-
      'http://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

    if (term %>% is.na()) {
      term_slug <-
        ''
      term_word <-
        'all words'
    } else {
      term_slug <-
        term %>%
        str_to_lower() %>%
        URLencode()
      term_word <-
        term
    }

    if (term %>% is.na() & !domain %>% is.na()) {
      term_word <-
        domain
    }

    if (!domain %>% is.na()) {
      domain_slug <-
        '%20domain:' %>%
        paste0(domain)
    } else {
      domain_slug <-
        ''
    }

    if (!search_language %>% is.na()) {
      search_lang_slug <-
        '&searchlang:' %>%
        paste0(search_language %>% str_to_lower())
    } else {
      search_lang_slug <-
        ''
    }

    if (!source_language %>% is.na()) {
      source_lang_slug <-
        '&sourcelang:' %>%
        paste0(source_language %>% str_to_lower())
    } else {
      source_lang_slug <-
        ''
    }

    if (!last_minutes %>% is.na()) {
      last_minute_slug <-
        '%20lastminutes:' %>%
        paste0(last_minutes)
    } else {
      last_minute_slug <-
        ''
    }

    if (!tone_more_than %>% is.na()) {
      if (tone_more_than >= 100) {
        stop("Tone can't be over 100")
      }
      tone_more_slug <-
        '%20tonemorethan:' %>%
        paste0(tone_more_than)
    } else {
      tone_more_slug <-
        ''
    }

    if (!tone_less_than %>% is.na()) {
      if (tone_less_than >= 100) {
        stop("Tone can't be under 100")
      }
      tone_less_slug <-
        '%20tonelessthan:' %>%
        paste0(tone_less_than)
    } else {
      tone_less_slug <-
        ''
    }

    term_slug <-
      term_slug %>%
      paste0(
        domain_slug,
        last_minute_slug,
        tone_more_slug,
        tone_less_slug,
        search_lang_slug,
        source_lang_slug
      )

    sort_df <-
      data_frame(
        sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
        sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
      )

    if (sort_by %in% sort_df$sort_term == F) {
      stop("Sorry sort terms can only be\n" %>%
             paste0(paste0(sort_df$sort_term, collapse = '\n')))
    }

    slug_sort <-
      sort_df %>%
      dplyr::filter(sort_term == sort_by) %>%
      .$sort_slug

    slug_sort <-
      '&sort=:' %>%
      paste0(slug_sort)

    if (dedeup_results) {
      dup_slug <-
        '&dropdup=true'
    } else {
      dup_slug <-
        ''
    }
    output_slug <-
      '&output=wordcloudcsv'

    url <-
      url_base %>%
      paste0(term_slug,
             dup_slug,
             slug_sort,
             output_slug)

    page.has.content <-
      url %>%
      httr::GET()

    page_size_df <-
      page.has.content$headers  %>%
      flatten_df() %>%
      mutate(`content-length` = `content-length` %>% as.numeric)

    if (page_size_df$`content-length` <= 41) {
      stop("This search has no data")
    }

    wordcloud_data <-
      url %>%
      read_csv() %>%
      mutate(term, url, dateTimeData = Sys.time()) %>%
      dplyr::select(term, everything()) %>%
      suppressMessages()

    names(wordcloud_data)[2:3] <-
      c('word', 'articles')

    wordcloud_data <-
      wordcloud_data %>%
      tidyr::separate(articles,
                      into = c('countArticles', 'size'),
                      sep = '\\(') %>%
      mutate(
        countArticles = countArticles %>% readr::parse_number(),
        size = size %>% readr::parse_number()
      ) %>%
      dplyr::rename(urlSearch = url,
                    sizeWord = size)

    if (!domain %>% is.na()) {
      wordcloud_data <-
        wordcloud_data  %>%
        mutate(domainSearch = domain) %>%
        dplyr::select(term, domainSearch, everything())
    }

    if (term %>% is.na()) {
      wordcloud_data <-
        wordcloud_data %>%
        dplyr::select(-term)
    }

    if (!tone_more_than %>% is.na()) {
      wordcloud_data <-
        wordcloud_data %>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na()) {
      wordcloud_data <-
        wordcloud_data  %>%
        mutate(tone_less_than)
    }

    if (return_message) {
      "You got " %>%
        paste0(wordcloud_data %>% nrow(),
               ' words for ',
               term_word,
               ' at ',
               Sys.time()) %>%
        message()
    }

    return(wordcloud_data)

  }


#' Returns GDELT free text API word clouds for a given domain, can be term restricted
#'
#' @param domains
#' @param term options \code{c(NA, "term_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#' @param nest_data
#' @import tidyr stringr rvest tidyverse dplyr purrr stringr wordcloud2
#' @return
#' @export
#'
#' @examples

get_data_wordcloud_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
           term = NA,
           visualize_word_cloud = TRUE,
           last_minutes = NA,
           search_language = 'English',
           tone_more_than = NA,
           tone_less_than = NA,
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = TRUE,
           nest_data = FALSE,
           return_message = TRUE) {
    get_data_wordcloud_ft_api_safe <-
      possibly(get_data_wordcloud_ft_api, data_frame())

    var_matrix <-
      expand.grid(
        domain = domains,
        term = term,
        last_minutes = last_minutes,
        search_language = search_language,
        tone_more_than = tone_more_than,
        tone_less_than = tone_less_than,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_data_frame %>%
      suppressWarnings()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      purrr::map_df(
        function(x)
          get_data_wordcloud_ft_api_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            last_minutes = var_matrix$last_minutes[x],
            search_language = var_matrix$search_language[x],
            tone_more_than = var_matrix$tone_more_than[x],
            tone_less_than = var_matrix$tone_less_than[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          ) %>%
          suppressWarnings()
      ) %>%
      suppressWarnings()

    if (visualize_word_cloud) {
      viz <-
        all_data %>%
        select(word, countArticles) %>%
        group_by(word) %>%
        summarise(countArticles = sum(countArticles, na.rm = T)) %>%
        arrange(desc(countArticles)) %>%
        ungroup() %>%
        data.frame(stringsAsFactors = TRUE) %>%
        wordcloud2::wordcloud2(fontFamily = 'Arial',
                               shuffle = F,
                               shape = 'pentagon')
      return(viz)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('domainSearch'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }

#' Returns GDELT free text API word clouds for a given term, can be domain restricted
#'
#' @param terms any word, can be quoted or not
#' @param domain domain name \code{NA} - domains, else vector of daomins
#' @param return_image_url if \code{TRUE} returns an image url
#' @param last_minutes how many prior minutes
#' @param max_rows number of rows
#' @param tone_less_than tone more than specified number
#' @param tone_more_than tone less than specified number
#' @param search_language language to search
#' @param source_language soruce language
#' @param sort_by method to sort the data
#' \code{c('date', 'relevence', 'tone.ascending', 'tone.descending')}
#' @param dedeup_results return unique results
#' \code{c(T, F)}
#' @param only_english return only english results
#' \code{c(T, F)}
#' @param return_message if \code{true} returns a message
#' \code{c(T, F)}
#' @param nest_data returns a nested data frame
#' \code{c(T, F)}
#' @importFrom tidyr nest
#' @import tidyr stringr rvest tidyverse
#' @return if \code{visualize_results} an interactive wordcloud else a \code{data_frame}
#' @export
#'
#' @examples
get_data_wordcloud_ft_api_terms <-
  function(terms = c('"Donald Trump"', '"Hilary Clinton"'),
           domain = NA,
           visualize_word_cloud = TRUE,
           last_minutes = NA,
           search_language = 'English',
           tone_more_than = NA,
           tone_less_than = NA,
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           nest_data = F,
           return_message = T) {
    get_data_wordcloud_ft_api_safe <-
      purrr::possibly(get_data_wordcloud_ft_api, NULL)

    var_matrix <-
      expand.grid(
        domain = domain,
        term = terms,
        last_minutes = last_minutes,
        search_language = search_language,
        tone_more_than = tone_more_than,
        tone_less_than = tone_less_than,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_data_frame %>%
      suppressWarnings()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      purrr::map_df(
        function(x)
          get_data_wordcloud_ft_api_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            last_minutes = var_matrix$last_minutes[x],
            search_language = var_matrix$search_language[x],
            tone_more_than = var_matrix$tone_more_than[x],
            tone_less_than = var_matrix$tone_less_than[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          )
      )

    if (visualize_word_cloud) {
      viz <-
        all_data %>%
        select(word, countArticles) %>%
        group_by(word) %>%
        summarise(countArticles = sum(countArticles, na.rm = T)) %>%
        arrange(desc(countArticles)) %>%
        ungroup() %>%
        data.frame(stringsAsFactors = TRUE) %>%
        wordcloud2::wordcloud2(fontFamily = 'Arial',
                               shuffle = F,
                               shape = 'pentagon')
      return(viz)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }

# sentiment ---------------------------------------------------------------


get_data_sentiment_ft_api <- function(term = 'Clinton',
                                      domain = NA,
                                      last_minutes = NA,
                                      is_tone = T,
                                      tone_less_than = NA,
                                      tone_more_than = NA,
                                      search_language = NA,
                                      source_language = NA,
                                      sort_by = 'date',
                                      dedeup_results = T,
                                      return_message = T) {
  url_base <-
    'http://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

  if (term %>% is.na()) {
    term_slug <-
      ''
    term_word <-
      'all words'
  } else {
    term_slug <-
      term %>%
      str_to_lower() %>%
      URLencode()

    term_word <-
      term
  }

  if (term %>% is.na() & !domain %>% is.na()) {
    term_word <-
      domain
  }

  if (!domain %>% is.na()) {
    domain_slug <-
      '%20domain:' %>%
      paste0(domain)
  } else {
    domain_slug <-
      ''
  }


  if (!search_language %>% is.na()) {
    search_lang_slug <-
      '&searchlang:' %>%
      paste0(search_language %>% str_to_lower())
  } else {
    search_lang_slug <-
      ''
  }

  if (!source_language %>% is.na()) {
    source_lang_slug <-
      '&sourcelang:' %>%
      paste0(source_language %>% str_to_lower())
  } else {
    source_lang_slug <-
      ''
  }

  if (!last_minutes %>% is.na()) {
    last_minute_slug <-
      '%20lastminutes:' %>%
      paste0(last_minutes)
  } else {
    last_minute_slug <-
      ''
  }

  if (!tone_more_than %>% is.na()) {
    if (tone_more_than >= 100) {
      stop("Tone can't be over 100")
    }
    tone_more_slug <-
      '%20tonemorethan:' %>%
      paste0(tone_more_than)
  } else {
    tone_more_slug <-
      ''
  }

  if (!tone_less_than %>% is.na()) {
    if (tone_less_than >= 100) {
      stop("Tone can't be under 100")
    }
    tone_less_slug <-
      '%20tonelessthan:' %>%
      paste0(tone_less_than)
  } else {
    tone_less_slug <-
      ''
  }

  term_slug <-
    term_slug %>%
    paste0(
      domain_slug,
      last_minute_slug,
      tone_more_slug,
      tone_less_slug,
      search_lang_slug,
      source_lang_slug
    )
  sort_df <-
    data_frame(
      sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
      sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
    )

  if (sort_by %in% sort_df$sort_term == F) {
    stop("Sorry sort terms can only be\n" %>%
           paste0(paste0(sort_df$sort_term, collapse = '\n')))
  }

  slug_sort <-
    sort_df %>%
    dplyr::filter(sort_term == sort_by) %>%
    .$sort_slug

  slug_sort <-
    '&sort=:' %>%
    paste0(slug_sort)



  if (dedeup_results) {
    dup_slug <-
      '&dropdup=true'
  } else {
    dup_slug <-
      ''
  }
  if (is_tone) {
    value_name <-
      'valueTone'
    output_slug <-
      '&output=timelinecsv&outputtype=tone'
  } else {
    output_slug <-
      '&output=timelinecsv&outputtype=volume'
    value_name <-
      'value.volume'
  }

  url <-
    url_base %>%
    paste0(term_slug,
           dup_slug,
           slug_sort,
           output_slug)

  page.has.content <-
    url %>%
    httr::GET()

  page_size_df <-
    page.has.content$headers  %>%
    flatten_df() %>%
    mutate(`content-length` = `content-length` %>% as.numeric)

  if (page_size_df$`content-length` <= 41) {
    stop("This search has no data")
  }

  sentiment_data <-
    url %>%
    readr::read_csv() %>%
    mutate(term, url, dateTimeData = Sys.time()) %>%
    dplyr::select(term, everything()) %>%
    suppressMessages()


  names(sentiment_data)[2:3] <-
    c('dateTime.url', 'dateTime_human.url')

  names(sentiment_data)[4] <-
    value_name

  sentiment_data <-
    sentiment_data  %>%
    mutate(
      dateTimeSentiment = dateTime_human.url %>% lubridate::mdy_hms(tz = 'UTC') %>%  lubridate::with_tz(Sys.timezone()),
      dateSentiment = dateTime_human.url %>% lubridate::mdy_hms(tz = 'UTC') %>% as.Date()
    ) %>%
    dplyr::select(-c(dateTime.url, dateTime_human.url)) %>%
    dplyr::select(term, dateTimeSentiment, dateSentiment, everything()) %>%
    dplyr::rename(urlSearch = url, dateTimeData = dateTimeData)

  if (!domain %>% is.na()) {
    sentiment_data <-
      sentiment_data  %>%
      mutate(domainSearch = domain) %>%
      dplyr::select(term, domainSearch, everything())
  }

  if (!tone_less_than %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      mutate(tone_less_than)
  }

  if (!tone_more_than %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      mutate(tone_more_than)
  }

  if (term %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      dplyr::select(-term)
  }

  if (return_message) {
    "You got " %>%
      paste0(sentiment_data %>% nrow(),
             ' words for ',
             term_word,
             ' at ',
             Sys.time()) %>%
      message()
  }

  return(sentiment_data)

}

#' Returns GDELT free text API word clouds for a given domain, can be term restricted
#'
#' @param domains
#' @param term options \code{c(NA, "term_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#' @param nest_data
#' @return
#' @import tidyr stringr rvest tidyverse ggplot2 ggthemes hrbrthemes
#' @importFrom grDevices colors
#' @importFrom plotly ggplotly
#' @export
#'
#' @examples

get_data_sentiment_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
           visualization = 'static',
           term = NA,
           last_minutes = NA,
           is_tone = T,
           tone_less_than = NA,
           tone_more_than = NA,
           search_language = NA,
           source_language = NA,
           sort_by = 'date',
           dedeup_results = T,
           nest_data = F,
           return_message = T) {
    get_data_sentiment_ft_api_safe <-
      purrr::possibly(get_data_sentiment_ft_api, data_frame())

    var_matrix <-
      expand.grid(
        term = term,
        domain = domains,
        is_tone = is_tone,
        last_minutes = last_minutes,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        search_language = search_language,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_data_frame %>%
      suppressWarnings()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      purrr::map_df(
        function(x)
          get_data_sentiment_ft_api_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            last_minutes = var_matrix$last_minutes[x],
            is_tone = var_matrix$is_tone[x],
            tone_less_than = var_matrix$tone_less_than[x],
            tone_more_than = var_matrix$tone_more_than[x],
            search_language = var_matrix$search_language[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          ) %>%
          suppressMessages
      )
    is_visalization <-
      !purrr::is_null(visualization)
    if (is_visalization) {
      viz <-
        all_data %>%
        ggplot(aes(x = dateTimeSentiment, y = valueTone)) +
        geom_line(aes(color = domainSearch)) +
        scale_y_continuous(limits = c(-7, 7)) +
        facet_wrap(~ domainSearch, scales = "free") +
        hrbrthemes::theme_ipsum_rc(grid = "XY") +
        scale_x_datetime(expand = c(0, 0)) +
        theme(legend.position = "none") +
        labs(
          x = NULL,
          y = "Tone",
          title = list("GDELT Domain Sentiment Analysis as of ", Sys.Date()) %>% purrr::reduce(paste0),
          caption = "Data from GDELT via gdeltr2"
        )

      if (domains %>% length() <= 8) {
        viz <-
          viz +
          ggthemes::scale_color_colorblind(guide = guide_legend(title = ""))
      } else {
        manual_colors <-
          RColorBrewer::brewer.pal(12, "Paired")
        over_12 <-
          domains %>% length() > 12
        if (over_12) {
          more_colors <-
            domains %>% length() - 12
          add_colors <-
            grDevices::colors() %>% sample(more_colors)
          manual_colors <-
            c(manual_colors, add_colors)
        }
        viz <-
          viz +
          scale_color_manual(values = manual_colors, guide = guide_legend(title = ""))
      }

      is_interactive <-
        visualization %>% str_to_lower() == 'interactive'

      if (is_interactive) {
        viz <-
          plotly::ggplotly(viz)
      }

      return(viz)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('domainSearch'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }

#' Returns GDELT free text API sentiment for a given term, can be domain restricted
#'
#' @param terms
#' @param domain  \code{c(NA, "domain_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#' @export
#' @import tidyr stringr rvest tidyverse ggplot2 ggthemes hrbrthemes
#' @importFrom grDevices colors
#' @importFrom plotly ggplotly
#' @examples
#' get_data_sentiment_ft_api_terms(terms = c("Zika", '"Golden State Warriors"')) %>% View
get_data_sentiment_ft_api_terms <-
  function(terms = c("Zika", '"Golden State Warriors"'),
           visualization = NULL,
           domain = NA,
           last_minutes = NA,
           is_tone = T,
           tone_less_than = NA,
           tone_more_than = NA,
           search_language = NA,
           source_language = NA,
           sort_by = 'date',
           dedeup_results = T,
           nest_data = F,
           return_message = T) {
    get_data_sentiment_ft_api_safe <-
      purrr::possibly(get_data_sentiment_ft_api, data_frame())

    var_matrix <-
      expand.grid(
        term = terms,
        domain = domain,
        is_tone = is_tone,
        last_minutes = last_minutes,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        search_language = search_language,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_data_frame %>%
      suppressWarnings()


    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      purrr::map_df(
        function(x)
          get_data_sentiment_ft_api_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            last_minutes = var_matrix$last_minutes[x],
            is_tone = var_matrix$is_tone[x],
            tone_less_than = var_matrix$tone_less_than[x],
            tone_more_than = var_matrix$tone_more_than[x],
            search_language = var_matrix$search_language[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          ) %>%
          suppressMessages
      ) %>%
      suppressWarnings()

    is_visalization <-
      !purrr::is_null(visualization)
    if (is_visalization) {
      viz <-
        all_data %>%
        ggplot(aes(x = dateTimeSentiment, y = valueTone)) +
        geom_line(aes(color = term)) +
        scale_y_continuous(limits = c(-7, 7)) +
        facet_wrap(~ term, scales = "free") +
        hrbrthemes::theme_ipsum_rc(grid = "XY") +
        scale_x_datetime(expand = c(0, 0)) +
        theme(legend.position = "none") +
        labs(
          x = NULL,
          y = "Tone",
          title = list("GDELT Term Sentiment Analysis as of ", Sys.Date()) %>% purrr::reduce(paste0),
          caption = "Data from GDELT via gdeltr2"
        )

      if (terms %>% length() <= 8) {
        viz <-
          viz +
          ggthemes::scale_color_colorblind(guide = guide_legend(title = ""))
      } else {
        manual_colors <-
          RColorBrewer::brewer.pal(12, "Paired")
        over_12 <-
          terms %>% length() > 12
        if (over_12) {
          more_colors <-
            terms %>% length() - 12
          add_colors <-
            grDevices::colors() %>% sample(more_colors)
          manual_colors <-
            c(manual_colors, add_colors)
        }
        viz <-
          viz +
          scale_color_manual(values = manual_colors, guide = guide_legend(title = ""))
      }

      is_interactive <-
        visualization %>% str_to_lower() == 'interactive'

      if (is_interactive) {
        viz <-
          plotly::ggplotly(viz)
      }

      return(viz)
    }


    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }



# instability -------------------------------------------------------------

#' Code book for instability locations
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr tidyr
#' @importFrom readr read_tsv
#' @importFrom purrr set_names
#' @examples
get_codes_stability_locations <-
  function() {
    country_df <-
      'http://data.gdeltproject.org/blog/stability-dashboard-api/GEOLOOKUP-COUNTRY.TXT' %>%
      read_tsv(col_names = F) %>%
      set_names(c('idLocation', 'nameLocation')) %>%
      separate(nameLocation, into = c('NL1', 'NL2'), sep = '\\, ') %>%
      mutate(nameLocation = ifelse(NL2 %>% is.na, NL1, paste(NL2, NL1))) %>%
      dplyr::select(-c(NL2, NL1)) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      mutate(isCountry = T,
             codeCountry = idLocation)

    place_df <-
      'http://data.gdeltproject.org/blog/stability-dashboard-api/GEOLOOKUP-ADM1.TXT' %>%
      read_tsv(col_names = F) %>%
      set_names(c('idLocation', 'nameLocation')) %>%
      mutate(
        nameLocation = nameLocation %>% str_to_title(),
        nameLocation =  nameLocation %>% str_replace_all("Bahamas, The General, Bahamas, The", "The Bahamas") %>% str_replace_all(
          "Etorofu, Habomai, Kunashiri And Shikotan Islands General, Etorofu, Habomai, Kunashiri And Shikotan Islands",
          "Kuril Islands"
        ) %>% str_replace_all('Serbia And Montenegro General,', 'Serbia and Montenegro') %>% str_replace_all(" Of ", ' of '),
        isCountry = F,
        codeCountry = idLocation %>% substr(1, 2),
        codeLocation = idLocation %>% substr(3, 4),
        idADM1 = idLocation
      ) %>%
      separate(
        nameLocation,
        into = c('placeLocation', 'countryLocation'),
        sep = '\\, ',
        remove = F
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    location_df <-
      place_df %>%
      bind_rows(country_df) %>%
      arrange(idLocation) %>%
      mutate(countryLocation = if_else(countryLocation %>% is.na, nameLocation, countryLocation))

    return(location_df)

  }



get_data_location_instability_api <-
  function(location_id = 'US',
           variable_name = 'instability',
           day_moving_average = NA,
           time_period = 'daily',
           use_multi_locations = F,
           return_wide = T,
           return_message = T) {
    if (!'location_codes' %>% exists) {
      location_codes <-
        get_codes_stability_locations()

      assign(x = 'location_codes',
             eval(location_codes),
             env = .GlobalEnv)
    }

    if (!location_id %in% location_codes$idLocation) {
      stop("Sorry " %>% paste0(location_id, ' is not a valid location'))
    }

    var_df <-
      data_frame(
        idVar = c('instability', 'conflict', 'protest', 'tone', 'artvolnorm'),
        nameVar = c(
          'instability',
          'conflict',
          'protest',
          'tone',
          'relative mentions'
        )
      )

    if (!variable_name %>% str_to_lower %in% var_df$idVar) {
      stop("Variable names can only be:\n" %>%
             paste0(paste0(var_df$nameVar, collapse = '\n')))
    }

    if (!time_period %>% str_to_lower() %in% c('daily',
                                               '15min',
                                               '15minutes',
                                               '15 minutes',
                                               '15 minute periods')) {
      stop("Time period can only be daily or 15 minute periods")
    }

    id_var <-
      var_df %>% dplyr::filter(nameVar == variable_name) %>% .$idVar

    var_slug <-
      '&VAR=' %>% paste0(id_var)

    if (time_period == "daily") {
      period_slug <-
        "&TIMERES=day"
      period_name <-
        'dateData'
    } else {
      period_slug <-
        "&TIMERES=15min"
      period_name <-
        'datetimeData'
    }

    output_slug <-
      "&OUTPUT=csv"

    if (use_multi_locations  &
        (!location_id %>% nchar == 2)) {
      use_multi_locations <-
        F
      "You entered detailed location which cannot have multi locations\nthis analysis will still run though" %>%
        message
    }

    if (use_multi_locations) {
      mode_slug <-
        "&MODE=multi"
    } else {
      mode_slug <-
        ''
    }

    if (day_moving_average %>% is.na()) {
      ma_slug <-
        ''
    } else {
      if (!day_moving_average %in% 1:5) {
        stop("Sorry moving averages can only be 1 through 5 day")
      } else {
        ma_slug <-
          "&SMOOTH=" %>% paste0(day_moving_average)
      }

    }

    base_url <-
      'http://api.gdeltproject.org/api/v1/dash_stabilitytimeline/dash_stabilitytimeline?LOC='

    data_url <-
      base_url %>% paste0(location_id,
                          var_slug,
                          output_slug,
                          period_slug,
                          ma_slug,
                          mode_slug)

    if (use_multi_locations == F) {
      data <-
        data_url %>%
        read_csv() %>%
        suppressMessages() %>%
        set_names(c(period_name, 'value')) %>%
        mutate(
          item = id_var,
          idLocation = location_id,
          typePeriod = time_period,
          dayMovingAverage = day_moving_average
        ) %>%
        dplyr::select(1,
                      idLocation,
                      typePeriod,
                      dayMovingAverage,
                      item,
                      value)
    } else {
      data <-
        data_url %>%
        read_csv() %>%
        suppressMessages() %>%
        gather(idLocation, value, -Date) %>%
        mutate(
          item = id_var,
          typePeriod = time_period,
          dayMovingAverage = day_moving_average
        ) %>%
        dplyr::select(Date, idLocation, typePeriod, dayMovingAverage,
                      item, value) %>%
        suppressMessages()

      names(data)[1] <-
        period_name
    }

    data <-
      data %>%
      left_join(
        location_codes %>%
          dplyr::select(
            idLocation,
            codeCountry,
            codeLocation,
            nameLocation,
            countryLocation
          )
      ) %>%
      dplyr::select(
        1:5,
        nameLocation,
        countryLocation,
        codeCountry,
        codeLocation,
        nameLocation,
        countryLocation,
        everything()
      ) %>%
      suppressMessages()

    if (use_multi_locations) {
      data <-
        data %>%
        dplyr::filter(!codeLocation == "00")
    }

    if (period_name == "dateData") {
      data <-
        data %>%
        mutate(dateData = dateData %>% lubridate::ymd())
    } else {
      data <-
        data %>%
        mutate(datetimeData = datetimeData %>% ymd_hms())
    }

    data <-
      data %>%
      remove_full_na_column()

    if (return_wide) {
      data <-
        data %>%
        spread(item, value)
    }

    if (return_message) {
      locations <-
        data$nameLocation %>% unique %>% paste0(collapse = '\n')
      time_period <-
        " from " %>% paste0(
          data %>% dplyr::select(1) %>% extract2(1) %>% min,
          ' to ',
          data %>% dplyr::select(1) %>% extract2(1) %>% max
        )
      "You got GDELT " %>%
        paste0(variable_name, ' data', time_period, ' for:\n',
               locations) %>%
        message()
    }

    return(data)

  }


#' Returns instability data for given locations
#'
#' @param location_ids Specify the location IDs
#' @param random_locations Number of random location IDs to add
#' @param visualization \itemize{
#' \item \code{NULL}: no visualization
#' \item \code{interactive}: returns an interactive visualization
#' \item \code{static}: returns a ggplot2 visualization
#' }
#' @param variable_names Specify variables they can include: \itemize{
#' \item \code{instability}
#' \item \code{conflict}
#' \item \code{protest}
#' \item \code{tone}
#' \item \code{artvolnorm}: Relative mentions
#' }
#' @param days_moving_average Specify day moving average, NA is unsmoothed
#' @param time_periods Specified time period
#' \itemize{
#' \item \code{daily}
#' \item \code{15min}
#' }
#' @param use_multi_locations if \code{TRUE} returns all cities in a select country
#' @param return_wide if \code{TRUE} returns a wide data frame
#' @param nest_data if \code{TRUE} returns a nested data frame
#' @param return_message if \code{TRUE} returns a location
#'
#' @return if \code{visualize} a ggplot visualization else a \code{data_frame}
#' @export
#' @import tidyr stringr rvest tidyverse ggplot2 ggthemes hrbrthemes
#' @importFrom magrittr extract2
#' @importFrom grDevices colors
#' @importFrom plotly ggplotly
#' @examples
#' \donotrun{
#' get_data_locations_instability_api(location_ids = NULL, random_locations = 5, visualization = 'static)
#' }
get_data_locations_instability_api <-
  function(location_ids = c('US', 'IS', "TU"),
           random_locations = NULL,
           variable_names = c('instability', 'conflict', 'tone', 'protest', 'artvolnorm'),
           visualization = 'static',
           days_moving_average = NA,
           time_periods = 'daily',
           use_multi_locations = F,
           return_wide = T,
           nest_data = F,
           return_message = T) {
    get_data_location_instability_api_safe <-
      purrr::possibly(get_data_location_instability_api, data_frame())

    if (location_ids %>% purrr::is_null()) {
      location_ids <-
        c()
    }

    if (!random_locations %>% purrr::is_null()) {
      random_locs <-
        get_codes_stability_locations() %>%
        mutate(ncharLoc = nchar(idLocation)) %>%
        filter(ncharLoc == 2) %>%
        .$idLocation %>% sample(random_locations)
      location_ids <-
        c(location_ids, random_locs)
    }

    var_matrix <-
      expand.grid(
        id_location = location_ids,
        variable_name = variable_names,
        day_moving_average = days_moving_average,
        time_period = time_periods,
        use_multi_locations = use_multi_locations,
        stringsAsFactors = F
      ) %>%
      as_data_frame

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      purrr::map_df((function(x) {
        get_data_location_instability_api_safe(
          location_id = var_matrix$id_location[x],
          variable_name = var_matrix$variable_name[x],
          day_moving_average = var_matrix$day_moving_average[x],
          time_period = var_matrix$time_period[x],
          use_multi_locations = var_matrix$use_multi_locations[x],
          return_wide = F,
          return_message = return_message
        ) %>%
          suppressWarnings()
      }))
    is_viz <-
      !visualization %>% purrr::is_null()

    if (is_viz) {
      viz <-
        all_data %>%
        ggplot(aes(x = dateData, y = value)) +
        geom_line(aes(color = nameLocation)) +
        facet_wrap(~ item, scales = "free") +
        hrbrthemes::theme_ipsum_rc(grid = "XY") +
        scale_x_date(expand = c(0, 0)) +
        labs(
          x = NULL,
          y = NULL,
          title = "GDELT Stabilitiy Analysis",
          subtitle = list(
            all_data$dateData %>% min(na.rm = TRUE),
            ' to ',
            all_data$dateData %>% max(na.rm = TRUE)
          ) %>% purrr::reduce(paste0),
          caption = "Data from GDELT via gdeltr2"
        )

      if (location_ids %>% length() <= 8) {
        viz <-
          viz +
          ggthemes::scale_color_colorblind(guide = guide_legend(title = ""))
      } else {
        manual_colors <-
          RColorBrewer::brewer.pal(12, "Paired")
        over_12 <-
          location_ids %>% length() > 12
        if (over_12) {
          more_colors <-
            location_ids %>% length() - 12
          add_colors <-
            grDevices::colors() %>% sample(more_colors)
          manual_colors <-
            c(manual_colors, add_colors)
        }
        viz <-
          viz +
          scale_color_manual(values = manual_colors, guide = guide_legend(title = ""))
      }

      is_interactive <-
        visualization %>% str_to_lower() == 'interactive'

      if (is_interactive) {
        viz <-
          plotly::ggplotly(viz)
      }

      return(viz)

    }

    if (return_wide) {
      all_data <-
        all_data %>%
        spread(item, value)
    }

    if (nest_data) {
      nest_cols <-
        all_data %>% dplyr::select(one_of(
          c(
            'value',
            'dateData',
            'datetimeData',
            'instability',
            'conflict',
            'protest',
            'tone',
            'artvolnorm'
          )
        )) %>% suppressWarnings() %>%
        names()

      all_data <-
        all_data %>%
        nest_(key_col = 'data', nest_cols = nest_cols) %>%
        arrange(idLocation)
    }

    return(all_data)
  }


# Trending terms ----------------------------------------------------------



#' Gets most recent terms
#'
#' @param sort_data
#'
#' @return
#' @export
#' @import tidyr stringr rvest tidyverse
#' @importFrom readr read_csv
#' @importFrom magrittr extract2
#' @examples
get_data_ft_trending_terms <-
  function(sort_data = T) {
    data <-
      'http://live.gdeltproject.org/autocomplete_last15.csv' %>%
      read_csv() %>%
      set_names('nameTerm') %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        isGDELTTag = nameTerm %>%  grepl("[[:upper:]]+$|\\_", .),
        datetimeData = Sys.time()
      )

    if (sort_data) {
      data <-
        data %>%
        arrange(nameTerm)
    }

    return(data)
  }


# new television api ------------------------------------------------------

# http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=Amy+Shumer&context_keywords=&filter_network=CNN&filter_timespan=ALL&filter_timespan_custom_start=&filter_timespan_custom_end=&filter_displayas=PERCENT&filter_combineseparate=SEPARATE&filter_outputtype=JSON
# http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=campaign&context_keywords=&filter_network=CNN&filter_timespan=ALL&filter_displayas=PERCENT&filter_combineseparate=SEPARATE&filter_outputtype=DISPLAY



# GEO V2 ------------------------------------------------------------------

read_codebook <-
  function(url = "http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT" ,
           id_name = 'idGKGTheme') {
    data <-
      url %>%
      read_tsv(col_names = FALSE) %>%
      purrr::set_names(c(id_name, 'value')) %>%
      suppressWarnings() %>%
      suppressMessages()
    data
  }

#' GDELT Geo API code book
#'
#' @param code_book selected codebook \itemize{
#' \item GKG: GKG themes
#' \item ADM: ADM codes
#' \item imagetags: Image tags
#' \item imageweb: imageweb tags
#' \item languages: language of text
#' \item countries: country
#' }
#'
#' @return
#' @export
#' @import readr dplyr stringr purrr tibble tidyverse
#' @examples
#' get_gdelt_codebook_geo_api(code_book = 'imagetags')
get_gdelt_codebook_geo_api <-
  function(code_book = 'adm') {
    df_codebooks <-
      data_frame(
        nameCodeBook = c(
          'gkg',
          'adm',
          'imagetags',
          'imageweb',
          'languages',
          'countries'
        ),
        nameId = c(
          'idGKGTheme',
          'idADM',
          'idImageTag',
          'idImageWeb',
          'idLanguage',
          'idCountry'
        ),
        urlCodeBook = c(
          "http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT",
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-ADM1S.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGEWEBTAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT'
        )
      )
    code_book <- code_book %>% str_to_lower()
    codes <- df_codebooks$nameCodeBook %>% str_to_lower()
    if (!code_book %in% (codes)) {
      stop(str_c('Codebooks can only be:\n', str_c(codes, collapse = '\n')))
    }

    df_section <-
      df_codebooks %>%
      filter(nameCodeBook == code_book)

    data <-
      df_section$urlCodeBook %>%
      read_codebook(id_name = df_section$nameId) %>%
      mutate(nameCodebook = code_book %>% str_to_upper()) %>%
      select(nameCodebook, everything())
    data
  }


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

    if (!value %>% str_to_lower() %in% value_options) {
      return('')
    }
    slug
  }



parse_query <-
  function(query_parameters) {
    df_query_params <-
      data_frame(
        nameFunction = c(
          'term',
          'domain',
          'image_face_tone',
          'image_num_faces',
          'image_ocr',
          'image_tag',
          'image_web_count',
          'image_web_tag',
          'location_name',
          'location_adm1',
          'location_country',
          'near',
          'source_country',
          'source_language',
          'gkg_theme',
          'tone',
          'tone_absolute_value'
        ) ,
        nameSlug = c(
          '',
          'Domain',
          'ImageFaceTone' ,
          'ImageNumFaces',
          'ImageOCRMeta',
          'ImageTag',
          'ImageWebCount',
          'ImageWebTag',
          'Location',
          'LocationADM1',
          'LocationCC',
          "Near",
          'SourceCountry',
          'SourceLang',
          'Theme',
          "Tone",
          'ToneAbs'
        ) %>% stringr::str_to_lower(),
        typeSep = c(
          '',
          ':',
          '',
          '',
          ':',
          ':',
          '',
          ':',
          ':',
          ':',
          ":",
          ":",
          ':',
          ':',
          ":",
          '',
          ''
        )
      )

    df_call <-
      1:length(query_parameters) %>%
      map_df(function(x) {
        function_param <-
          names(query_parameters[x])
        value <-
          query_parameters[x][[1]]

        if (value %>% purrr::is_null()) {
          return(data_frame())
        }

        if (value %>% is.na()) {
          value <- NULL
        }

        if (value %>% purrr::is_null()) {
          return(data_frame())
        }
        value <- value %>% curl::curl_escape()
        has_or <-
          value %>% length() > 1
        is_quoted <-
          function_param %in% c('image_tag' , 'image_web_tag', 'image_ocr')
        if (is_quoted) {
          value <-
            str_c('%22', value, '%22')
        }

        param <-
          df_query_params %>%
          filter(nameFunction == function_param) %>%
          unite(param, nameSlug, typeSep, sep = '') %>%
          .$param

        if (has_or) {
          value

          values <- glue::glue("{param}{value}")
          values <- values %>% str_c(collapse = "%20OR%20")


          value <-
            str_c(value %>% str_c(collapse = "%20OR%20"), ")")
          param <-
            str_c('(', param, sep = '')
          df_call <-
            data_frame(nameCall = glue::glue("({values})"))
          return(df_call)
        }

        data_frame(nameCall = str_c(param, value, collapse = ''))

      })
    df_call$nameCall
  }

#' Generate a GDELT GEO V2 API query
#'
#' This function will generate a V2 GEO api query and either open
#' the URL in the browser for exploration or download the GEOJSON data
#' for users to explore in R
#'
#' @param query_parameters list of query parameters \itemize{
#'  \item term: search team
#'  \item domain: web domain
#'  \item image_face_tone: image face tone
#'  \item image_num_face: number of faces
#'  \item image_ocr: OCR to find
#'  \item image_tag: image tagcode
#'  \item image_web_tag: image web code
#'  \item image_web_count: image count
#'  \item location_name: name of the location
#'  \item location_adm1: adm code
#'  \item location_country: country
#'  \item near: specified distances
#'  \item source_country: source country
#'  \item source_language: source language
#'  \item gkg_theme: GKG theme
#'  \item tone: numeric tone
#'  \item tone_absolute_value:
#' }
#' @param mode visualization mode \itemize{
#' \item PointData
#' \item ImagePointData
#' \item PointHeatmap
#' \item ImagePointheatmap
#' \item PointAnimation
#' \item ImagePointAnimation
#' \item Country
#' \item ImageCountry
#' \item SourceCountry
#' \item ImageSourceCountry
#' \item ADM1
#' \item ImageADM1
#' }
#' @param format format of the output \itemize{
#' \item html: HTML
#' \item ImageHTML: image html
#' \item GeoJSON: geoJSON
#' \item ImageGeoJSON: geoJSON with images
#' \item Imagehtmlshow: shows overlayed pictures on map
#' }
#' @param timespan time span 15 to 1440 minutes
#' @param max_points maximum number of points
#' @param geore geore level \itemize{
#' \item 0: All locations
#' \item 1: Excludes country mentions
#' \item 2: Only landmarks
#' }
#' @param sort how to sort the data \itemize{
#' \item date: by date
#' \item toneDesc: Tone descending
#' \item toneAsc: Tone ascending
#' }
#' @param browse_url if \code{TRUE} open url in browser
#' @references \href{http://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/}{GDELT GEO API}
#'
#' @return
#' @export
#' @import purrr glue readr stringr dplyr curl tibble tidyr httr
#' @examples
generate_geo_query <-
  function(query_parameters = list(
    term = NULL,
    domain = NULL,
    gkg_theme = NULL,
    image_face_tone = NULL,
    image_num_faces = NULL,
    image_ocr = NULL,
    image_tag = NULL,
    image_web_tag = NULL,
    image_web_count = NULL,
    location_name = NULL,
    location_adm1 = NULL,
    location_country = NULL,
    near = NULL,
    source_country = NULL,
    source_language = NULL,
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
      parse_query(query_parameters = query_parameters)

    if (query_slug %>% length() > 1) {
      qs <- query_slug %>% str_c(collapse  = "%20OR%20")
      query_slug <- glue::glue("({qs})")
    }

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
      c('imagehtmlshow',
        'html',
        'ImageHTML',
        'GeoJSON',
        'ImageGeoJSON') %>% str_to_lower()
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
        parameter = 'sort',
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
      ) %>%
      str_to_lower()

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
        purrr::set_names(
          c(
            'typeMap',
            'nameProperty',
            'countProperty',
            'propertiesShareImage',
            'htmlArticle',
            'typeGeometry',
            'idRow'
          )
        ) %>%
        left_join(df_lat_lon) %>%
        suppressMessages()
      data <-
        data %>%
        mutate(dataGeoJSON = list(df_geo))
    }

    data
  }



# FreeText V2 -------------------------------------------------------------
parse_timespan <- function(timespan = "24 hours") {
  if (timespan %>% purrr::is_null()) {
    df <-
      data_frame(period_time = NA,
                 period_timeframe = NA)
    return(df)
  }

  period_time <-
    timespan %>% readr::parse_number()
  period_timeframe <-
    timespan %>% str_split('\\ ') %>% flatten_chr() %>% str_to_lower() %>% {
      .[2]
    }
  data_frame(period_time, period_timeframe)
}

parse_datetimes <-
  function(dates = NULL) {
    if (dates %>% purrr::is_null()) {
      return(invisible())
    }

    datetime_start <- dates[1] %>% lubridate::ymd_hms()
    datetime_end <- dates[2]  %>% lubridate::ymd_hms()
    data_frame(datetime_start,
               datetime_end)
  }


# http://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/

#' GDELT Free Text API 2.0 Codebooks
#'
#' @param code_book code book selection \itemize{
#' \item gkg: Global knowledge graph codes
#' \item imagetags: Image tag data store
#' \item imageweb: Image web data store
#' \item languages: Languages
#' \item countries: Countries with country code
#'
#' }
#'
#' @return
#' @export
#' @import dplyr stringr purrr glue readr
#' @examples
#' get_gdelt_codebook_ft_api(code_book = "gkg")
#' get_gdelt_codebook_ft_api(code_book = "imagetags")
#' get_gdelt_codebook_ft_api(code_book = "imageweb")
#' get_gdelt_codebook_ft_api(code_book = "languages")
get_gdelt_codebook_ft_api <-
  function(code_book = 'gkg') {
    df_codebooks <-
      data_frame(
        nameCodeBook = c(
          'gkg',
          'imagetags',
          'imageweb',
          'languages',
          'countries'
        ),
        nameId = c(
          'idGKGTheme',
          'idImageTag',
          'idImageWeb',
          'idLanguage',
          'idCountry'
        ),
        urlCodeBook = c(
          "http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT",
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGEWEBTAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT'
        )
      )
    code_book <- code_book %>% str_to_lower()
    codes <- df_codebooks$nameCodeBook %>% str_to_lower()
    if (!code_book %in% (codes)) {
      stop(str_c('Codebooks can only be:\n', str_c(codes, collapse = '\n')))
    }

    df_section <-
      df_codebooks %>%
      filter(nameCodeBook == code_book)

    data <-
      df_section$urlCodeBook %>%
      read_codebook(id_name = df_section$nameId) %>%
      mutate(nameCodebook = code_book %>% str_to_upper()) %>%
      select(nameCodebook, everything()) %>%
      suppressMessages()
    data
  }



# gdeltr2::load_needed_packages(c("glue", 'dplyr', 'jsonlite',  'tidyr', 'purrr', 'readr',  'stringr', 'anytime', 'tibble', 'curl', 'httr'))
generate_free_text_api <-
  function(query_parameters = list(
    term = '"Brooklyn Nets"',
    domain = NULL,
    image_face_tone = NULL,
    image_num_faces = NULL,
    image_ocr = NULL,
    image_tag = NULL,
    image_web_tag = NULL,
    image_web_count = NULL,
    source_country = NULL,
    gkg_theme = NULL,
    tone = NULL,
    tone_absolute_value = NULL
  ),
  source_language = "English",
  use_or = FALSE,
  mode = 'ArtList',
  #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
  format = 'JSON',
  # HTML, CSV, JSON, JSONP, RSS, RSS archive,
  search_language = 'eng',
  # http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT
  timespan = "24 hours",
  dates = NULL,
  maximum_records = 250,
  translate = NULL,
  timeline_smooth = 5,
  sort_by = 'DateDesc' # DateDesc, DateAsc, ToneDesc
  ) {
    base <- "http://api.gdeltproject.org/api/v2/doc/doc?query="

    if (query_parameters$gkg_theme %>% length() > 0) {

      if (query_parameters$gkg_theme %>% str_detect("WB_")) {
        parts <- query_parameters$gkg_theme %>% str_split('\\_') %>% flatten_chr()
        query_parameters$gkg_theme <- parts[3:length(parts)] %>% str_c(collapse = '_')
      }

      query_parameters$gkg_theme <-
        query_parameters$gkg_theme %>% stringr::str_replace_all("\\_", '\\ ')
    }

    for (i in 1:length(query_parameters)) {
      query_parameters[query_parameters[i] %>% names()] %>% is.na()
    }
    query_slug <-
      parse_query(query_parameters = query_parameters)

    if (query_slug %>% length() > 1) {
      if (use_or) {
      qs <-
        query_slug %>% str_c(collapse  = "%20OR%20")
      query_slug <- glue::glue("({qs})")
      } else {
        query_slug <-
          query_slug %>% str_c(collapse  = "%20") %>% URLencode()
      }
    }

    if (source_language %>% length() > 0) {
      source_language_slug <-
        glue::glue("%20sourcelang:{source_language}")
    } else {
      source_language_slug <- ""
    }

    mode_options <-
      c(
        "ArtList",
        "ImageCollage",
        "ImageCollageInfo",
        "ImageCollageShare",
        "TimelineVol",
        "TimelineVolInfo",
        "TimelineTone",
        "TimelineLang",
        "TimelineSourceCountry",
        "ToneChart",
        "WordCloudEnglish",
        "WordCloudNative",
        "WordCloudTheme",
        "WordCloudImageTags",
        "WordCloudImageWebTags"
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

    format_options <-
      c("HTML", "CSV", "JSON", "JSONP", "RSS", "RSS archive", "") %>%
      str_to_lower()

    format_slug <-
      generate_slug(
        parameter = 'format',
        sep = '=',
        value = format %>% str_to_lower(),
        value_options = format_options
      )

    if (timespan %>% length() > 0) {
      df_timespan <-
        timespan %>% parse_timespan()

      period <- df_timespan$period_time

      metric <- df_timespan$period_timeframe %>% str_to_lower()

      id_metric <-
        case_when(
        metric %>% str_detect("minutes") ~ '',
        metric %>% str_detect("hour") ~ 'h',
        metric %>% str_detect("day") ~ 'd',
        metric %>% str_detect("week") ~ 'w',
        metric %>% str_detect("month") ~ 'm'
      )

      timespan_slug <-
        glue::glue("&timespan={period}{id_metric}")

    } else {
      timespan_slug <- ''
    }

    if (dates %>% length() > 0) {
      dates_df <-
        dates %>% parse_datetimes()

      start_time <-
        dates_df$datetime_start %>%
        as.character() %>% str_replace_all('\\ |\\:|\\-', '')

      end_time <-
        dates_df$datetime_end %>%
        as.character() %>% str_replace_all('\\ |\\:|\\-', '')

      datetime_slug <-
        glue::glue('&startdatetime={start_time}&enddatetime={end_time}')
    } else {
      datetime_slug <- ''
    }

    sort_options <-
      c('DateDesc', 'DateAsc', 'DateAsc',
        'ToneAsc') %>%
      str_to_lower()

    sort_slug <-
      generate_slug(
        parameter = 'sort',
        sep = '=',
        value = sort_by,
        value_options = sort_options
      )

    max_slug <-
      generate_slug(
        parameter = 'maxrecords',
        sep = '=',
        value = maximum_records,
        value_options = 1:250
      )


    url_api <-
      glue(
        "{base}",
        "{query_slug}",
        "{source_language_slug}",
        '{mode_slug}',
        "{format_slug}",
        "{timespan_slug}",
        "{datetime_slug}",
        "{max_slug}",
        "{sort_slug}"
      ) %>%
      str_to_lower()

    url_api %>% str_replace_all('\\&&', '\\&')
    url_api
  }


get_gdelt_ft_api_names <-
  function() {
    df_name <-
      data_frame(nameGDELT = c("bin", "count", "date", "domain", "imageurl", "imagewebcount",
                               "imageweburls", "label", "language", "seendate", "series", "socialimage",
                               "sourcearticleurl", "sourcecountry", "timelapsesec", "title",
                               "url", "url_mobile", "value"),
                 nameActual = c("idBIN", "countValue", "datetimeData", "domainArticle", "urlImage", "countImageUses",
                                "urlsImageWeb", "labelTime", "languageArticle", "datetimeArticle", "nameSeries", "urlImage",
                                "urlArticle", "countryArticle", "countTimelapseSeconds", "titleArticle",
                                "urlArticle", "urlArticleMobile", "value")

      )
    df_name
  }

parse_json_api_2 <-
  function(url = "http://api.gdeltproject.org/api/v2/doc/doc?query=domain:netsdaily.com&timespan=1m&maxrecords=250&format=json") {
    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)


    data <-
      data[[names(data)]] %>%
      as_data_frame()

    if (data %>% tibble::has_name("imageweburls")) {
      df_urls <-
        1:length(data$imageweburls) %>%
        map_df(function(x){
          value <- data$imageweburls[[x]]

          if (value %>% length() == 0) {
            return(data_frame(idRow = x,
                              imageweburls = NA))
          }
          values <- value %>% str_c(collapse = ', ')
          data_frame(idRow = x, imageweburls = values)
        })

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        dplyr::select(-imageweburls) %>%
        left_join(
          df_urls
        ) %>%
        suppressMessages() %>%
        dplyr::select(-idRow)
    }

    if (data %>% tibble::has_name("series")) {
      data <-
        data %>%
        tidyr::unnest()
    }

    if (data %>% tibble::has_name("toparts")) {
      data <-
        data %>%
        unnest()
    }
    df_name <-
      get_gdelt_ft_api_names()

    actual_names <-
      names(data) %>%
      map_chr(function(x){
        df_name %>%
          filter(nameGDELT == x) %>%
          pull(nameActual)
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    data <-
      data %>%
      mutate_if(is.character,
                funs(ifelse(. == '', NA, .)))

    has_datetime <- data %>% names() %>% str_detect("^datetime[A-Z]") %>% sum(na.rm = TRUE) > 0

    if (has_datetime) {
      data <-
        data %>%
        mutate_at(.vars = data %>% dplyr::select(matches("^datetime[A-Z]")) %>% names(),
                  funs(. %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())))
    }

    data
  }


query_gdelt_ft_v2_api.one <-
  function(term = NULL,
           domain = NULL,
           image_face_tone = NULL,
           image_num_faces = NULL,
           image_ocr = NULL,
           image_tag = NULL,
           image_web_tag = NULL,
           image_web_count = NULL,
           source_country = NULL,
           source_language = "English",
           gkg_theme = NULL,
           tone = NULL,
           tone_absolute_value = NULL,
           use_or = FALSE,
           mode = 'ArtList',
           #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
           search_language = 'eng',
           # http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT

           timespan = "24 hours",
           dates = NULL,
           maximum_records = 250,
           translate = NULL,
           timeline_smooth = 5,
           sort_by = 'DateDesc',
           nest_data = FALSE,
           return_message = TRUE,
           trelliscope_parameters = list(
             visualize  = TRUE,

           )) {

    if (!gkg_theme %>% purrr::is_null()) {
      if (gkg_theme %>% is.na()) {
        gkg_theme <- NULL
      }
    }
    query_params <-
      list(
        term = term,
        domain = domain,
        image_face_tone = image_face_tone,
        image_num_faces = image_num_faces,
        image_ocr = image_ocr,
        image_tag = image_tag,
        image_web_tag = image_web_tag,
        image_web_count = image_web_count,
        source_country = source_country,
        gkg_theme = gkg_theme,
        tone = tone,
        tone_absolute_value = tone_absolute_value
      )

    for (i in 1:length(query_params)) {
      query_params[query_params[i] %>% names()] %>% is.na()
    }


    if (query_params %>% flatten_df() %>% ncol() == 0) {
      stop("Please enter query parameters")
    }

    df_query_params <-
      query_params %>%
      flatten_df() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    df_search <-
      df_query_params %>%
      tidyr::gather(item, value) %>%
      mutate(item = item %>% str_replace_all('\\_', ''),
             item = str_c(item, "Search", sep = '')) %>%
      spread(item, value) %>%
      mutate(isOR = use_or,
             modeSearch = mode,
             countMaximumRecords = maximum_records)

    if (mode %>% str_to_lower() %>% str_detect("timeline")) {
      df_search <-
        df_search %>%
        mutate(timelineSmoothPeriod = timeline_smooth)
    }

    url <-
      generate_free_text_api(
        query_parameters = query_params,
        use_or = use_or,
        mode = mode,
        format = "JSON",
        search_language = search_language,
        timespan = timespan,
        dates = dates,
        source_language = source_language,
        maximum_records = maximum_records,
        translate = translate,
        timeline_smooth = timeline_smooth,
        sort_by = sort_by
      )

    if (return_message) {

      search_params <-
        df_search %>%
        gather(item, value) %>%
        tidyr::unite(item, item, value, sep = ": ") %>%
        pull(item) %>%
        str_c(collapse = "\n")

      glue::glue("

Querying GDELT Free Text API 2.0 for:\n{search_params}

                 ") %>%
        message()
    }

    url <-
      url %>% str_replace_all("\\ ", "%20")

    parse_json_api_2_safe <-
        purrr::possibly(parse_json_api_2, data_frame())

      data <-
        url %>%
        parse_json_api_2_safe() %>%
        mutate(urlGDELTV2FTAPI = url) %>%
        suppressWarnings()

      if (timespan %>% length() > 0) {
        data <-
          data %>%
          mutate(periodtimeSearch = timespan)
      }

      if (dates %>% length() > 0) {

        datetimes <- dates %>% as.character() %>% str_c(collapse = "-")

        data <-
          data %>%
          mutate(datetimesSearch = datetimes)
      }

      df_search <-
        df_search %>%
        mutate(dataSearch = list(data))

      if (!nest_data) {
        df_search <-
          df_search %>%
          tidyr::unnest()
      }

      if (return_message) {
        glue::glue('
                   {nrow(data)} result(s)

                   ') %>% message()
      }

      df_search

  }

#' Query GDELT V2 Free Text API
#'
#' Queries GDELT's free text API for
#' user specified terms, machine learned items,
#' web domains and more.  Users can return raw data
#' or a Trelliscope of visual results.  Data is a available
#' in rolling three month periods.
#'
#' @param terms a vector of terms
#' @param domains a vector of webdomains
#' @param images_face_tone vector of facial tone scores
#' @param images_num_faces vector of face count
#' @param images_ocr vector of words to search for OCR'd text
#' @param images_tag vector of image tags from the image tag code book.
#' use \code{get_gdelt_codebook_ft_api(code_book = "imagetag"))} for options
#' @param images_web_tag vector of image tags from the image web tag code book.
#' use \code{get_gdelt_codebook_ft_api(code_book = "imageweb"))}
#' @param images_web_count numeric vector of number of times photo appeared
#' @param source_country character source country
#' #' see \code{get_gdelt_codebook_ft_api(code_book = "countries")} for options
#' @param source_language vector of source language - default is English
#' #' see \code{get_gdelt_codebook_ft_api(code_book = "languages")} for options
#' @param gkg_theme global knowledge graph theme
#' #' use \code{get_gdelt_codebook_ft_api(code_book = "gkg"))} for options
#' @param tone numeric tone - default (NA)
#' @param tone_absolute_value numeric tone absolute value (default NA)
#' @param use_or if \code{TRUE} chains multiple items using and or statement
#' else chained by and
#' @param mode API search mode \itemize{
#' \item  ArtList - data frame of articles for specified terms/domains/webtags/imagewebtags and OCR'd text (default)
#' \item ImageCollage - data frame of images from for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item ImageCollageInfo - data frame of images from the article for specified terms/domains/webtags/imagewebtags and OCR'd text, includes information on age of the photo and number of uses
#' \item ImageCollageShare - data frame of images shared on social media from the article for specified terms/domains/webtags/imagewebtags and OCR'd text, includes information on age of the photo and number of uses
#' \item TimelineVol - Timeline of of article volume for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineVolInfo - Timeline of of article volume with article information for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineTone - Timeline of of article tone for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineLang - Timeline of of article language for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineSourceCountry - Timeline of of article sourcelanguage for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item ToneChart - histogram of binned counts by tone for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudEnglish - word cloud of English words for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudNative - word cloud of native text for specified specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudTheme - word cloud of Global Knowledge Graph themes specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudImageTags - word cloud of resolved imagetags for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudImageWebTags - word cloud of resolved image web tags for specified terms/domains/webtags/imagewebtags and OCR'd text
#' }
#' @param search_language vector of search language - default is english
#' see \code{get_gdelt_codebook_ft_api(code_book = "languages")} for options
#' @param timespan character vector of the time frame - no more than 12 weeks -
#' default is 24 hours
#' acceptable periods include: \itemize{
#' \item hours (default)
#' \item minutes
#' \item weeks
#' \item months
#' }
#' @param dates vector of dates in YMD HMS format, default \code{NULL}
#' @param maximum_records Number between 1 and 250
#' @param translate
#' @param timeline_smooth if \code{mode} is a timeline
#' @param sort_by sorting method \itemize{
#' \item DateDesc - descending by date (default)
#' \item DateAsc - ascending by date
#' \item ToneDesc - descinding tone
#' }
#' @param visualize_results if \code{TRUE} returns a visualization
#' @param trelliscope_parameters list of parameters to pass along to trelliscope \itemize{
#' \item path: if not \code{NULL} the path to save the trelliscope
#' \item rows: rows for trelliscope
#' \item columns: columns for trelliscope
#' \item id_columns: initial columns
#' }

#' @param nest_data if \code{TRUE} returns a nested \code{data_frame()}
#' @param return_message \code{TRUE} returns a message
#'
#' @return a \code{data_frame} or a form of visualization
#' @export
#' @import glue dplyr purrr anytime stringr tidyr purrr lubridate jsonlite
#' @examples
get_data_gdelt_ft_v2_api <-
  function(terms = NA,
         domains = NA,
         images_face_tone = NA,
         images_num_faces = NA,
         images_ocr = NA,
         images_tag = NA,
         images_web_tag = NA,
         images_web_count = NA,
         source_country = NA,
         source_language = "English",
         gkg_theme = NA,
         tone = NA,
         tone_absolute_value = NA,
         use_or = FALSE,
         mode = 'ArtList',
         #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
         search_language = 'eng',
         timespan = "24 hours",
         dates = NULL,
         maximum_records = 250,
         translate = NULL,
         timeline_smooth = 5,
         sort_by = 'DateDesc',
         visualize_results = TRUE,
         trelliscope_parameters = list(
           path = NULL,
           rows = 1,
           columns = 2,
           id_columns = NULL
         ),
         nest_data = FALSE,
         return_message = TRUE) {
    df_terms <-
      list(
        data_frame(term = terms),
        data_frame(domain = domains),
        data_frame(image_ocr = images_ocr),
        data_frame(image_face_tone = images_face_tone),
        data_frame(image_num_faces =images_num_faces),
        data_frame(image_web_tag = images_web_tag),
        data_frame(image_tag = images_tag),
        data_frame(image_web_count = images_web_count)
      ) %>%
      purrr::reduce(bind_rows) %>%
      distinct()

    query_gdelt_ft_v2_api.one_safe <-
      purrr::possibly(query_gdelt_ft_v2_api.one, data_frame())

    all_data <-
      1:nrow(df_terms) %>%
      map_df(function(x){
        df_row <-
          df_terms %>%
          slice(x)

        if (df_row %>% gather(item, value) %>% filter(!value %>% is.na()) %>% nrow() == 0) {
          return(data_frame())
        }

        query_gdelt_ft_v2_api.one_safe(
          term = df_row$term,
          domain = df_row$domain,
          image_face_tone = df_row$image_face_tone,
          image_num_faces = df_row$image_num_faces,
          image_ocr = df_row$image_ocr,
          image_tag = df_row$image_tag,
          image_web_tag = df_row$image_web_tag,
          image_web_count = df_row$image_web_count,
          source_country = source_country,
          source_language = source_language,
          gkg_theme = gkg_theme,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          use_or = use_or,
          mode = mode,
          search_language = search_language,
          timespan = timespan,
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          nest_data = nest_data,
          return_message = return_message
        )

      }) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1)) %>%
      distinct()

    search_params <-
      all_data %>% dplyr::select(matches("Search")) %>% names()

    search_params <- c('modeSearch', search_params) %>% unique()

    all_data <-
      all_data %>%
      dplyr::select(one_of(search_params), everything())

    if (visualize_results && mode %>% str_to_lower() == 'artlist') {
      all_data <-
        all_data %>%
        dplyr::select(
          -one_of(
            'modeSearch',
            'countMaximumRecords',
            'urlGDELTV2FTAPI',
            'isOR',
            'periodtimeSearch',
            'urlArticleMobile'
          )
        ) %>%
        arrange(desc(datetimeArticle)) %>%
        mutate(dateArticle = anytime::anydate(datetimeArticle) %>% as.character(),
               datetimeArticle = datetimeArticle %>% as.character()) %>%
        suppressWarnings()

      check_for_trelliscope_js()

      title <-
        list("GDELT V2 FT API Search at ", Sys.Date()) %>%
        purrr::reduce(paste0)

      df_parameters <- trelliscope_parameters %>% flatten_df()

      if (!df_parameters %>% has_name('id_columns')) {
        id_columns <-
          c(
            'datetimeArticle',
            'domainArticle',
            "domainSearch",
            'imagetagSearch',
            'imagewebtagSearch',
            'imageocrSearch',
            'imagewebtagSearch',
            "titleArticle",
            "urlArticle"
          )
      } else {
        id_columns <- df_parameters$id_columns
      }

      if (!df_parameters %>% has_name('rows')) {
        rows <-
          1
      } else {
        rows <- df_parameters$rows
      }

      if (!df_parameters %>% has_name('columns')) {
        columns <-
          2
      } else {
        columns <- df_parameters$columns
      }

      has_path <-
        df_parameters %>% has_name('path'
        )

      all_data <-
        all_data %>%
        mutate(
          idArticle = 1:n(),
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(urlArticle)
        ) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        mutate_at(c('dateArticle', 'datetimeArticle'),
                  funs(. %>% as.character()))

      id_columns <- id_columns[id_columns %in% names(all_data)]

      if (has_path) {
        path_loc <-
          df_parameters$path
        viz <-
          all_data %>%
          trelliscopejs::trelliscope(
            name = title,
            nrow = rows,
            ncol = columns,
            path = path_loc,
            state = list(labels = c(id_columns))
          )
        return(viz)
      }
      viz <-
        all_data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(id_columns))
        )


      return(viz)

    }

    all_data
}