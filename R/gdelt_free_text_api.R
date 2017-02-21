
# trelliscope -------------------------------------------------------------

check_for_trelliscope_js <-
  function() {
    missing <-
      installed.packages() %>% as_data_frame() %>%
      filter(Package == 'trelliscopejs') %>%
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
      mutate_each_(funs(str_trim),
                   vars = c('source', 'language', 'country')) %>%
      suppressWarnings()

    return(source_df)
  }

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

    if (term %>% is.na() & !domain %>% is.na) {
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
      '&sortby:' %>%
      paste0(slug_sort)

    if (!max_rows %>% is.na) {
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
      str_replace_all('\\);','')

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

    url_df <-
      data_frame(
        term,
        titleArticle,
        urlArticle = url.source,
        dateTimeData = Sys.time(),
        urlSearch = url
      ) %>%
      bind_cols(sources %>%
                  parse_source()) %>%
      suppressWarnings()

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

    if (!domain %>% is.na) {
      url_df %<>%
        mutate(domainSearch = domain) %>%
        dplyr::select(term, domainSearch, everything())
    }

    if (!tone_more_than %>% is.na) {
      url_df %<>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na) {
      url_df %<>%
        mutate(tone_less_than)
    }

    if (only_english == T) {
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

    if (restrict_to_usa == T) {
      url_df <-
        url_df %>%
        dplyr::filter(countryArticle == 'United States')
    }

    if (return_message == T) {
      "You got " %>%
        paste0(url_df %>% nrow, ' urls for ', term_word, ' at ', Sys.time()) %>%
        message()
    }

    return(url_df)

  }

#' Returns GDELT free text API results for multiple terms
#'
#' @param terms
#' @param domain
#' @param return_image_url
#' @param last_minutes
#' @param max_rows
#' @param search_language
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param only_english
#' @param return_message
#' @import tidyr stringr rvest tidyverse dplyr trelliscopejs devtools
#' @importFrom lubridate with_tz
#' @importFrom lubridate mdy_hm
#' @importFrom magrittr %<>%
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom purrr flatten_df
#' @importFrom xml2 read_html
#' @return
#' @export
#'
#' @examples
#' get_data_ft_api_terms(terms = c('"Kevin Durant"','"Stephen Curry"'), only_english = T)
get_data_ft_api_terms <-
  function(terms = c('"Kevin Durant"','"Stephen Curry"'),
           visualize_results = TRUE,
           domain = NA,
           dedeup_results = T,
           restrict_to_usa = F,
           only_english = F,
           return_image_url = T,
           last_minutes = NA,
           max_rows = 1000,
           search_language = 'English',
           source_language = 'English',
           sort_by = 'date',
           nest_data = F,
           return_message = T) {

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
      purrr::map_df(
        function(x) {
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
        }
      ) %>%
      arrange(desc(dateTimeArticle))

    if (visualize_results) {
      check_for_trelliscope_js()
      title <-
        list("GDELT Term Search as of ", Sys.time()) %>%
        purrr::reduce(paste0)

      viz <-
        all_data %>%
        mutate(idArticle = 1:n(),
               panel = trelliscopejs::img_panel(urlThumbnail),
               urlArticle = trelliscopejs::cog_href(urlArticle)) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = 2,
          ncol = 3,
          state = list(labels = c(
            "term", "titleArticle", "urlArticle"
          ))
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

#' Returns GDELT free text API results for multiple webdomains
#'
#' @param term
#' @param visualize_results
#' @param domains
#' @param return_image_url
#' @param last_minutes
#' @param max_rows
#' @param search_language
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param only_english
#' @param return_message
#' @param nest_data
#' @import tidyr stringr rvest tidyverse trelliscopejs purrr dplyr devtools
#' @importFrom xml2 read_html
#' @return
#' @export
#'
#' @examples
#' get_data_ft_api_domains(domains = c('realdeal.com', 'pehub.com', 'sbnation.com', 'wsj.com', 'seekingalpha.com', 'washingtonpost.com', 'nytimes.com')) %>% View

get_data_ft_api_domains <-
  function(domains = c('washingtonpost.com', 'nytimes.com'),
           visualize_results = TRUE,
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
      as_data_frame %>%
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
      arrange(desc(dateTimeArticle))

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
        list("GDELT Domain Search as of ", Sys.time()) %>%
        purrr::reduce(paste0)

      viz <-
        all_data %>%
        mutate(idArticle = 1:n(),
               panel = trelliscopejs::img_panel(urlThumbnail),
               urlArticle = trelliscopejs::cog_href(urlArticle)) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = 2,
          ncol = 3,
          state = list(labels = c(
            "domainSearch", "titleArticle", "urlArticle"
          ))
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

    if (term %>% is.na) {
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

    if (term %>% is.na() & !domain %>% is.na) {
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
      '&sortby:' %>%
      paste0(slug_sort)

    if (dedeup_results == T) {
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

    if (!domain %>% is.na) {
      wordcloud_data %<>%
        mutate(domainSearch = domain) %>%
        dplyr::select(term, domainSearch, everything())
    }

    if (term %>% is.na()) {
      wordcloud_data <-
        wordcloud_data %>%
        dplyr::select(-term)
    }

    if (!tone_more_than %>% is.na) {
      wordcloud_data %<>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na) {
      wordcloud_data %<>%
        mutate(tone_less_than)
    }

    if (return_message == T) {
      "You got " %>%
        paste0(wordcloud_data %>% nrow,
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
#' @import tidyr stringr rvest tidyverse
#' @return
#' @export
#'
#' @examples

get_data_wordcloud_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
           term = NA,
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
      failwith(NULL, get_data_wordcloud_ft_api)

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
      seq_len(var_matrix %>% nrow) %>%
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
#' @param domain domain name
#' \code{c(NA,'domain_name')}
#' @param return_image_url
#' @param last_minutes how many prior minutes
#' @param max_rows number of rows
#' @param tone_less_than tone more than specified number
#' @param tone_more_than tone less than specified number
#' @param search_language
#' @param source_language
#' @param sort_by How do you wish to sort the data
#' \code{c('date', 'relevence', 'tone.ascending', 'tone.descending')}
#' @param dedeup_results return unique results
#' \code{c(T, F)}
#' @param only_english return only english results
#' \code{c(T, F)}
#' @param return_message
#' \code{c(T, F)}
#' @param nest_data returns a nested data frame
#' \code{c(T, F)}
#' @importFrom tidyr nest
#' @import tidyr stringr rvest tidyverse
#' @return
#' @export
#'
#' @examples
get_data_wordcloud_ft_api_terms <-
  function(terms = c('"Donald Trump"', '"Hilary Clinton"'),
           domain = NA,
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
      failwith(NULL, get_data_wordcloud_ft_api)

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
      seq_len(var_matrix %>% nrow) %>%
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

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }

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

  if (term %>% is.na) {
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

  if (term %>% is.na() & !domain %>% is.na) {
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
    '&sortby:' %>%
    paste0(slug_sort)



  if (dedeup_results == T) {
    dup_slug <-
      '&dropdup=true'
  } else {
    dup_slug <-
      ''
  }
  if (is_tone == T) {
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

  sentiment_data %<>%
    mutate(
      dateTimeSentiment = dateTime_human.url %>% lubridate::mdy_hms(tz = 'UTC') %>%  lubridate::with_tz(Sys.timezone()),
      dateSentiment = dateTime_human.url %>% lubridate::mdy_hms(tz = 'UTC') %>% as.Date()
    ) %>%
    dplyr::select(-c(dateTime.url, dateTime_human.url)) %>%
    dplyr::select(term, dateTimeSentiment, dateSentiment, everything()) %>%
    dplyr::rename(urlSearch = url, dateTimeData = dateTimeData)

  if (!domain %>% is.na) {
    sentiment_data %<>%
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

  if (return_message == T) {
    "You got " %>%
      paste0(sentiment_data %>% nrow,
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
#' @import tidyr stringr rvest tidyverse
#' @export
#'
#' @examples

get_data_sentiment_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
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
      failwith(NULL, get_data_sentiment_ft_api)

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
      seq_len(var_matrix %>% nrow) %>%
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
#'
#' @examples
#' get_data_sentiment_ft_api_terms(terms = c("Zika", '"Golden State Warriors"')) %>% View
get_data_sentiment_ft_api_terms <-
  function(terms = c("Zika", '"Golden State Warriors"'),
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
      failwith(NULL, get_data_sentiment_ft_api)

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
      seq_len(var_matrix %>% nrow) %>%
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

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }


#' get_codebook_stability_locations
#'
#' @return
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

remove_full_na_column <-
  function(data, col_name = c('codeLocation')) {
    col_selected <-
      data %>%
      dplyr::select(one_of(col_name)) %>%
      extract2(1)

    if (col_selected %>% is.na() %>% sum  / length(col_selected) == 1) {
      data <-
        data %>%
        dplyr::select(-one_of(col_name))
    }
    return(data)
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

      'To save time please run the function get_codes_stability_locations() and save to a dataframe called location_codes prior to running this function' %>%
        message
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

    if (!time_period %>% str_to_lower() %in% c('daily', '15min', '15minutes', '15 minutes', '15 minute periods')) {
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

    if (use_multi_locations == T &
        (!location_id %>% nchar == 2)) {
      use_multi_locations <-
        F
      "You entered detailed location which cannot have multi locations\nthis analysis will still run though" %>%
        message
    }

    if (use_multi_locations == T) {
      mode_slug <-
        "&MODE=multi"
    } else {
      mode_slug <-
        ''
    }

    if (day_moving_average %>% is.na) {
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

    if (use_multi_locations == T) {
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
      remove_full_na_column(col_name = c("codeLocation")) %>%
      remove_full_na_column(col_name = "dayMovingAverage")

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
#' @param variable_names Specify variables they can include
#' \code{c('instability', 'conflict', 'protest', 'tone', 'relative mentions')}
#' @param days_moving_average Specify day moving average, NA is unsmoothed
#' @param time_periods Specified time period
#'  \code{c('daily', '15min')}
#' @param use_multi_locations If a country is selected do you want to select all the cities
#' \code{c(T,F)}
#' @param return_wide Return data in wide form
#' @param nest_data Return data in nested form
#' \code{c(T,F)}

#' @param return_message Return a message
#'
#' @return
#' @export
#' @import tidyr stringr rvest tidyverse
#' @importFrom magrittr extract2
#' @examples
get_data_locations_instability_api <-
  function(location_ids = c('US', 'IS', "TU"),
           variable_names = c('instability', 'conflict', 'tone', 'protest'),
           days_moving_average = NA,
           time_periods = 'daily',
           use_multi_locations = F,
           return_wide = T,
           nest_data = F,
           return_message = T) {
    get_data_location_instability_api_safe <-
      failwith(NULL, get_data_location_instability_api)

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
      seq_len(var_matrix %>% nrow) %>%
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
      read_csv %>%
      set_names('nameTerm') %>%
      suppressMessages()

    data <-
      data %>%
      mutate(isGDELTTag = nameTerm %>%  grepl("[[:upper:]]+$|\\_", .),
             datetimeData = Sys.time())

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
