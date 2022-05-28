source("1_code/functions/f-all.R")

get_lm_stop <- function() {
  urls <- c(
    "https://drive.google.com/file/d/0B4niqV00F3mseWZrUk1YMGxpVzQ/view?usp=sharing",
    "https://drive.google.com/file/d/0B4niqV00F3msV1h6N2RhLTNBZG8/view?usp=sharing",
    "https://drive.google.com/file/d/0B4niqV00F3msT18yTE42VWdLdVE/view?usp=sharing",
    "https://drive.google.com/file/d/0B4niqV00F3msVGc4NldrajhQbDg/view?usp=sharing",
    "https://drive.google.com/file/d/0B4niqV00F3msSktONVhfaElXeEk/view?usp=sharing",
    "https://drive.google.com/file/d/0B4niqV00F3msTXRiSmUxRmZWUFE/view?usp=sharing",
    "https://drive.google.com/file/d/0B4niqV00F3msYlZxTm5QaEQ1dTQ/view?usp=sharing"
  )
  ids <- unlist(stringi::stri_split_fixed(urls, "/"))
  ids <- ids[startsWith(ids, "0B4")]


  dir_ <- tempdir()
  tab_lm_stop <- purrr::map_dfr(
    .x = ids,
    .f = ~ download_google_drive(.x, dir_, function(.x) read_tsv(.x, FALSE, col_types = cols("c")))
  ) %>%
    dplyr::rename(word = X1) %>%
    mutate(
      word = word %>%
        stringi::stri_enc_toascii() %>%
        stringi::stri_replace_all_regex("\\|.*", "") %>%
        trimws() %>%
        tolower()
    ) %>%
    dplyr::distinct()
}

pdf_read_and_tokenize <- function(.path, .dir) {
  tab_ <- try(readtext::readtext(.path))
  if (inherits(tab_, "try-error")) {
    return(NULL)
  } else {
    tab_ %>%
      dplyr::select(-doc_id) %>%
      tidytext::unnest_tokens(word, text) %>%
      dplyr::filter(!grepl("\\d", word)) %>%
      dplyr::filter(nchar(word) > 3) %>%
      fst::write_fst(file.path(.dir, gsub("\\.pdf$", ".fst", basename(.path))))
  }
  
  
}

# .path <- tab_files_sec_use$path[1]
# browseURL(.path)
# cat(stri_sub(tab_$text, 1000, 2000))
sec_read_and_tokenize <- function(.path, .dir = NULL) {
  tab_  <- try(readtext::readtext(.path))
  
  if (inherits(tab_, "try-error")) {
    return(NULL)
  } else {
    text_ <- stringi::stri_replace_all_regex(tab_$text, "([[:blank:]]|[[:space:]])+", " ")
    text_ <- unlist(stri_extract_all_regex(text_, "<DOCUMENT>.+?</DOCUMENT>"))
    text_ <- text_[grepl("<TYPE>10-K|<TYPE>EX-", text_)]
    text_ <- paste(text_, collapse = " ")
    
    tab_ <- tab_ %>%
      dplyr::select(-doc_id) %>%
      dplyr::mutate(text = remove_html_tags(text_)) %>%
      tidytext::unnest_tokens(word, text) %>%
      dplyr::filter(!grepl("\\d", word)) %>%
      dplyr::filter(nchar(word) > 3)
  }
  
  if (!is.null(.dir)) {
    fst::write_fst(tab_, file.path(.dir, gsub("\\.zip$", ".fst", basename(.path))))
    return(NULL)
  } else {
    return(tab_)
  }
}



get_ngrams <- function(.path, .ngram, .stop = tibble(word = "", .rows = 0), .rm_num = TRUE) {
  tab_ <- fst::read_fst(.path) %>%
    dplyr::filter(nchar(word) > 4) %>%
    dplyr::filter(!stringi::stri_detect_regex(word, "[[:punct:]]")) %>%
    dplyr::anti_join(dplyr::mutate(.stop, word = tolower(word)), by = "word")
  
  if (.rm_num) {
    tab_ <- dplyr::filter(tab_, !grepl("\\d", word))
  }
  
  if (.ngram == 1) {
    tab_ <- tab_ %>%
      dplyr::count(word, sort = TRUE) %>%
      dplyr::mutate(p = n / sum(n))
  } else {
    tab_ <- tab_ %>%
      dplyr::summarise(word = paste(word, collapse = " ")) %>%
      tidytext::unnest_tokens(word, word, token = "ngrams", n = .ngram) %>%
      dplyr::count(word, sort = TRUE) %>%
      dplyr::mutate(p = n / sum(n))
  }
  
  return(tab_)
}


prep_top_n_by <- function(.tab_word, .tab_data, .col, .n = 10) {
  tab_data_ <- dplyr::select(.tab_data, doc_id, {{ .col }})
  .tab_word %>%
    dplyr::left_join(tab_data_, by = "doc_id") %>%
    dplyr::group_by({{ .col }}, word)  %>%
    dplyr::summarise(n = sum(n), .groups = "drop_last")  %>%
    dplyr::arrange(dplyr::desc(n), .by_group = TRUE) %>%
    dplyr::slice(1:.n) %>%
    dplyr::ungroup() 
}

display_top_n_by <- function(.tab, .col) {
  for (i in unique(dplyr::pull(.tab, {{ .col }}))) {
    print(
      .tab %>%
        dplyr::filter({{ .col }} == i) %>%
        dplyr::mutate(word = reorder(word, n)) %>%
        ggplot2::ggplot(aes(n, word)) +
        ggplot2::geom_col(fill = "blue", color = "grey") +
        ggplot2::scale_x_continuous(labels = scales::comma) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_bw() + 
        ggplot2::ggtitle(i)
    )
  }
}


compare_word_frequqncies <- function(.tab, .tab_data) {
  .tab %>%
    left_join(select(.tab_data, doc_id, country), by = "doc_id") %>%
    group_by(country, word) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    mutate(p = n / sum(n, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = country, values_from = p) %>%
    filter(!is.na(Europe) & !is.na(`United States`)) %>%
    ggplot(aes(x = `United States`, y = Europe)) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3, color = "blue") +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "lightblue", high = "blue") +
    theme(legend.position="none") +
    theme_bw()
}

show_sentiment <- function(.tab, .type = c("pos/neg", "uncertainty", "litigious", "constraining", "superfluous")) {
  tab_ <- .tab %>%
    left_join(get_sentiments("loughran"), by = "word") %>%
    left_join(select(tab_data, doc_id, country, year), by = "doc_id") %>%
    group_by(country, year, sentiment) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    mutate(p = n / sum(n)) %>%
    ungroup() %>%
    mutate(year = as.integer(year))
  
  
  if (.type == "pos/neg") {
    tab_ <- tab_ %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      mutate(p = if_else(sentiment == "negative", -p, p))
  } else {
    tab_ <- tab_ %>%
      filter(sentiment %in% c(.type))
  }
  
  tab_ %>%
    ggplot(aes(x = year, y = p, color = sentiment, fill = sentiment)) +
    geom_area(stat = "identity", alpha = 0.4) +
    geom_point(size = 1) +
    facet_wrap(~country, nrow = 1) +
    theme_bw() + 
    scale_x_continuous(breaks= pretty_breaks()) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL) +
    geom_vline(xintercept = c(2009)) +
    theme(legend.position = "none") + 
    ggtitle(toupper(.type))
}