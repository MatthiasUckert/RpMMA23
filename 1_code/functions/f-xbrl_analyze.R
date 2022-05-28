xbrl_extract_line_items <- function(.tab, .tags) {
  tab_ <- dplyr::filter(.tab, tag %in% .tags) 
  
  if (nrow(tab_) == 0 | !"amount" %in% colnames(.tab)) {
    return(tibble::tibble(.rows = 0))
  }
  
  tab_ %>%
    dplyr::filter(tag %in% .tags) %>%
    dplyr::mutate(
      year = lubridate::year(endDate),
      amount = as.numeric(amount)
    ) %>%
    dplyr::group_by(year, tag) %>%
    dplyr::filter(amount == max(amount)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(tag, year, amount) %>%
    dplyr::mutate(tag = tolower(stringi::stri_replace_first_regex(tag, ".+?_", ""))) %>%
    tidyr::pivot_wider(names_from = tag, values_from = amount, values_fill = 0)
}