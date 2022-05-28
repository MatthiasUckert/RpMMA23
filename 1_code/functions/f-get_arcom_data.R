get_company_table <- function(.url) {
  html_ <- read_html(.url) %>%
    html_elements(xpath = "/html/body/div[1]/section[1]/div[2]/ul") %>%
    html_nodes("li")
  
  html_ <- html_[-1]
  headers_ <- c("company", "industry", "sector", "premium", "request")
  
  f_get_table <- function(.node) {
    spans_ <- html_elements(.node, "span")
    links_ <- url_absolute(html_attr(html_elements(spans_, "a"), "href"), "https://www.annualreports.com/Company/")[1]
    
    vals_ <- map_chr(spans_, html_text2)
    vals_[4] <- html_attr(html_elements(spans_, "img"), "src")
    
    names(vals_) <- headers_
    
    mutate(pivot_wider(enframe(vals_)), link = links_)
  }
  
  
  map_dfr(seq_len(length(html_)), ~ f_get_table(html_[[.x]]))
  
}

get_infos <- function(.path) {
  
  html_ <- read_html(.path)
  ticker_ <- html_text2(html_elements(html_, ".ticker_name"))
  ticker_ <- ifelse(length(ticker_) != 1, NA_character_, ticker_)
  
  
  
  links_ <- html_ %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    tibble::enframe(value = "link") %>%
    dplyr::filter(grepl("pdf$", link, ignore.case = TRUE)) %>%
    dplyr::distinct(link) %>%
    dplyr::mutate(
      ticker = ticker_,
      year = as.integer(stringi::stri_extract_last_regex(link, "\\d{4}")),
      doc_id = gsub("\\.html$", "", basename(.path)),
      link = url_absolute(link, "https://www.annualreports.com/Company/")
    ) %>%
    dplyr::select(doc_id, ticker, year, link)
  
}

standardize_name <- function(.name) {
  .name %>%
    stringi::stri_trans_general(., "latin-ascii") %>%
    stringi::stri_replace_all_regex(., "[[:punct:]]", " ") %>%
    stringi::stri_replace_all_regex(., "([[:space:]]|[[:blank:]])+", " ") %>%
    tolower() %>%
    trimws()
} 

downlad_ar <- function(.url, .dir) {
  try(download.file(
    url = .url,
    destfile = file.path(.dir, basename(.url)),
    mode = "wb",
    quiet = TRUE
  ))
}