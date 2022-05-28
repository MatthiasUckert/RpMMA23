get_f500 <- function() {
  
  # Source: https://datahub.io/core/s-and-p-500-companies-financials#r
  
  json_file <- 'https://datahub.io/core/s-and-p-500-companies-financials/datapackage.json'
  json_data <- jsonlite::fromJSON(paste(readLines(json_file), collapse=""))

  for(i in 1:length(json_data$resources$datahub$type)){
    if(json_data$resources$datahub$type[i]=='derived/csv'){
      path_to_file = json_data$resources$path[i]
      data <- read.csv(url(path_to_file))
    }
  }
  
  return(data)
}


map_company_filings <- function(.tickers, .ownership = FALSE, .type = "", .before = "",
                                .count = 100, .page = 1, .progress = TRUE, .sleep = 0, .retry = 5) {
  sf <- purrr::safely(edgarWebR::company_filings)
  names(.tickers) <- .tickers
  
  
  pb <- progress::progress_bar$new(total = length(.tickers))
  
  lst <- purrr::map(
    .x = .tickers,
    .f = ~ {
      if (.progress) pb$tick() # Progress bar
      Sys.sleep(.sleep) # Optionally, a sleep parameter
      check <- sf(.x, .ownership, .type, .before, .count, .page) # store results
      # check if we run into an error, and repeat as long until it works
      if (!is.null(check$error)) {
        for (i in seq_len(.retry)) {
          check <- sf(.x, .ownership, .type, .before, .count, .page)
          if (is.null(check$error)) {
            break
          }
        }
      }
      return(check)
    }
  ) %>%
    purrr::transpose() %>%
    purrr::map(., purrr::compact)
  
  print(paste0("Results: ", scales::comma(length(lst$result))))
  print(paste0("Errors:  ", scales::comma(length(lst$error))))
  
  return(lst)
}

map_filing_details <- function(.id, .hrefs, .progress = TRUE, .sleep = 0, .retry = 5) {
  sf <- purrr::safely(edgarWebR::filing_details)
  names(.hrefs) <- .id
  pb <- progress::progress_bar$new(total = length(.hrefs))
  
  lst <- purrr::map(
    .x = .hrefs,
    .f = ~ {
      if (.progress) pb$tick() # Progress bar
      Sys.sleep(.sleep) # Optionally, a sleep parameter
      check <- sf(.x) # store results
      # check if we run into an error, and repeat as long until it works
      if (!is.null(check$error)) {
        for (i in seq_len(.retry)) {
          check <- sf(.x)
          if (is.null(check$error)) {
            break
          }
        }
      }
      return(check)
    }
  ) %>%
    purrr::transpose() %>%
    purrr::map(., purrr::compact)
  
  print(paste0("Results: ", scales::comma(length(lst$result))))
  print(paste0("Errors:  ", scales::comma(length(lst$error))))
  
  return(lst)
}

download_edgar_files <- function(.tab, .dir, .retry = 5, .sleep = 1) {
  if (!dir.exists(.dir)) dir.create(.dir, recursive = TRUE)
  files_zip <- list.files(.dir, pattern = "zip$")
  
  
  tab_ <- .tab %>%
    select(year, symbol, document, href, file_ext) %>%
    mutate(
      file_ext = case_when(
        file_ext == "txt" ~ "txt",
        startsWith(file_ext, "htm") ~ "htm",
        TRUE ~ "xxx"
      ),
      path_downlod = file.path(.dir, document),
      path_zip = file.path(.dir, paste0(file_ext, "_", symbol, "-", year, ".zip"))
    ) %>%
    group_by(symbol, year, path_zip) %>%
    summarise(across(c(document, href, path_downlod), list), .groups = "drop") %>%
    filter(!basename(path_zip) %in% files_zip)
  
  if (nrow(tab_) == 0) return(NULL)
  
  lst_ <- group_split(tab_, symbol, year)
  
  f_try <- function(.url, .path) {
    try(download.file(.url, .path, quiet = TRUE, mode = "wb"), silent = TRUE)
  }
  
  f_download <- function(.row, .retry) {
    
    urls_ <- unlist(.row$href)
    paths_ <- unlist(.row$path_downlod)
    
    lgl_sucess_ <- logical(length(urls_))
    
    for (i in seq_len(length(urls_))) {
      check_ <- f_try(urls_[i], paths_[i])
      
      if (inherits(check_, "try-error")) {
        for (j in 1:.retry) {
          check_ <- f_try(urls_[i], paths_[i])
          if (!inherits(check_, "try-error")) break
          Sys.sleep(.sleep)
        }
      }
      
      lgl_sucess_[i] <- ifelse(inherits(check_, "try-error"), FALSE, TRUE)
      Sys.sleep(.sleep)
      
    }
    .row[["success"]] <- list(lgl_sucess_)
    return(.row)
    
  }
  
  f_zip <- function(.row, .dir) {
    paths_ <- unlist(.row$path_downlod)
    zip_ <- .row$path_zip
    files_ <- list.files(.dir)
    paths_ <- paths_[basename(paths_) %in% files_]
    
    zip::zipr(zip_, paths_, compression_level = 9)
    invisible(file.remove(paths_))
    
  }
  
  f_all <- function(.row, .dir, .retry) {
    tab_ <- suppressMessages(suppressWarnings(invisible(f_download(.row, .retry))))
    suppressMessages(suppressWarnings(invisible(f_zip(.row, .dir))))
    return(tab_)
  }
  
  f_all(.row = lst_[[1]], .dir = .dir, .retry)
  
  pb <- progress::progress_bar$new(total = length(lst_))
  map_dfr(lst_, ~ {pb$tick(); f_all(.x, .dir, .retry)})
  
  
}