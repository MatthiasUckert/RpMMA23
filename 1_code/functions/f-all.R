download_google_drive <- function(.id, .dir = NULL, .read_fun = NULL, .overwrite = FALSE) {
  suppressMessages(googledrive::drive_deauth())
  suppressMessages(googledrive::drive_user())
  pub_fil_ <- googledrive::drive_get(googledrive::as_id(.id))
  
  
  if (!is.null(.dir)) {
    dir.create(.dir, recursive = TRUE, showWarnings = FALSE)
    path_ <- file.path(.dir, pub_fil_$name)
  } else {
    path_ <- file.path(tempdir(), pub_fil_$name)
  }
  
  if (!file.exists(path_) | .overwrite) {
    googledrive::drive_download(pub_fil_, path_, NULL, .overwrite, FALSE)
  }
  
  
  if (is.null(.read_fun)) {
    return(NULL)
  } else {
    return(.read_fun(path_))
  }
}

list_files_tab <- function(dirs, reg = "*", id = "doc_id", rec = FALSE, info = FALSE) {
  path <- file_ext <- NULL
  
  tab_fil <- purrr::map_dfr(
    .x = dirs,
    .f = ~ tibble::tibble(path = list.files(.x, reg, F, T, rec))
  ) %>%
    dplyr::mutate(
      file_ext = paste0(".", tools::file_ext(path)),
      !!dplyr::sym(id) := stringi::stri_replace_last_fixed(basename(path), file_ext, "")
    ) %>%
    dplyr::select(!!dplyr::sym(id), file_ext, path)
  
  if (info) {
    tab_fil <- dplyr::bind_cols(tab_fil, tibble::as_tibble(file.info(tab_fil$path)))
  }
  
  return(tab_fil)
}

# .string <- text_
remove_html_tags <- function(.string, rm_linebreaks = TRUE) {
  string_ <- .string
  
  if (rm_linebreaks) {
    string_ <- stringi::stri_replace_all_regex(string_, "([[:blank:]]|[[:space:]])+", " ")  
  }
  
  string_ <- string_ %>%
    stringi::stri_replace_all_regex(., "(?i)<script.*?>.*?</script.*?>", "") %>%
    stringi::stri_replace_all_regex(., "(?i)<xbrl.*?>.*?</xbrl.*?>", "") %>%
    stringi::stri_replace_all_regex(., "(?i)<xml.*?>.*?</xml.*?>", "") %>%
    stringi::stri_replace_all_regex(., "(?i)<link:.*?/>", "") %>%
    stringi::stri_replace_all_regex(., "(?i)<table.*?>.*?</table.*?>", "") %>%
    # stringi::stri_replace_all_regex(., "(?i)<ix.*?>.*?</ix.*?>", "") %>%
    stringi::stri_replace_all_regex(., "(?i)<.*?>|&#.+?;|&lt;.*?&gt;", "") %>%
    stringi::stri_replace_all_regex(., "(?i)&nbsp;", " ") %>%
    stringi::stri_replace_all_regex(., "(?i)&amp;", "&") 
  
  if (rm_linebreaks) {
    string_ <- stringi::stri_replace_all_regex(string_, "([[:blank:]]|[[:space:]])+", " ")  
  }

  return(trimws(string_))
}

create_dirs <- function(.dirs) {
  dirs0_ <- unlist(.dirs)
  file_ext_ <- tools::file_ext(dirs0_)
  dirs1_ <- dplyr::if_else(file_ext_ == "", dirs0_, dirname(dirs0_))

  purrr::walk(
    .x = unique(dirs1_),
    .f = ~ {
      if (!dir.exists(.x)) {
        dir.create(.x, showWarnings = FALSE, recursive = TRUE)
      }
    }
  )
  
  purrr::map(.dirs, normalizePath)
  
  
}



show_table <- function(.tab, .n = Inf) {
  if (is.infinite(.n)) {
    tab_ <- .tab
  } else {
    tab_ <-  dplyr::slice(.tab, 1:.n)
  }
  
  tab_ %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper("hover") %>%
    kableExtra::kable_styling(full_width = FALSE) %>% 
    kableExtra::scroll_box(width = "100%")
}

# remove_html_tags('<HR SIZE="3" NOSHADE COLOR="#000000" ALIGN="left"> <P STYLE="margin-top:0px;margin-bottom:0px" ALIGN="center"><FONT FACE="Times New Roman" SIZE="5"><B>UNITED STATES </B></FONT></P> <P
# STYLE="margin-top:0px;margin-bottom:0px" ALIGN="center"><FONT FACE="Times New Roman" SIZE="5"><B>SECURITIES AND EXCHANGE COMMISSION </B></FONT></P> <P STYLE="margin-top:0px;margin-bottom:0px" ALIGN="center"><FONT FACE="Times New Roman"
# SIZE="3">')
