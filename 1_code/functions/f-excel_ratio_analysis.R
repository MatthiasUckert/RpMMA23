calculate_formula <- function(.tab) {
  
  f <- na.omit(unique(.tab$Calculation))
  for(i in seq_len(length(f))) {
    int <- as.integer(trimws(unlist(stri_split_regex(f[i], "\\=|\\+"))))
    
    name <- .tab %>%
      filter(id %in% int[1]) %>%
      pull(name_stand)
    
    
    tab_ <- .tab %>%
      filter(id %in% int[-1]) %>%
      select(-Calculation) %>%
      mutate(name_stand = name, id = int[1]) %>%
      group_by(id, name_stand) %>%
      summarise(across(everything(), ~ sum(., na.rm = TRUE)), .groups = "drop")
    
    .tab <- bind_rows(tab_, .tab) %>%
      distinct(id, .keep_all = TRUE)
    
    
  }
  
  .tab %>%
    arrange(id) %>%
    select(-Calculation, -id)
  
}

get_stand_statement <- function(.paths, .path_map1, .path_map2) {
  
  lst1 <- list()
  lst2 <- list()
  for (.type in c("is", "bsa", "bsl", "cf")) {
    
    sheet_ <- switch (.type, "is" = 1, "bsa" = 2, "bsl" = 3, "cf" = 4)
    map_1 <- openxlsx::read.xlsx(.path_map1, sheet_) %>%
      select(id, name = name_disp, name_stand) %>%
      mutate(name = trimws(name))
    
    map_2 <- openxlsx::read.xlsx(.path_map2, sheet_) %>%
      mutate(name = trimws(name)) %>%
      rename(name_stand = name)
    
    tab0_ <- map(.paths, ~ openxlsx::read.xlsx(.x, (sheet_ + 1))) %>%
      purrr::reduce(., full_join, by = "name") %>%
      select(-ends_with("y")) %>%
      `colnames<-`(gsub(".x", "", colnames(.), fixed = TRUE))
    cols <- c("name", sort(colnames(tab0_)[-1], decreasing = TRUE))
    tab0_ <- tab0_[, cols]
    tab0_[is.na(tab0_)] <- 0
    
    tab1 <- tab0_ %>%
      mutate(name = trimws(name)) %>%
      left_join(map_1, by = "name") %>%
      arrange(id) %>%
      select(-id)
    
    
    tab2 <- tab1 %>%
      filter(!is.na(name_stand)) %>%
      select(-name) %>%
      group_by(name_stand) %>%
      summarise(across(everything(), ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
      full_join(map_2, by = c("name_stand")) %>%
      arrange(id) %>%
      filter(!id == 0)
    
    if (.type == "cf") {
      is <- lst2$is
      
      tab2[1, -c(1, 11, 12)] <- is[10, -1] - is[6, -1] - is[7, -1]
      tab2[2, -c(1, 11, 12)] <- is[11, -1]
      tab2[8, -c(1, 11, 12)] <- is[7, -1]
      tab2[12, -c(1, 11, 12)] <- is[6, -1]
    } 
    
    tab2 <- calculate_formula(tab2)
    tab2[is.na(tab2)] <- 0
    lst2[[.type]] <- as_tibble(tab2)
    lst1[[.type]] <- as_tibble(select(tab1, -name_stand))
  }
  
  bind_rows(
    enframe(lst1) %>% mutate(type = "Original"),
    enframe(lst2) %>% mutate(type = "Standardized")
  )
  
}

calculate_wb_fn <- function(.dir, .name) {
  dir_ <- normalizePath(.dir)
  hash_ <- digest::digest(.name, algo = "xxhash32")
  
  path_vbs_ <- normalizePath(file.path(dir_,  paste0(hash_, ".vbs")), mustWork = FALSE)
  path_xlsx_ <- normalizePath(file.path(dir_,  .name), mustWork = FALSE)
  
  macro_calculate_wb <- file(path_vbs_)
  writeLines(c("Const xlVisible = -1",
               "Dim objExcel",
               "Dim objWb",
               "Dim objws",
               "Dim strFileName",
               paste0("strFileName = \"", path_xlsx_,"\""),
               "On Error Resume Next",
               "Set objExcel = CreateObject(\"excel.application\")",
               "Set objWb = objExcel.Workbooks.Open(strFileName)",
               "objExcel.DisplayAlerts = False",
               "objWb.Save",
               "objWb.Close SaveChanges=True",
               "objExcel.Close",
               "objExcel.Quit",
               "set objWb = Nothing",
               "set objExcel = Nothing",
               "On Error GoTo 0")
             , macro_calculate_wb)
  close(macro_calculate_wb)

  shell(shQuote(string = path_vbs_), wait = T, mustWork = TRUE, intern = TRUE)
  
  file.remove(path_vbs_)
}

# .tab = tab_fs[[1]]
# .name = names(tab_fs)[1]
# .dir = lst_paths$dir_excel
# .template = lst_paths$path_template
write_to_excel <- function(.tab, .name, .dir, .template) {
  wb <- openxlsx::loadWorkbook(.template)
  
  .row <- 1
  openxlsx::writeData(wb, "Original Statements", .tab$value[[1]], startRow = .row)
  .row <- .row + nrow(.tab$value[[1]]) + 3
  openxlsx::writeData(wb, "Original Statements", .tab$value[[2]], startRow = .row, colNames = FALSE)
  .row <- .row + nrow(.tab$value[[2]]) + 1
  openxlsx::writeData(wb, "Original Statements", .tab$value[[3]], startRow = .row, colNames = FALSE)
  .row <- .row + nrow(.tab$value[[3]]) + 3
  openxlsx::writeData(wb, "Original Statements", .tab$value[[4]], startRow = .row, colNames = FALSE)
  openxlsx::setColWidths(wb, "Original Statements", cols = 1:ncol(.tab$value[[1]]), widths = "auto")
  
  
  openxlsx::writeData(wb, "Standardized Statements", .tab$value[[5]], startRow = 2)
  openxlsx::writeData(wb, "Standardized Statements", .tab$value[[6]], startRow = 20, colNames = FALSE)
  openxlsx::writeData(wb, "Standardized Statements", .tab$value[[7]], startRow = 36, colNames = FALSE)
  openxlsx::writeData(wb, "Standardized Statements", .tab$value[[8]], startRow = 53, colNames = FALSE)
  
  path_ <- file.path(.dir, paste0(.name, ".xlsx"))
  openxlsx::saveWorkbook(wb, path_, overwrite = TRUE)
  
  # .dir  = dirname(path_)
  # .name = basename(path_)
  calculate_wb_fn(dirname(path_), basename(path_))
  
}