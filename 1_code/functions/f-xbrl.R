own_XBRL <- function() {
  self <- list(
    element = NULL, role = NULL, label = NULL,
    presentation = NULL, definition = NULL, calculation = NULL,
    context = NULL, unit = NULL, fact = NULL, footnote = NULL
  )
  cache.dir <- NULL
  discovered.files <- NULL
  doc.inst <- NULL
  dname.inst <- NULL
  verbose <- FALSE
  inst.lnkb <- NULL
  
  fixFileName <- function(dname, file.name) {
    if (!(substr(file.name, 1, 5) %in% c("http:", "https"))) {
      if (substr(file.name, 1, 5) == "../..") {
        file.name <- paste0(
          dirname(dirname(dname)),
          "/", substr(file.name, 7, nchar(file.name))
        )
      }
      else if (substr(file.name, 1, 2) == "..") {
        file.name <- paste0(dirname(dname), "/", substr(
          file.name,
          4, nchar(file.name)
        ))
      }
      else {
        file.name <- paste0(dname, "/", file.name)
      }
    }
    file.name
  }
  
  
  setVerbose <- function(newVerbose) {
    oldVerbose <- verbose
    verbose <<- newVerbose
    oldVerbose
  }
  setCacheDir <- function(new.cache.dir) {
    if (!file.exists(new.cache.dir)) {
      dir.create(new.cache.dir)
    }
    cache.dir <<- new.cache.dir
  }
  fileFromCache <- function(file) {
    if (!(gsub("^(http|https|ftp)://.*$", "\\1", file) %in%
          c("http", "https", "ftp"))) {
      return(file)
    }
    bname <- basename(file)
    cached.file <- paste0(cache.dir, "/", bname)
    
    if (!file.exists(cached.file)) {
      if (verbose) {
        cat("Downloading to cache dir...")
      }
      status <- try(download.file(file, cached.file, quiet = !verbose),
                    silent = TRUE
      )
      if (class(status)[1] == "try-error" || status ==
          1) {
        unlink(cached.file)
        stop(status, "\n")
      }
    }
    else {
      if (verbose) {
        cat("Using file from cache dir...\n")
      }
    }
    cached.file
  }
  openInstance <- function(file.inst) {
    dname.inst <<- dirname(file.inst)
    if (!is.null(cache.dir)) {
      file.inst <- fileFromCache(file.inst)
      inst.lnkb <<- file.inst
    }
    doc.inst <<- XBRL::xbrlParse(file.inst)
  }
  deleteCachedInstance <- function() {
    if (verbose) {
      cat("Deleting the following downloaded and/or cached files...\n")
      print(inst.lnkb)
    }
    unlink(inst.lnkb)
    if (verbose) {
      cat("Done...\n")
    }
  }
  getSchemaName <- function() {
    fixFileName(dname.inst, .Call("xbrlGetSchemaName", doc.inst,
                                  PACKAGE = "XBRL"
    ))
  }
  
  processSchema <- function(file, level = 1) {
    if (verbose) {
      cat("Schema: ", file, "\n")
    }
    if (length(which(discovered.files == file)) > 0) {
      if (verbose) {
        cat("Already discovered. Skipping\n")
      }
      return(NULL)
    }
    
    discovered.files <<- c(discovered.files, file)
    dname <- dirname(file)
    if (level >= 1 && !is.null(cache.dir)) {
      if (verbose) {
        cat("Level:", level, "==>", file, "\n")
      }
      file <- fileFromCache(file)
      if (level == 1) {
        inst.lnkb <<- c(inst.lnkb, file)
      }
    }
    doc <- XBRL::xbrlParse(file)
    if (level == 1) {
      processRoles(doc)
    }
    processElements(doc)
    linkbaseNames <- .Call("xbrlGetLinkbaseNames", doc,
                           PACKAGE = "XBRL"
    )
    importNames <- .Call("xbrlGetImportNames", doc, PACKAGE = "XBRL")
    .Call("xbrlFree", doc, PACKAGE = "XBRL")
    for (linkbaseName in linkbaseNames) {
      linkbaseName <- fixFileName(dname, linkbaseName)
      if (verbose) {
        cat(file, " ==> Linkbase: ", linkbaseName, "\n")
      }
      processLinkbase(linkbaseName, level + 1)
    }
    for (importName in importNames) {
      importName <- fixFileName(dname, importName)
      if (verbose) {
        cat(file, " ==> Schema: ", importName, "\n")
      }
      processSchema(importName, level + 1)
    }
  }
  processRoles <- function(doc) {
    if (verbose) {
      cat("Roles\n")
    }
    self$role <<- rbind(self$role, .Call("xbrlProcessRoles",
                                         doc,
                                         PACKAGE = "XBRL"
    ))
  }
  processElements <- function(doc) {
    if (verbose) {
      cat("Elements\n")
    }
    self$element <<- rbind(self$element, .Call("xbrlProcessElements",
                                               doc,
                                               PACKAGE = "XBRL"
    ))
  }
  processLinkbase <- function(file, level) {
    if (verbose) {
      cat("Linkbase: ", file, "\n")
    }
    if (length(which(discovered.files == file)) > 0) {
      if (verbose) {
        cat("Already discovered. Skipping\n")
      }
      return(NULL)
    }
    discovered.files <<- c(discovered.files, file)
    if (level >= 2 && !is.null(cache.dir)) {
      if (verbose) {
        cat("Level:", level, "==>", file, "\n")
      }
      file <- fileFromCache(file)
      inst.lnkb <<- c(inst.lnkb, file)
    }
    doc <- XBRL::xbrlParse(file)
    if (!processLabels(doc)) {
      if (!processPresentations(doc)) {
        if (!processDefinitions(doc)) {
          processCalculations(doc)
        }
      }
    }
    .Call("xbrlFree", doc, PACKAGE = "XBRL")
  }
  processLabels <- function(doc) {
    pre.length <- length(self$label)
    self$label <<- rbind(self$label, ans <- .Call("xbrlProcessLabels",
                                                  doc,
                                                  PACKAGE = "XBRL"
    ))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Labels.\n")
      }
      return(TRUE)
    }
    FALSE
  }
  processPresentations <- function(doc) {
    pre.length <- length(self$presentation)
    self$presentation <<- rbind(self$presentation, ans <- .Call("xbrlProcessArcs",
                                                                doc, "presentation",
                                                                PACKAGE = "XBRL"
    ))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Presentations.\n")
      }
      return(TRUE)
    }
    FALSE
  }
  processDefinitions <- function(doc) {
    pre.length <- length(self$definition)
    self$definition <<- rbind(self$definition, ans <- .Call("xbrlProcessArcs",
                                                            doc, "definition",
                                                            PACKAGE = "XBRL"
    ))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Definitions.\n")
      }
      return(TRUE)
    }
    FALSE
  }
  processCalculations <- function(doc) {
    pre.length <- length(self$calculation)
    self$calculation <<- rbind(self$calculation, ans <- .Call("xbrlProcessArcs",
                                                              doc, "calculation",
                                                              PACKAGE = "XBRL"
    ))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Calculations.\n")
      }
      return(TRUE)
    }
    FALSE
  }
  processContexts <- function() {
    if (verbose) {
      cat("Contexts\n")
    }
    self$context <<- .Call("xbrlProcessContexts", doc.inst,
                           PACKAGE = "XBRL"
    )
  }
  processFacts <- function() {
    if (verbose) {
      cat("Facts\n")
    }
    self$fact <<- .Call("xbrlProcessFacts", doc.inst, PACKAGE = "XBRL")
  }
  processUnits <- function() {
    if (verbose) {
      cat("Units\n")
    }
    self$unit <<- .Call("xbrlProcessUnits", doc.inst, PACKAGE = "XBRL")
  }
  processFootnotes <- function() {
    if (verbose) {
      cat("Footnotes\n")
    }
    self$footnote <<- .Call("xbrlProcessFootnotes", doc.inst,
                            PACKAGE = "XBRL"
    )
  }
  closeInstance <- function() {
    .Call("xbrlFree", doc.inst, PACKAGE = "XBRL")
    doc.inst <<- NULL
  }
  getResults <- function() {
    self
  }
  list(
    setVerbose = setVerbose, setCacheDir = setCacheDir,
    openInstance = openInstance, deleteCachedInstance = deleteCachedInstance,
    getSchemaName = getSchemaName, processSchema = processSchema,
    processContexts = processContexts, processFacts = processFacts,
    processUnits = processUnits, processFootnotes = processFootnotes,
    closeInstance = closeInstance, getResults = getResults
  )
}

# file.inst <- file.path(.dir_tmp, inst_file_)
# cache.dir <- .dir_tmp
# prefix.out = NULL
# verbose = TRUE
# delete.cached.inst = FALSE

own_xbrlDoAll <- function(file.inst, cache.dir = "xbrl.Cache", prefix.out = NULL,
                          verbose = FALSE, delete.cached.inst = TRUE) {
  xbrl <- own_XBRL()
  xbrl$setVerbose(verbose)
  if (!is.null(cache.dir)) {
    xbrl$setCacheDir(cache.dir)
  }
  xbrl$openInstance(file.inst)
  file_schema <- suppressWarnings(xbrl$getSchemaName())[1]
  xbrl$processSchema(file_schema)
  xbrl$processContexts()
  xbrl$processFacts()
  xbrl$processUnits()
  xbrl$processFootnotes()
  xbrl$closeInstance()
  if (delete.cached.inst && gsub("^(http|https|ftp)://.*$", "\\1", file.inst) %in% c("http", "https", "ftp")) {
    xbrl$deleteCachedInstance()
  }
  xbrl.vars <- xbrl$getResults()
  if (!is.null(prefix.out)) {
    if (verbose) {
      cat("Saving data\n")
    }
    write.csv(xbrl.vars$role, file = paste0(prefix.out, "_roles.csv"))
    write.csv(xbrl.vars$element, file = paste0(prefix.out, "_elements.csv"))
    write.csv(xbrl.vars$label, file = paste0(prefix.out, "_labels.csv"))
    write.csv(xbrl.vars$presentation, file = paste0(prefix.out, "_presentations.csv"))
    write.csv(xbrl.vars$definition, file = paste0(prefix.out, "_definitions.csv"))
    write.csv(xbrl.vars$calculation, file = paste0(prefix.out, "_calculations.csv"))
    write.csv(xbrl.vars$context, file = paste0(prefix.out, "_contexts.csv"))
    write.csv(xbrl.vars$fact, file = paste0(prefix.out, "_facts.csv"))
    write.csv(xbrl.vars$unit, file = paste0(prefix.out, "_units.csv"))
    write.csv(xbrl.vars$footnote, file = paste0(prefix.out, "_footnotes.csv"))
  }
  invisible(xbrl.vars)
}


# .zip <- tab_files_sec$path[i]
# .dir_tmp <- .dir_tmp
# .dir_out <- .dir_elements_raw
xbrl_extract_elements <- function(.zip, .dir_tmp, .dir_out) {
  zip::unzip(.zip, exdir = .dir_tmp)
  
  
  files_ <- zip_list(.zip)[["filename"]]
  inst_file_ <- files_[endsWith(files_, ".xml")]
  inst_file_ <- inst_file_[!grepl("_lab|_cal|_pre|_def", inst_file_)]
  
  if (length(inst_file_) != 1) {
    return(NULL)
  }
  
  elements_ <- try(own_xbrlDoAll(file.path(.dir_tmp, inst_file_), .dir_tmp, delete.cached.inst = FALSE), silent = TRUE)
  invisible(file.remove(file.path(.dir_tmp, files_)))
  
  if (inherits(elements_, "try-error")) {
    NULL
  } else {
    write_rds(elements_, file.path(.dir_out, gsub("zip$", "rds", basename(.zip))), compress = "gz")
  }
}

# .inst <- read_rds(tab_files_elements_use$path[1])
xbrl_process_elements <- function(.inst) {
  chr_desc_ <- .inst$role[["description"]]
  chr_type_ <- .inst$role[["type"]]
  
  if (length(chr_type_) == 0 | length(chr_desc_) == 0) {
    return(NULL)
  }
  
  names(chr_desc_) <- names(chr_type_) <- paste0(chr_type_, "_", chr_desc_)
  
  get_statements <- function(.type, .desc) {
    tab_role_ <- dplyr::filter(.inst$role, description == .desc)
    
    
    tab_skeleton_ <- .inst$presentation %>%
      dplyr::filter(roleId == tab_role_$roleId) %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      tibble::as_tibble()
    
    tab_element_ <- tab_skeleton_ %>%
      dplyr::left_join(.inst$label, by = c("toElementId" = "elementId")) %>%
      dplyr::left_join(.inst$fact, by = c("toElementId" = "elementId")) %>%
      dplyr::left_join(.inst$context, by = c("contextId")) %>%
      dplyr::filter(labelRole == "http://www.xbrl.org/2003/role/label") %>%
      dplyr::filter(!is.na(fact)) %>%
      dplyr::arrange(row_id, startDate, endDate) %>%
      tibble::as_tibble()
    
    if (.type == "Statement") {
      tab_element_ %>%
        dplyr::select(
          tag = toElementId, line_item = labelString,
          unit = unitId, amount = fact, contextId, startDate, endDate
        )
    } else if (.type %in% c("Disclosure", "Document", "Schedule")) {
      tab_element_ %>%
        dplyr::mutate(fact = remove_html_tags(fact)) %>%
        dplyr::select(
          tag = toElementId, line_item = labelString,
          text = fact, contextId, startDate, endDate
        )
    } else {
      stop(paste0("Type not found: ", .type), call. = FALSE)
    }
  }
  
  test <- purrr::map2(chr_type_, chr_desc_, get_statements)
}


# .path <- files$path[1]
own_get_fs <- function(.path, .dir_cache) {
  
  inst_ <- read_rds(.path)
  
  lst_fs <- list(
    is = c(
      "CONSOLIDATED STATEMENTS OF INCOME",
      "CONSOLIDATED STATEMENT OF INCOME", "CONSOLIDATED STATEMENTS OF OPERATIONS",
      "CONSOLIDATED STATEMENT OF OPERATIONS", "CONSOLIDATED STATEMENT OF EARNINGS",
      "CONSOLIDATED STATEMENTS OF EARNINGS", "INCOME STATEMENTS",
      "CONSOLIDATED RESULTS OF OPERATIONS", "STATEMENT OF INCOME"
    ),
    bs = c(
      "CONSOLIDATED BALANCE SHEET",
      "CONSOLIDATED BALANCE SHEETS", "CONSOLIDATED STATEMENT OF FINANCIAL POSITION",
      "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION", "BALANCE SHEETS",
      "CONSOLIDATED FINANCIAL POSITION"
    ),
    cf = c(
      "CONSOLIDATED STATEMENT OF CASH FLOWS",
      "CONSOLIDATED STATEMENTS OF CASH FLOWS", "CASH FLOWS STATEMENTS",
      "CONSOLIDATED STATEMENT OF CASH FLOW"
    )
  )
  
  purrr::map(lst_fs, ~ own_GetFInancial(inst_, .x))
}
