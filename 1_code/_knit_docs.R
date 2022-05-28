files_in  <- list.files("1_code/", "\\.Rmd", full.names = TRUE)
files_in  <- files_in[grepl("00|01|02|03|04|05", basename(files_in))]
dir.create("docs/", FALSE, TRUE)

files_out <- file.path("docs/", basename(files_in)) 
file.copy(files_in, files_out)

rmarkdown::render_site("docs/")
browseURL("docs/index.html")
file.remove(files_out)
# rmarkdown::clean_site("docs/", preview = FALSE)
