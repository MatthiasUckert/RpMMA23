source("1_code/functions/f-all.R")

if (!dir.exists("0_data")) dir.create("0_data")

if (!file.exists("0_data/data.zip")) {
  download_google_drive(
    .id = "1O8RRxSz9MFvOUR2udvI46wVJtzjWKRGl",
    .dir = "0_data/"
  )
  
  zip::unzip("0_data/data.zip", exdir = "0_data")
  
  message("data succesfully downloaded and extracted")
}