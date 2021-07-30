library(httr)
source('DefaultValues.R')
set_config(config(ssl_verifypeer = 0L))

args = commandArgs(trailingOnly=TRUE)
query_date = args[1] # Date format: "yyyy-mm-dd"
job = readRDS(paste0('./job/', query_date, '_job.rds'))
r = GET(paste0("https://api.tomtom.com/traffic/trafficstats/status/1/", job$Id, "?key=", tomtom_api_key))
url_zip = content(r, "parse")$urls[[3]]

download_zip = function(url, name) {
  if(is.null(url)) {
    cat(paste0(content(r, "text"), "\n"))
  } else {
    gz_file = paste0('./', name, '.gz')
    download.file(url = url, destfile = gz_file)
    utils::untar(gz_file, exdir = paste0('./', name))
    system(paste('rm', gz_file))
  }
}

download_zip(url_zip, job$date)
if(file.exists(as.character(job$date))) {
  cat("TomTom data is downloaded.\n")
} else {
  cat("TomTom data download is failed.\n")
}

