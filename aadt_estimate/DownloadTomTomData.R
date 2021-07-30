library(httr)
source('DefaultValues.R')
set_config(config(ssl_verifypeer = 0L))

args = commandArgs(trailingOnly=TRUE)
query_month = args[1] 
query_week_day = args[2]
job = readRDS(paste0('./job/', query_month, "_", query_week_day, '_job.rds'))
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

download_zip(url_zip, paste0(job$month, '_', job$day))
if(file.exists(as.character(paste0(job$month, '_', job$day)))) {
  cat(paste0('TomTom data download for ', job$month, '_', job$day, " is successful!\n"))
} 

