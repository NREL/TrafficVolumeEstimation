library(httr)
set_config(config(ssl_verifypeer = 0L))

# Read TomTom download urls
url_zip = readRDS('url_zip.rds')

# Define TomTom download function
download_zip = function(url, name) {
  gz_file = paste0('./data/', name, '.gz')
  download.file(url = url, destfile = gz_file)
  utils::untar(gz_file, exdir = paste0('./data/', name))
  system(paste('rm', gz_file))
}

# Create a folder to store downloaded data
if(!file.exists('./data/')) {
  dir.create('./data/')
}

# Download data
success = 0
fail = 0
for(i in 1:nrow(url_zip)) {
  try(download_zip(as.character(url_zip$url[i]), as.character(url_zip$name[i])))
  if(file.exists(paste0('./data/', as.character(url_zip$name[i])))) {
    success = success + 1
    cat(paste0("TomTom data ", as.character(url_zip$name[i]), " is downloaded.\n\n"))
  } else {
    fail = fail + 1
    cat(paste0("TomTom data ", as.character(url_zip$name[i]), " download is failed.\n\n"))
  }
  Sys.sleep(1)
}

# Log TomTom data download process
data_log = readRDS('data_log.rds')
data_log$SuccessfulTomTomDownloads = success
data_log$FailedTomTomDownloads = fail
saveRDS(data_log, 'data_log.rds')
cat(paste0('\nTotal number of successful download: ', success, ' \n'))
cat(paste0('Total number of failed download: ', fail, ' \n'))
