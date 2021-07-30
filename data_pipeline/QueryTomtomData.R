library(httr)
library(lubridate)
library(reshape2)
library(sqldf)
set_config(config(ssl_verifypeer = 0L))
source('DefaultValues.R')

# Define TomTom query function (Day of week: "FRI", "SAT", "SUN", "MON", "TUE", "WED", "THU")
tomtom_query = function(lat, lon, date, week_day, stationid){
  left_lon = lon-0.002
  right_lon = lon+0.002
  top_lat = lat+0.002
  down_lat = lat-0.002
  
  from_date = date-3
  to_date = date+3
  
  job = paste(stationid, date, sep = '_')
  
  timesets = paste0(
  '{
    "name":"0-1",
    "timeGroups":[
      {
        "days":[
          "', week_day, '" 
          ],
        "times":[
          "0:00-1:00"
          ]
      }
      ]
  }')
  for(i in 1:23){
    temp = paste0(
      '{
        "name":"', i, '-', i+1, '",
        "timeGroups":[
          {
            "days":[
              "', week_day, '" 
              ],
            "times":[
              "', i, ':00-', i+1, ':00', '"
              ]
          }
          ]
      }')
    timesets = paste(timesets, temp, sep = ",\n")
  }
  
  body = paste0('{
    "jobName":"', job, '",
    "distanceUnit":"MILES",
    "mapVersion":"', tomtom_map_version, '",
    "network": {
      "name": "co",
      "boundingBox" : {
        "leftDownCorner": {
          "longitude":', left_lon, ',
          "latitude":', down_lat, '
        },
        "rightTopCorner": {
          "longitude":', right_lon, ',
          "latitude":', top_lat, '
        }
      },
      "timeZoneId": "', time_zone, '",
      "frcs": [
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8"
        ],
      "probeSource":"ALL"
    },
    "dateRange": {
      "name":"', job, '",
      "from":"', from_date, '",
      "to":"', to_date, '"
    },
    "timeSets":[',
      timesets,
      ']
  }')
  
  p = POST(
    paste0("https://api.tomtom.com/traffic/trafficstats/caa/1?key=", tomtom_api_key),
    body = body,
    encode = "raw",
    content_type_json()
  )
  jobId = content(p, "parse")$jobId
  if(is.null(jobId)) {
    jobId = NA
  }
  return(data.frame(name=job, date=as.character(date), Id=jobId))
}

# Read data and create a table with a unique station and date combination in each row
volume_24hr = readRDS('volume_24hr.rds')
stn_date = sqldf(paste0('select "', station_id_colname, '", "', date_colname, '", "', long_colname, '", "', lat_colname, 
                        '" from volume_24hr group by "', station_id_colname, '", "', date_colname, '"'))
stn_date$DAYOFWEEK = weekdays(stn_date[[date_colname]])
stn_date$DAYOFWEEK = toupper(substring(stn_date$DAYOFWEEK, 1, 3))

# Check if query need to starts with where it left
if(file.exists('jobs.rds')) {
  jobs = readRDS('jobs.rds')
} else {
  jobs = data.frame(name = character(), date = character(), Id = character())
}
if(file.exists('url_zip.rds')) {
  url_zip = readRDS('url_zip.rds')
} else {
  url_zip = data.frame(name = character(), url = character())
}
start = min(nrow(jobs), nrow(url_zip))

# Query TomTom by submitting 50 jobs as an batch
cat(paste0('\nTotal number of queries: ', nrow(stn_date), ' \n'))
for(k in seq(start+1, nrow(stn_date), 50)) {
  
  if(nrow(jobs) == nrow(url_zip)) {
    for(i in k:min(k+49, nrow(stn_date))) {
      results = tomtom_query(stn_date[[lat_colname]][i],
                             stn_date[[long_colname]][i],
                             stn_date[[date_colname]][i],
                             stn_date[['DAYOFWEEK']][i],
                             stn_date[[station_id_colname]][i])
      jobs = rbind(jobs, results)
      if(is.na(results$Id)) {
        print(paste(i, "has a error.\n"))
      } else
        cat(paste(i, "is posted.\n"))
      Sys.sleep(15)
    }
    saveRDS(jobs, 'jobs.rds')
    Sys.sleep(900)
  }
  
  for(i in k:min(k+49, nrow(stn_date))) {
    if(is.na(jobs$Id[i])) {
      download_url = NA
    } else {
      r = GET(paste0("https://api.tomtom.com/traffic/trafficstats/status/1/", jobs$Id[i], "?key=", tomtom_api_key))
      download_url = content(r, "parse")$urls[[3]]
      attempt = 1
      while(is.null(download_url)) {
        attempt = attempt + 1
        Sys.sleep(300)
        r = GET(paste0("https://api.tomtom.com/traffic/trafficstats/status/1/", jobs$Id[i], "?key=", tomtom_api_key))
        download_url = content(r, "parse")$urls[[3]]
        if(attempt >= 20) break
      }
    }
    if(is.null(download_url)) {
      download_url = NA
      url_zip = rbind(url_zip, data.frame(name = jobs$name[i],  url = download_url))
      cat(paste0("Fail to retrieve download url for ", i, ".\n"))
    } else {
      url_zip = rbind(url_zip, data.frame(name = jobs$name[i],  url = download_url))
      cat(paste0("Download url is available for ", i, ".\n"))
    }
  }
  saveRDS(url_zip, 'url_zip.rds')
}

# Log TomTom data query process
data_log = readRDS('data_log.rds')
data_log$SuccessfulTomTomQueries = sum(!is.na(url_zip$url))
data_log$FailedTomTomQueries = sum(is.na(url_zip$url))
saveRDS(data_log, 'data_log.rds')
cat(paste0('\nTotal number of successful queries: ', data_log$SuccessfulTomTomQueries, ' \n'))
cat(paste0('Total number of failed queries: ', data_log$FailedTomTomQueries, ' \n'))



