library(httr)
library(lubridate)
source('DefaultValues.R')
set_config(config(ssl_verifypeer = 0L))

tomtom_query = function(left_lon, right_lon, top_lat, down_lat, from_date, to_date, week_day){

  month = month(as.Date(from_date))
  week_day = toupper(substring(week_day, 1, 3))
  
  timesets = paste0(
    '{
    "name":"', week_day, '",
    "timeGroups":[
      {
        "days":[
          "', week_day, '" 
          ],
        "times":[
          "0:00-23:00"
          ]
      }
      ]
  }')
  
  body = paste0('{
    "jobName":"', month, '_', week_day, '",
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
      "frcs": [',
        paste0('"', lower_frc:upper_frc, '"', collapse = ','), 
        '],
      "probeSource":"ALL"
    },
    "dateRange": {
      "name":"', month, '",
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
  return(list(month=month, day=week_day, Id=jobId))
}

args = commandArgs(trailingOnly=TRUE)
query_from_date = args[1] # Date format: "yyyy-mm-dd"
query_to_date = args[2] # Date format: "yyyy-mm-dd"
query_week_day = args[3]

if(!file.exists('job')) {
  dir.create('./job/')
}

job = tomtom_query(left, right, top, down, query_from_date, query_to_date, query_week_day)
if(is.na(job$Id)) {
  cat(paste0('TomTom data query for ', month(as.Date(query_from_date)), "_", query_week_day, " is failed!\n"))
} else {
  saveRDS(job, paste0('./job/', month(as.Date(query_from_date)), "_", query_week_day, '_job.rds'))
  cat(paste0('TomTom data query for ', month(as.Date(query_from_date)), "_", query_week_day, " is successful!\n"))
}





