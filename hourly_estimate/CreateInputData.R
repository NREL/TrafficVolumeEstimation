library(randomForest)
library(plyr)
library(sqldf)
library(lubridate)
library(doParallel)
library(ggplot2)
library(reshape2)
library(httr)
library(jsonlite)
library(zoo)
library(AtmRay)
library(data.table)
library(htmlwidgets)
library(mapview)
library(rgdal)
library(foreign)
registerDoParallel(cores=8) 
set_config(config(ssl_verifypeer = 0L))
source('DefaultValues.R')

args = commandArgs(trailingOnly=TRUE)
query_date = args[1] # Date format: "yyyy-mm-dd"
week_day = weekdays(as.Date(query_date))
week_dict = 1:7
names(week_dict) = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

get_weather = function(lat, long, startdate, enddate){
  r = GET(
    "http://cleanedobservations.wsi.com/CleanedObs.svc/GetObs?",
    query = list(version = 2, lat=lat, long=long, startDate=startdate, endDate=enddate, interval='hourly', time='lwt',
                 units='imperial', format='json', fields='surfaceTemperatureFahrenheit,precipitationPreviousHourInches,snowfallInches,windSpeedMph', 
                 delivery='stream', userKey=weather_api_key)
  )
  weather = fromJSON(content(r, "text", encoding = "UTF-8"))$weatherData$hourly$hours
  return(weather)
}

# Read TomTom network
cat('\nReading network data...\n')
network = readOGR(dsn = paste0('./', query_date), layer = 'network', stringsAsFactors = F)
network_df = network@data
coords = foreach(i = 1:length(network), .combine = rbind) %dopar% {
  network@lines[[i]]@Lines[[1]]@coords[1,]
}
network_df$LONGITUDE = coords[, 1]
network_df$LATITUDE = coords[, 2]
saveRDS(network, paste0(query_date, '/network.rds'))

# Obtain daily data
cat('\nProcessing TomTom data...\n')
filenames = list.files(paste0('./', query_date, '/'))
daily_data = data.frame(Id=integer(),
                        AvgSp=numeric(), 
                        count=integer(),
                        HOUR=integer())
for(file in filenames){
  if(grepl(query_date, file)){
    data = read.dbf(paste0("./", query_date, "/", file))
    data = data[, c(1,5,9)]
    names(data)= c("Id", "AvgSp", "count")
    data$HOUR = as.integer(unlist(strsplit(file, "\\-|\\_"))[4])
    daily_data = rbind(daily_data, data)
  }
}

id_hour = meshgrid(network$Id, 0:23)
dim(id_hour$x) = c(length(network$Id)*24, 1)
dim(id_hour$y) = c(length(network$Id)*24, 1)
id_hour = data.frame(Id = id_hour$x, HOUR = id_hour$y)

daily_data = sqldf('select id_hour.*, daily_data."AvgSp", daily_data.count
                    from id_hour left join daily_data
                    on daily_data."Id" = id_hour."Id" and daily_data."HOUR" = id_hour."HOUR"')

daily_data = sqldf('select daily_data.*, network_df."SpeedLimit" as speed_limit, network_df."FRC" as frc, network_df."LONGITUDE", network_df."LATITUDE" 
                   from daily_data left join network_df
                   on daily_data."Id" = network_df."Id"')
daily_data$DayOfWeek = week_dict[week_day]

# Join weather info with volume data
cat('\nQuerying weather data...\n')
if(!file.exists(paste0(query_date, '/weather.rds'))) {
  weather = get_weather((top+down)/2, (left+right)/2, format(as.Date(query_date), "%m/%d/%Y"), format(as.Date(query_date)+1, "%m/%d/%Y"))
  weather$dateHrLwt = strptime(weather$dateHrLwt, '%m/%d/%Y %H:%M:%S', tz = time_zone)
  weather$DATE = date(weather$dateHrLwt)
  weather$HOUR = hour(weather$dateHrLwt)
  weather$dateHrLwt = NULL
  weather = weather[!duplicated(weather[, c("DATE", "HOUR")]), ]
  weather$surfaceTemperatureFahrenheit = na.locf(weather$surfaceTemperatureFahrenheit)
  weather$precipitationPreviousHourInches = na.locf(weather$precipitationPreviousHourInches)
  weather$windSpeedMph = na.locf(weather$windSpeedMph)
  weather$snowfallInches = na.locf(weather$snowfallInches)
  saveRDS(weather, paste0(query_date, '/weather.rds'))
} else {
  weather = readRDS(paste0(query_date, '/weather.rds'))
}

cat('\nJoining weather data...\n')
daily_data = sqldf('select weather."surfaceTemperatureFahrenheit" as temp, weather."windSpeedMph" as wind, weather."precipitationPreviousHourInches" as precip, weather."snowfallInches" as snow, daily_data.*  
                      from daily_data left join weather 
                      on daily_data."HOUR" = weather."HOUR"')

# Output daily data
daily_data$count[is.na(daily_data$count)] = 0
daily_data$AvgSp[daily_data$count==0] = daily_data$speed_limit[daily_data$count==0]
daily_data = daily_data[, c('Id', 'temp', 'wind', 'precip', 'snow', 'LONGITUDE', 'LATITUDE',
                            'speed_limit', 'frc', 'DayOfWeek', 'HOUR', 'AvgSp', 'count')]
names(daily_data) = c('Id', 'Temp', 'WindSp', 'Precip', 'Snow', 'Long', 'Lat',
                      'SpeedLimit', 'FRC', 'DayOfWeek', 'Hour', 'AvgSp', 'ProbeCount')
write.table(daily_data, file = paste0(query_date, "/daily_data.csv"), row.names = FALSE, sep=",")
cat('\nInput data is created!\n')

