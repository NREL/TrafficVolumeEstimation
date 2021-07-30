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
set_config(config(ssl_verifypeer = 0L))
source('DefaultValues.R')

# Read volume, TomTom data, and station data and create a table with a unique station, direction, and station combination in each row
volume_24hr = readRDS('volume_24hr.rds')
tomtom_data = readRDS('tomtom_data.rds')
tomtom_data$Date = as.Date(tomtom_data$Date)
stn_dir = readRDS('stn_dir.rds')
stations = sqldf(paste0('select "', station_id_colname, '", "', long_colname, '", "', lat_colname,  
                        '" from stn_dir group by "', station_id_colname, '"'))
stn_date = sqldf(paste0('select "', station_id_colname, '", "', long_colname, '", "', lat_colname, '", "', date_colname, 
                        '" from volume_24hr group by "', station_id_colname, '", "', date_colname, '"'))
StnDirStationIDs = stations[[station_id_colname]]
StnDateRowSum = nrow(stn_date)
cat(paste0('\nTotal number of unique station-date combinations: ', StnDateRowSum, ' \n'))

# Reformat volume data
hourly_volume = melt(volume_24hr, id=names(volume_24hr)[-hour_colnum], variable.name="HOUR", value.name="VOLUME")
hourly_volume$HOUR = as.integer(as.character(hourly_volume$HOUR))
cat(paste0('\nTotal number of rows in hourly volume data: ', nrow(hourly_volume), ' \n'))
cat(paste0('Total number of rows with NAs in hourly volume data: ', sum(apply(hourly_volume, anyNA, MARGIN = 1)), ' \n'))
cat(paste0('Total number of rows in tomtom data: ', nrow(tomtom_data), ' \n'))

# Query weather data
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
cat('\nQuerying weather data...\n')
if(!file.exists('weather_df.rds')) {
  if(weather_batch_query) {
    weather_df = foreach(i = 1:nrow(stations), .combine = rbind) %do% {
      stn_date_subset = subset(stn_date, stn_date[[station_id_colname]]==stations[[station_id_colname]][i])
      date_min = format(min(stn_date_subset[[date_colname]]), format = '%m/%d/%Y')
      date_max = format(max(stn_date_subset[[date_colname]])+1, format = '%m/%d/%Y')
      weather = get_weather(stations[[lat_colname]][i], stations[[long_colname]][i], date_min, date_max)
      weather$STATIONID = stations[[station_id_colname]][i]
      weather
    }
  } else {
    weather_df = foreach(i = 1:nrow(stn_date), .combine = rbind) %do% {
      date_min = format(stn_date[[date_colname]][i], format = '%m/%d/%Y')
      date_max = format(stn_date[[date_colname]][i]+1, format = '%m/%d/%Y')
      weather = get_weather(stn_date[[lat_colname]][i], stn_date[[long_colname]][i], date_min, date_max)
      weather$STATIONID = stn_date[[station_id_colname]][i]
      weather
    }
  }
  weather_df$dateHrLwt = strptime(weather_df$dateHrLwt, '%m/%d/%Y %H:%M:%S', tz = time_zone)
  weather_df$DATE = date(weather_df$dateHrLwt)
  weather_df$HOUR = hour(weather_df$dateHrLwt)
  weather_df$dateHrLwt = NULL
  weather_df = weather_df[!duplicated(weather_df[, c("DATE", "HOUR", "STATIONID")]), ]
  weather_df$surfaceTemperatureFahrenheit = na.locf(weather_df$surfaceTemperatureFahrenheit)
  weather_df$precipitationPreviousHourInches = na.locf(weather_df$precipitationPreviousHourInches)
  weather_df$windSpeedMph = na.locf(weather_df$windSpeedMph)
  weather_df$snowfallInches = na.locf(weather_df$snowfallInches)
  saveRDS(weather_df, 'weather_df.rds')
} else {
  weather_df = readRDS('weather_df.rds')
}
cat('Weather data is successfully queried.\n')
WeatherRowSum = nrow(weather_df)
WeatherNARowSum = sum(apply(weather_df, anyNA, MARGIN = 1))
cat(paste0('Total number of rows in weather data: ', WeatherRowSum, ' \n'))
cat(paste0('Total number of rows with NAs in weather data: ', WeatherNARowSum, ' \n'))

# Join weather data with volume data
hourly_volume = sqldf(paste0('select weather_df."surfaceTemperatureFahrenheit" as temp, weather_df."windSpeedMph" as wind, weather_df."precipitationPreviousHourInches" as precip, weather_df."snowfallInches" as snow, hourly_volume.*  
                      from hourly_volume left join weather_df 
                      on hourly_volume."', date_colname, '" = weather_df."DATE" 
                      and hourly_volume."HOUR" = weather_df."HOUR" 
                      and hourly_volume."', station_id_colname, '" = weather_df."STATIONID"'))
cat(paste0('\nTotal number of rows in hourly volume data after joining with weather data: ', nrow(hourly_volume), ' \n'))
cat(paste0('Total number of rows with NAs in hourly volume data after joining with weather data: ', sum(apply(hourly_volume, anyNA, MARGIN = 1)), ' \n'))

# Join tomtom data with volume data
combined_data = sqldf(paste0('select hourly_volume.*, tomtom_data."AvgSp", tomtom_data."count" 
                      from hourly_volume left join tomtom_data 
                      on hourly_volume."', station_id_colname, '" = tomtom_data."STATIONID"
                      and hourly_volume."', dir_colname, '" = tomtom_data."Dir"
                      and hourly_volume."', date_colname, '" = tomtom_data."Date"
                      and hourly_volume."HOUR" = tomtom_data."HOUR"'))
combined_data = sqldf(paste0('select combined_data.*, stn_dir.speed_limit, stn_dir.frc 
                      from combined_data left join stn_dir 
                      on combined_data."', station_id_colname, '" = stn_dir."', station_id_colname, '"
                      and combined_data."', dir_colname, '" = stn_dir."', dir_colname, '"'))

combined_data = combined_data[order(combined_data[[station_id_colname]], combined_data[[date_colname]], combined_data[[dir_colname]], combined_data[['HOUR']]), ]
combined_data$DAYOFWEEK = weekdays(combined_data[[date_colname]])
combined_data$MONTH = month(combined_data[[date_colname]])
combined_data$AvgSp[combined_data$count==0&!is.na(combined_data$count)] = combined_data$speed_limit[combined_data$count==0&!is.na(combined_data$count)]
cat(paste0('\nTotal number of rows in combined data table after joining with tomtom data: ', nrow(combined_data), ' \n'))
cat(paste0('Total number of rows with NAs in combined data table after joining with tomtom data: ', sum(apply(combined_data, anyNA, MARGIN = 1)), ' \n'))


# Produce the final data for modeling
final_data = combined_data[, c("temp", "wind", "precip", "snow", station_id_colname, date_colname, dir_colname, long_colname, lat_colname, num_lane_colname, 
                               fc_colname, "speed_limit", "frc", "DAYOFWEEK", "MONTH", "HOUR", "AvgSp", "count", "VOLUME")]
names(final_data) = c("Temp", "WindSp", "Precip", "Snow", "StationId", "Date", "Dir", "Long", "Lat", "NumberOfLanes", 
                      "FC", "SpeedLimit", "FRC", "DayOfWeek", "Month", "Hour", "AvgSp", "ProbeCount", "Volume")
final_data$Volume = as.integer(final_data$Volume)
final_data$NumberOfLanes = as.numeric(final_data$NumberOfLanes)
final_data$Hour = as.numeric(final_data$Hour)
final_data[final_data$Dir == both_code, 'Volume'] = final_data[final_data$Dir == both_code, 'Volume']/2
final_data = na.omit(final_data)
final_data$PenRate = final_data$ProbeCount/final_data$Volume
final_data = subset(final_data, is.na(PenRate)|PenRate<=1)
FinalDataRowSum = nrow(final_data)
cat(paste0('\nTotal number of rows in final data: ', FinalDataRowSum, ' \n'))
saveRDS(final_data, "final_data.rds")

# Log data join process
data_log = readRDS('data_log.rds')
data_log$StnDirStationIDs = StnDirStationIDs
data_log$StnDateRowSum = StnDateRowSum
data_log$WeatherRowSum = WeatherRowSum
data_log$WeatherNARowSum = WeatherNARowSum
data_log$FinalDataRowSum = FinalDataRowSum
saveRDS(data_log, 'data_log.rds')


