library(sqldf)
library(plyr)
library(leaflet)
library(htmlwidgets)
source('DefaultValues.R')

# Read data
final_data = readRDS('final_data.rds')
final_data = subset(final_data, !StationId %in% removed_stations)
stations = sqldf('select "StationId", "Long", "Lat", "FRC" from final_data group by "StationId"')
data_log = readRDS('data_log.rds')

# Create training and test datasets based on the input values in DefaultValues.R
if(validation_only == T) {
  train_stations = stations
  if(leave_one_out) {
    train_stations$fold = 1:nrow(train_stations)
  } else {
    train_stations$fold = sample(rep(1:nfold, length = nrow(train_stations)))
  }
  
  # hourly data
  final_train_data = sqldf(paste0('select final_data.*, train_stations.fold 
                    from final_data left join train_stations 
                    on final_data."StationId" = train_stations."StationId"'))
  write.table(final_train_data, file = "final_train_data.csv", row.names = FALSE, sep=",")
  
  # ADT data
  daily_data = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, sum(Precip) as Precip, sum(Snow) as Snow, StationId, Date, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek, 
      avg(AvgSp) as AvgSp, sum(ProbeCount) as ProbeCount, sum(Volume) as Volume, fold from final_train_data group by StationId, Date, Dir')
  month_day_adt = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, avg(Precip) as Precip, avg(Snow) as Snow, StationId, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek,
      avg(AvgSp) as AvgSp, avg(ProbeCount) as ProbeCount, avg(Volume) as Volume, fold from daily_data group by StationId, Month, DayOfWeek, Dir')
  write.table(month_day_adt, file = "final_train_data_adt.csv", row.names = F, sep=",")
  
  data_log$TrainFoldFreq = table(train_stations$fold)
  cat('\nTraining data fold frequency: \n')
  print(data_log$TrainFoldFreq)
} else {
  if(test_sample_method == 'random') {
    test_stations = stations[sample(1:nrow(stations), as.integer(nrow(stations)*test_sample_ratio)), ]
  } else if(test_sample_method == 'stratified') {
    test_stations = ddply(stations, .(FRC), function(x){x[sample(1:nrow(x), as.integer(nrow(x)*test_sample_ratio)), ]})
  } else if(test_sample_method == 'one of each') {
    test_stations = ddply(stations, .(FRC), function(x){x[sample(1:nrow(x), 1), ]})
  }
  train_stations = subset(stations, !stations$StationId %in% test_stations$StationId)
  train_stations$fold = sample(rep(1:nfold, length = nrow(train_stations)))
  
  # hourly training and test data
  final_test_data = subset(final_data, final_data$StationId %in% test_stations$StationId)
  final_train_data = subset(final_data, !final_data$StationId %in% test_stations$StationId)
  final_train_data = sqldf(paste0('select final_train_data.*, train_stations.fold 
                    from final_train_data left join train_stations 
                    on final_train_data."StationId" = train_stations."StationId"'))
  write.table(final_train_data, file = "final_train_data.csv", row.names = FALSE, sep=",")
  write.table(final_test_data, file = "final_test_data.csv", row.names = FALSE, sep=",")
  
  # ADT training data
  daily_data_train = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, sum(Precip) as Precip, sum(Snow) as Snow, StationId, Date, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek, 
      avg(AvgSp) as AvgSp, sum(ProbeCount) as ProbeCount, sum(Volume) as Volume, fold from final_train_data group by StationId, Date, Dir')
  month_day_adt_train = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, avg(Precip) as Precip, avg(Snow) as Snow, StationId, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek,
      avg(AvgSp) as AvgSp, avg(ProbeCount) as ProbeCount, avg(Volume) as Volume, fold from daily_data_train group by StationId, Month, DayOfWeek, Dir')
  write.table(month_day_adt_train, file = "final_train_data_adt.csv", row.names = F, sep=",")
  
  # ADT test data
  daily_data_test = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, sum(Precip) as Precip, sum(Snow) as Snow, StationId, Date, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek, 
      avg(AvgSp) as AvgSp, sum(ProbeCount) as ProbeCount, sum(Volume) as Volume, fold from final_test_data group by StationId, Date, Dir')
  month_day_adt_test = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, avg(Precip) as Precip, avg(Snow) as Snow, StationId, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek,
      avg(AvgSp) as AvgSp, avg(ProbeCount) as ProbeCount, avg(Volume) as Volume, fold from daily_data_test group by StationId, Month, DayOfWeek, Dir')
  write.table(month_day_adt_test, file = "final_test_data_adt.csv", row.names = F, sep=",")
  
  data_log$TrainFoldFreq = table(train_stations$fold)
  cat('\nTraining data fold frequency: \n')
  print(data_log$TrainFoldFreq)
  data_log$TestDataFRCFreq = table(test_stations$FRC)
  cat('\nTest data FRC freqency: \n')
  print(data_log$TestDataFRCFreq)
  
  pal = colorFactor(c("#377EB8", "#E41A1C"), c("Train", "Test"), ordered = T)
  m1=leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addCircleMarkers(data = train_stations, ~Long, ~Lat, radius = station_radius, stroke= F, fillOpacity=1, fillColor = "#377EB8") %>%
    addCircleMarkers(data = test_stations, ~Long, ~Lat, radius = station_radius, stroke= F, fillOpacity=1, fillColor = "#E41A1C") %>%
    addLegend(position = "bottomright",
              pal = pal, values = c("Train", "Test"))
  saveWidget(m1, file = "train_test.html")
} 
saveRDS(data_log, 'data_log.rds')


