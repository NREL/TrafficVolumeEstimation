library(sqldf)
library(leaflet)
library(htmlwidgets)
source('DefaultValues.R')

# Read data
volume_24hr = read.csv(volume_filename, stringsAsFactors = F)
VolumeTableStationIds = sort(unique(volume_24hr[[station_id_colname2]]))
VolumeTableRowSum = nrow(volume_24hr)
VolumeTableNARowSum = sum(apply(volume_24hr, anyNA, MARGIN = 1))
names(volume_24hr)[hour_colnum] = 0:23
volume_24hr[[date_colname]] = as.Date(volume_24hr[[date_colname]], date_format)

stations = read.csv(station_filename, stringsAsFactors = F)
StationTableStationIds = sort(unique(stations[[station_id_colname]]))
StationTableRowSum = nrow(stations)
StationTableNARowSum = sum(apply(stations, anyNA, MARGIN = 1))
stations = sqldf(paste0('select "', station_id_colname, '", "', long_colname, '", "', lat_colname, '", "', num_lane_colname, '", "', fc_colname, '" 
                        from stations group by "', station_id_colname, '"'))

# Remove volume duplicates
VolumeDuplicates = duplicated(volume_24hr)
volume_24hr = volume_24hr[!VolumeDuplicates, ]
stn_date_dir = volume_24hr[, c(station_id_colname2, dir_colname, date_colname)]
StnDateDirDuplicates = duplicated(stn_date_dir) | duplicated(stn_date_dir[nrow(stn_date_dir):1, ])[nrow(stn_date_dir):1]
volume_24hr = volume_24hr[!StnDateDirDuplicates, ]

# Merge stations with volume
volume_24hr = subset(volume_24hr, volume_24hr[[date_colname]]>=as.Date(date_min)&volume_24hr[[date_colname]]<=as.Date(date_max))
volume_24hr = sqldf(paste0('select volume_24hr.*, stations."', long_colname, '", stations."', lat_colname, '", stations."', num_lane_colname, '", stations."', fc_colname, '"
                    from volume_24hr left join stations
                    on volume_24hr."', station_id_colname2, '" = stations."', station_id_colname, '"'))
volume_24hr = subset(volume_24hr, (!is.na(volume_24hr[[long_colname]]))&(!is.na(volume_24hr[[lat_colname]])))
names(volume_24hr)[names(volume_24hr)==station_id_colname2] = station_id_colname
saveRDS(volume_24hr, 'volume_24hr.rds')
stations = sqldf(paste0('select "', station_id_colname, '", "', long_colname, '", "', lat_colname, '", "', num_lane_colname, '", "', fc_colname, '" 
                        from volume_24hr group by "', station_id_colname, '"'))

# Log data summary
data_log = list()
data_log$VolumeTableStationIds = VolumeTableStationIds
data_log$VolumeTableRowSum = VolumeTableRowSum
data_log$VolumeTableNARowSum = VolumeTableNARowSum
data_log$StationTableStationIds = StationTableStationIds
data_log$StationTableRowSum = StationTableRowSum
data_log$StationTableNARowSum = StationTableNARowSum
data_log$MinDate = min(volume_24hr[[date_colname]])
data_log$MaxDate = max(volume_24hr[[date_colname]])
data_log$VolumeDuplicatesSum = sum(VolumeDuplicates)
data_log$StnDateDirDuplicatesSum = sum(StnDateDirDuplicates)
data_log$JoinedVolumeTableRowSum = nrow(volume_24hr)
data_log$JoinedVolumeTableNARowSum = sum(apply(volume_24hr, anyNA, MARGIN = 1))
data_log$JoinedVolumeStaionIDs = stations[[station_id_colname]]
data_log$JoinedVolumeStationFCFreq = table(stations[[fc_colname]])
data_log$JoinedVolumeStationLaneCountFreq = table(stations[[num_lane_colname]])
data_log$JoinedVolumeDirFreq = table(volume_24hr[[dir_colname]])
saveRDS(data_log, 'data_log.rds')
print(data_log)

# Visualize stations
m1 = leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircleMarkers(data = stations, ~stations[[long_colname]], ~stations[[lat_colname]], radius = station_radius, 
                   stroke= F, fillOpacity=1, fillColor = "#377EB8", 
                   popup = ~paste0("Station ID: ", as.character(stations[[station_id_colname]]), "<br/>", 
                                   "FC: ", as.character(stations[[fc_colname]]), "<br/>",
                                   "Number of Lanes: ", as.character(stations[[num_lane_colname]]), "<br/>"))
saveWidget(m1, file = "stations.html")

