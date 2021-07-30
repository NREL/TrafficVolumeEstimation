library(plyr)
library(ggplot2)
library(data.table)
library(sqldf)
library(grid)
library(gtable)
library(doParallel)
library(ggmap)
library(leaflet)
library(rgdal)
library(sp) 
library(rgeos)
library(htmlwidgets)
library(foreign)
library(reshape2)
# options("scipen"=100, "digits"=4)
source('DefaultValues.R')

# Obtain volume data
volume_24hr = readRDS('volume_24hr.rds')

# Define a function to determine TomTom segment direction
find_dir = function(x1, y1, x2, y2) {
  a = x2 - x1
  b = y2 - y1
  if(a>=0&b>0) {
    dir = c(north_code, northeast_code, east_code)
  } else if(a>0&b<=0) {
    dir = c(south_code, southeast_code, east_code)
  } else if(a<=0&b<0) {
    dir = c(south_code, southwest_code, west_code)
  } else if(a<0&b>=0) {
    dir = c(north_code, northwest_code, west_code)
  }
  return(dir)
}

# Define a function that finds the nearest TomTom segment for volume a count station
seg_search = function(lon, lat, net, dir){
  point = SpatialPoints(data.frame(lon = lon, lat = lat))
  proj4string(point) = proj4string(net)
  ids = order(gDistance(point, net, byid=TRUE))
  
  if(dir %in% c(oneway_code, both_code)) {
    seg_id = net$Segment.Id[ids[1]]
  } else {
    seg_id = NA
    j = 1
    while(is.na(seg_id)) {
      coords = network@lines[[ids[j]]]@Lines[[1]]@coords
      d = find_dir(coords[1, 1], coords[1, 2], coords[nrow(coords), 1], coords[nrow(coords), 2])
      if(dir %in% d) seg_id = net$Segment.Id[ids[j]]
      j = j + 1
      if(j > 5) break
    }
  }
  
  return(seg_id)
}

# Create a table with a unique combination of station and traffic direction at each row and match them with the nearest TomTom segment
stn_dir = sqldf(paste0('select "', station_id_colname, '", "', dir_colname, '", "', long_colname, '", "', lat_colname, '", "', date_colname, 
                       '" from volume_24hr group by "', station_id_colname, '", "', dir_colname, '"'))
stn_dir$folder = paste(paste(stn_dir[[station_id_colname]], stn_dir[[date_colname]], sep = '_'))
stn_dir$seg_id = NA
for(i in 1:nrow(stn_dir)){
  dsn = paste0("data/", stn_dir$folder[i])
  if(file.exists(dsn)){
    network = readOGR(dsn = dsn, layer = "network", stringsAsFactors = F)
    network = subset(network, FRC <= 1)
    seg_id = tryCatch(seg_search(stn_dir[[long_colname]][i], 
                                 stn_dir[[lat_colname]][i], 
                                 network, 
                                 stn_dir[[dir_colname]][i]), error=function(err) NA)
    stn_dir$seg_id[i] = seg_id
    cat(paste(i, "is done!\n"))
  }
}
StnDirRowSum = nrow(stn_dir)
StnDirNARowSum = sum(is.na(stn_dir$seg_id))
stn_dir = stn_dir[order(stn_dir[[station_id_colname]], stn_dir[[dir_colname]]), ]
stn_dir = subset(stn_dir, !is.na(seg_id))

# Match TomTom speed limit and FRC with each traffic direction of each count station 
stn_dir$net_id = NA
stn_dir$speed_limit = NA
stn_dir$frc = NA
m2 = leaflet() %>% addProviderTiles(providers$Stamen.TonerLite)
for(i in 1:nrow(stn_dir)) {
  dsn = paste0("data/", stn_dir$folder[i])
  network = readOGR(dsn = dsn, layer = "network", stringsAsFactors = F)
  stn_dir$net_id[i] = which(network$Segment.Id==stn_dir$seg_id[i])
  stn_dir$speed_limit[i] = network$SpeedLimit[stn_dir$net_id[i]]
  stn_dir$frc[i] = network$FRC[stn_dir$net_id[i]]
  m2 = m2 %>%
    addPolylines(data = subset(network, Segment.Id==stn_dir$seg_id[i]), popup = ~as.character(Segment.Id), color = 'red') %>%
    addCircleMarkers(data = stn_dir[i, ], ~stn_dir[i, long_colname], ~stn_dir[i, lat_colname], radius = 3, 
                     popup = ~paste0("Station ID: ", as.character(stn_dir[i, station_id_colname]), "<br/>", 
                                     "FRC: ", as.character(stn_dir[i, 'frc']), "<br/>",
                                     "Speed Limit: ", as.character(stn_dir[i, 'speed_limit']), "<br/>"))
  cat(paste(i, "is done!\n"))
}
saveWidget(m2, file = "stations & segments.html")

# Create a table with a unique station, date, and direction combination in each row
stn_date_dir = volume_24hr[, c(station_id_colname, dir_colname, date_colname)]
stn_date_dir = stn_date_dir[order(stn_date_dir[[station_id_colname]], stn_date_dir[[date_colname]], stn_date_dir[[dir_colname]]), ]
stn_date_dir = sqldf(paste0('select stn_date_dir.*, stn_dir.seg_id, stn_dir.net_id, stn_dir.speed_limit, stn_dir.frc
                         from stn_date_dir left join stn_dir
                         on stn_date_dir."', station_id_colname, '" = stn_dir."', station_id_colname, '"
                         and stn_date_dir."', dir_colname, '" = stn_dir."', dir_colname, '"'))
stn_date_dir$folder = paste(paste(stn_date_dir[[station_id_colname]], stn_date_dir[[date_colname]], sep = '_'))
StnDateDirRowSum = nrow(stn_date_dir)
StnDateDirNARowSum = sum(is.na(stn_date_dir$seg_id))
stn_date_dir = subset(stn_date_dir, !is.na(seg_id))

# Format TomTom probe data and save as a seperate csv file for data collected on each traffic direction of each station on each day
if(!file.exists('./daily_data/')) {
  dir.create('./daily_data/')
}

for(i in 1:nrow(stn_date_dir)){
  if(file.exists(paste0("./data/", stn_date_dir$folder[i]))){
    daily_data = data.frame(Id=integer(), 
                            AvgTt=numeric(), 
                            AvgSp=numeric(), 
                            count=integer(), 
                            STATIONID=integer(),
                            Dir=character(),
                            Date=character(),
                            HOUR=integer())
    for(j in 0:23){
      filename = paste0(stn_date_dir$folder[i], '_0_', j, '-', j+1, '_', j, '.dbf')
      filepath = paste0("./data/", stn_date_dir$folder[i], "/", filename)
      if(file.exists(filepath)){
        data = read.dbf(filepath)
        data = data[, c(tomtom_Id_colnum, tomtom_AvgTt_colnum, tomtom_AvgSp_colnum, tomtom_count_colnum)]
        names(data)= c("Id", "AvgTt", "AvgSp", "count")
        if(stn_date_dir$net_id[i] %in% data$Id) {
          data = subset(data, Id == stn_date_dir$net_id[i])
        } else {
          data = data.frame(Id=stn_date_dir$net_id[i], AvgTt=NA, AvgSp=NA, count=0)
        }
        data$STATIONID = stn_date_dir[[station_id_colname]][i]
        data$Dir = stn_date_dir[[dir_colname]][i]
        data$Date = stn_date_dir[[date_colname]][i]
        data$HOUR = j
      } else {
        data = data.frame(Id=stn_date_dir$net_id[i], 
                          AvgTt=NA, 
                          AvgSp=NA, 
                          count=0, 
                          STATIONID=stn_date_dir[[station_id_colname]][i],
                          Dir=stn_date_dir[[dir_colname]][i],
                          Date=stn_date_dir[[date_colname]][i],
                          HOUR=j)
      }
      daily_data = rbind(daily_data, data)
    }
  } else {
    daily_data = data.frame(Id=NA, 
                            AvgTt=NA, 
                            AvgSp=NA, 
                            count=NA, 
                            STATIONID=stn_date_dir[[station_id_colname]][i],
                            Dir=stn_date_dir[[dir_colname]][i],
                            Date=stn_date_dir[[date_colname]][i],
                            HOUR=0:23)
  }
  write.table(daily_data, file = paste0("./daily_data/", stn_date_dir$folder[i], "_", stn_date_dir[[dir_colname]][i], ".csv"), row.names = FALSE, sep=",", na = "")
  cat(paste(i, "is done!\n"))
}

# Combine all TomTom probe data
daily_data_filenames = list.files("./daily_data/")
tomtom_data=data.frame(Id=integer(), 
                       AvgTt=numeric(), 
                       AvgSp=numeric(), 
                       count=integer(), 
                       STATIONID=integer(),
                       Dir=character(),
                       Date = character(),
                       HOUR=integer())
for(file in daily_data_filenames){
  daily_data = read.csv(paste0("./daily_data/", file), stringsAsFactors = F, strip.white = T)
  tomtom_data = rbind(tomtom_data, daily_data)
}

# Log the TomTom data process
data_log = readRDS('data_log.rds')
data_log$StnDirRowSum = StnDirRowSum
data_log$StnDirNARowSum = StnDirNARowSum
data_log$StnDateDirRowSum = StnDateDirRowSum
data_log$StnDateDirNARowSum = StnDateDirNARowSum
data_log$TomTomDataRowSum = nrow(tomtom_data)
data_log$TomTomDataNARowSum = sum(apply(tomtom_data, anyNA, MARGIN = 1))
data_log$TomTomDataProbeNASum = sum(is.na(tomtom_data$count))
data_log$TomTomDataSpeedNASum = sum(is.na(tomtom_data$AvgSp))
saveRDS(data_log, 'data_log.rds')
cat(paste0('\nTotal number of unique station-direction combinations: ', StnDirRowSum, ' \n'))
cat(paste0('Total number of unique station-direction combinations not matched with TomTom segments: ', StnDirNARowSum, ' \n'))
cat(paste0('Total number of unique station-direction-date combinations: ', StnDateDirRowSum, ' \n'))
cat(paste0('Total number of station-direction-date combinations with missing TomTom segment IDs: ', StnDateDirNARowSum, ' \n'))
cat(paste0('Total number of rows in TomTom data: ', data_log$TomTomDataRowSum, ' \n'))
cat(paste0('Total number of rows with NAs in TomTom volume data: ', data_log$TomTomDataNARowSum, ' \n'))
cat(paste0('Total number of probe counts missing in TomTom data: ', data_log$TomTomDataProbeNASum, ' \n'))
cat(paste0('Total number of average speeds missing in TomTom data: ', data_log$TomTomDataSpeedNASum, ' \n'))

# Save TomTom data
saveRDS(tomtom_data, "tomtom_data.rds")
saveRDS(stn_dir, "stn_dir.rds")

















