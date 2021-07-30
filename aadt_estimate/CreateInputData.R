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
query_month = args[1] 
query_wkd = args[2] 
folder_name = paste0(query_month, '_', toupper(substring(query_wkd, 1, 3)))
week_dict = 1:7
names(week_dict) = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Read TomTom network
cat('\nReading network data...\n')
network = readOGR(dsn = paste0('./', folder_name), layer = 'network', stringsAsFactors = F)
network_df = network@data
coords = foreach(i = 1:length(network), .combine = rbind) %dopar% {
  network@lines[[i]]@Lines[[1]]@coords[1,]
}
network_df$LONGITUDE = coords[, 1]
network_df$LATITUDE = coords[, 2]
saveRDS(network, paste0(folder_name, '/network.rds'))

# Obtain daily data
cat('\nProcessing TomTom data...\n')
filenames = list.files(paste0('./', folder_name, '/'))
daily_data = data.frame(Id=integer(),
                        AvgSp=numeric(), 
                        count=integer(),
                        HOUR=integer())
for(file in filenames){
  if(grepl(folder_name, file)){
    data = read.dbf(paste0("./", folder_name, "/", file))
    data = data[, c(1,5,9)]
    names(data)= c("Id", "AvgSp", "count")
    # data$HOUR = as.integer(unlist(strsplit(file, "\\-|\\_"))[4])
    daily_data = rbind(daily_data, data)
  }
}

daily_data = sqldf('select daily_data.*, network_df."SpeedLimit" as speed_limit, network_df."FRC" as frc, network_df."LONGITUDE", network_df."LATITUDE" 
                   from daily_data left join network_df
                   on daily_data."Id" = network_df."Id"')
daily_data$DayOfWeek = week_dict[query_wkd]
daily_data$Month = query_month

# Output daily data
daily_data$count[is.na(daily_data$count)] = 0
daily_data$AvgSp[daily_data$count==0] = daily_data$speed_limit[daily_data$count==0]
daily_data = daily_data[, c('Id', 'LONGITUDE', 'LATITUDE', 'speed_limit', 'frc', 'Month', 'DayOfWeek', 'AvgSp', 'count')]
names(daily_data) = c('Id', 'Long', 'Lat', 'SpeedLimit', 'FRC', 'Month', 'DayOfWeek', 'AvgSp', 'ProbeCount')
write.table(daily_data, file = paste0(folder_name, "/daily_data.csv"), row.names = FALSE, sep=",")
cat('\nInput data is created!\n')

