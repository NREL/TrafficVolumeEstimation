library(data.table)
library(reshape2)
library(sqldf)
library(rgdal)
source('DefaultValues.R')

args = commandArgs(trailingOnly=TRUE) # yyyy-mm-dd hour hour hour ...
query_date = args[1]

network = readRDS(paste0(query_date, '/network.rds'))
daily_data_pred = fread(paste0(query_date, '/daily_data_pred.csv'), stringsAsFactors = F)
pred_volume = dcast(daily_data_pred, Id ~ Hour, value.var = "pred_volume")
names(pred_volume) = c('vid', paste0('Vol_', 0:23))
avg_speed = dcast(daily_data_pred, Id ~ Hour, value.var = "AvgSp")
names(avg_speed) = c('sid', paste0('AvgSp_', 0:23))
network_df = network@data
network_df = sqldf('select network_df.*, pred_volume.* from network_df left join pred_volume
                   on network_df."Id" = pred_volume."vid"')
network_df = sqldf('select network_df.*, avg_speed.* from network_df left join avg_speed
                   on network_df."Id" = avg_speed."sid"')
network@data = network_df
if(file.exists(paste0("./", query_date, '/estimated_traffic_volume'))) {
  system(paste0('rm -r ./', query_date, '/estimated_traffic_volume'))
}
writeOGR(network, dsn=paste0("./", query_date, "/estimated_traffic_volume"), layer="estimated_traffic_volume", driver="ESRI Shapefile")
saveRDS(network, paste0(query_date, '/network.rds'))

