library(plyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(sqldf)
library(grid)
library(gtable)
library(doParallel)
library(ggmap)
library(leaflet)
library(rgdal)
library(randomForest)
library(htmlwidgets)
library(plyr)
library(RColorBrewer)
library(doParallel)
library(ggplot2)
library(sqldf)
registerDoParallel(cores=8) 

final_data_pred=read.csv("train_data_pred.csv", stringsAsFactors = F, strip.white = T)
final_data_pred$PredVolume[final_data_pred$PredVolume<0] = 0
final_data_pred$residual = final_data_pred$PredVolume-final_data_pred$Volume
final_data_pred$ae = abs(final_data_pred$PredVolume-final_data_pred$Volume)
final_data_pred = ddply(final_data_pred, .(StationId), function(x){x$ecr = x$ae/max(x$Volume); return(x)}, .parallel = T)
final_data_pred$vol_group = cut(final_data_pred$Volume, breaks=seq(0, 2000, 200), labels=(1:10)*200)
final_data_pred$vol_group = as.integer(as.character(final_data_pred$vol_group))
final_data_pred$prob_group = cut(final_data_pred$ProbeCount, breaks=seq(0, 180, 20), labels=(1:9)*20, right = F)
final_data_pred$prob_group = as.integer(as.character(final_data_pred$prob_group))
final_data_pred$FC = as.character(final_data_pred$FC)
final_data_pred$frc = as.character(final_data_pred$FRC)

# Mean Absolute Error 
mae = mean(final_data_pred$ae)

# Error to Maximum Flow Ratio
emfr = mean(final_data_pred$ecr)

# Mean Absolute Percentage Error & Weighted Abosolute Percentage Error
ape = final_data_pred$ae/final_data_pred$Volume
mape = mean(ape[!is.infinite(ape)], na.rm = T)
wape = mean(final_data_pred$ae)/mean(final_data_pred$Volume)




