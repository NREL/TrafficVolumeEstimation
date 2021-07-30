library(data.table)
library(foreach)
library(reshape2)
library(sqldf)
library(rgdal)
library(corrplot)
library(tidyverse)
library(readxl)
library(lubridate)
library(caret)
library(MLmetrics)
library(ggplot2)
source('DefaultValues.R')

# Combine data
combined_data = 
  foreach(m = 1:12, .combine = rbind, .errorhandling = "remove") %:% 
  foreach(d = c('MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN'), .combine = rbind, .errorhandling = "remove") %do% {
    freeway_data = read.csv(paste0(m, '_', d, '/freeway_daily_data_pred.csv'), stringsAsFactors = F)
    off_freeway_data = read.csv(paste0(m, '_', d, '/off_freeway_daily_data_pred.csv'), stringsAsFactors = F)
    rbind(freeway_data, off_freeway_data)
    }

combined_data = combined_data[order(combined_data$Id, combined_data$Month, combined_data$DayOfWeek), ]
id_freq = sqldf('select Id, count(*) as count from combined_data group by Id')
# month_day_adt = subset(combined_data, Id %in% id_freq$Id[id_freq$count>=80])
month_day_adt = combined_data
names(month_day_adt)[10] = 'est_ground_adt'

# AASHTO steps which lead to FHWA calculation
monthly_adt<-month_day_adt %>% group_by(Id, Month) %>%
  summarize(est_madt = mean(est_ground_adt))
AADT_AASHTO<- monthly_adt %>% group_by(Id) %>%
  summarise(est_AASHTO_AADT = mean(est_madt))

#FHWA AADT calculation
week_dict = 1:7
names(week_dict) = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

dates<-seq(as.Date(start_date), as.Date(end_date), by="days")
dates_df<-data.frame(date=dates, Month=as.numeric(month(dates)), DayOfWeek=weekdays(dates))
month_day_n<-dates_df %>% group_by(Month, DayOfWeek) %>% summarize(num_days=n())
month_day_n$DayOfWeek = week_dict[month_day_n$DayOfWeek]
month_days<-summarize(month_day_n, days_in_month=sum(num_days))

month_day_adt<-full_join(month_day_adt, month_day_n, by=c( "Month","DayOfWeek"))
month_day_adt<-mutate(month_day_adt, est_monthly_total_of_day = est_ground_adt*num_days)

monthly_adt<-month_day_adt %>% group_by(Id, Month) %>% summarize(est_monthly_total = sum(est_monthly_total_of_day))
monthly_adt<-inner_join(monthly_adt, month_days, by="Month")
monthly_adt<-mutate(monthly_adt, est_madt= est_monthly_total/days_in_month)

AADT_FHWA<- monthly_adt %>% group_by(Id) %>% summarize(est_AADT = sum(est_monthly_total)/365)

# Join AADT with network
network = readRDS('1_MON/network.rds')
network_df = network@data
network_df = sqldf('select network_df.*, AADT_AASHTO.est_AASHTO_AADT as AASHITO_AADT from network_df left join AADT_AASHTO
                   on network_df."Id" = AADT_AASHTO."Id"')
network_df = sqldf('select network_df.*, AADT_FHWA.est_AADT as FHWA_AADT from network_df left join AADT_FHWA
                   on network_df."Id" = AADT_FHWA."Id"')
network@data = network_df
if(!file.exists('./AADT_estimation')) {
  dir.create('./AADT_estimation')
}
writeOGR(network, dsn=paste0("./AADT_estimation"), layer="AADT_estimation", driver="ESRI Shapefile")
saveRDS(network, 'AADT_estimation.rds')

