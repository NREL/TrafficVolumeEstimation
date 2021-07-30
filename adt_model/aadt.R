library(corrplot)
library(tidyverse)
library(readxl)
library(lubridate)
library(caret)
library(MLmetrics)
library(sqldf)
library(ggplot2)

# Read data 
data <- read.csv("final_train_data.csv")
daily_data = sqldf('Select avg(Temp) as Temp, avg(WindSp) as WindSp, sum(Precip) as Precip, sum(Snow) as Snow, StationId, Date, Dir, Long, Lat, NumberOfLanes, FC, SpeedLimit, FRC, Month, DayOfWeek, 
      avg(AvgSp) as AvgSp, sum(ProbeCount) as ProbeCount, sum(Volume) as Volume, fold from data group by StationId, Date, Dir')
month_day_adt <- read.csv('train_data_adt_pred.csv', stringsAsFactors = F) # read train_data_adt_pred.csv or test_data_adt_pred.csv
names(month_day_adt)[17] = 'ground_adt'
names(month_day_adt)[19] = 'est_ground_adt'
daily_data$Date <- as.Date(daily_data$Date)

# AASHTO AADT calculation
monthly_adt<-month_day_adt %>% group_by(StationId, Dir, Month, FRC) %>%
  summarize(madt=mean(ground_adt), est_madt = mean(est_ground_adt))

AADT_AASHTO<- monthly_adt %>% group_by(StationId, Dir, FRC) %>%
  summarise(ground_AASHTO_AADT = mean(madt), est_AASHTO_AADT = mean(est_madt))
AADT_AASHTO <- AADT_AASHTO %>% group_by(StationId, Dir, FRC) %>% mutate(MAPE = MAPE(est_AASHTO_AADT, ground_AASHTO_AADT), MAE = MAE(est_AASHTO_AADT, ground_AASHTO_AADT))

AADT_AASHTO_mape = mean(AADT_AASHTO$MAPE) 
AADT_AASHTO_mae = mean(AADT_AASHTO$MAE) 
write.csv(AADT_AASHTO, file = "AADT_AASHTO_ADT_aggregation.csv", row.names = F)

#FHWA AADT calculation
dates<-seq(min(daily_data$Date), max(daily_data$Date), by="days")
dates_df<-data.frame(date=dates, Month=as.numeric(month(dates)), DayOfWeek=weekdays(dates))
month_day_n<-dates_df %>% group_by(Month, DayOfWeek) %>% summarize(num_days=n())
month_days<-summarize(month_day_n, days_in_month=sum(num_days))

month_day_adt<-full_join(month_day_adt, month_day_n, by=c( "Month","DayOfWeek"))
month_day_adt<-mutate(month_day_adt, ground_monthly_total_of_day=ground_adt*num_days, est_monthly_total_of_day = est_ground_adt*num_days)

monthly_adt<-month_day_adt %>% group_by(StationId, Month, Dir, FRC) %>% summarize(ground_monthly_total=sum(ground_monthly_total_of_day), est_monthly_total = sum(est_monthly_total_of_day))
monthly_adt<-inner_join(monthly_adt, month_days, by="Month")
monthly_adt<-mutate(monthly_adt, ground_madt=ground_monthly_total/days_in_month, est_madt= est_monthly_total/days_in_month)

AADT_FHWA<- monthly_adt %>% group_by(StationId, Dir, FRC) %>%
  summarize(ground_AADT=sum(ground_monthly_total)/sum(days_in_month), est_AADT = sum(est_monthly_total)/sum(days_in_month))
AADT_FHWA <- AADT_FHWA %>% group_by(StationId, Dir, FRC) %>% mutate(MAPE = MAPE(est_AADT, ground_AADT), MAE = MAE(est_AADT, ground_AADT))

AADT_FHWA_mape = mean(AADT_FHWA$MAPE) 
AADT_FHWA_mae = mean(AADT_FHWA$MAE) 
write.csv(AADT_FHWA, file = "AADT_FHWA_ADT_aggregation.csv", row.names = F)


