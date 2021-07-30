library(sqldf)
library(plyr)
library(lubridate)
source('DefaultValues.R')

dates = seq.Date(as.Date(start_date), as.Date(end_date), 1)
dates_df<-data.frame(date=dates, Month=as.numeric(month(dates)), DayOfWeek=weekdays(dates))
month_date_range = ddply(dates_df, .(Month), function(x){data.frame(Month=x$Month[1], first_day=x$date[1], last_day=tail(x$date, 1))})
month_weekday = sqldf('select Month, DayofWeek from dates_df group by Month, DayofWeek')
month_weekday = sqldf('select month_weekday.*, month_date_range.first_day, month_date_range.last_day from month_weekday 
                      left join month_date_range on month_weekday.Month = month_date_range.Month')

for(i in 1:50) {
  system(paste0('Rscript QueryTomTomData.R ', month_weekday$first_day[i], ' ', month_weekday$last_day[i], ' ', month_weekday$DayOfWeek[i]))
  Sys.sleep(10)
}

i = 51
attempt = 0
while(i<=nrow(month_weekday)) {
  Sys.sleep(120)
  system(paste0('Rscript DownloadTomTomData.R ', month_weekday$Month[i-50], ' ', month_weekday$DayOfWeek[i-50]))
  if(file.exists(as.character(paste0(month_weekday$Month[i-50], '_', toupper(substring(month_weekday$DayOfWeek[i-50], 1, 3))))) || attempt >= 20) {
    system(paste0('Rscript QueryTomTomData.R ', month_weekday$first_day[i], ' ', month_weekday$last_day[i], ' ', month_weekday$DayOfWeek[i]))
    i = i + 1
    attempt = 0
  } else {
    attempt = attempt + 1
    # cat(paste0(attempt, " attempt(s)\n"))
    if(attempt >= 20) cat(paste0('TomTom data download for ', month_weekday$Month[i-50], '_', month_weekday$DayOfWeek[i-50], " is failed!\n"))
  }
}

i = nrow(month_weekday)-49
attempt = 0
while(i<=nrow(month_weekday)) {
  Sys.sleep(120)
  system(paste0('Rscript DownloadTomTomData.R ',  month_weekday$Month[i], ' ', month_weekday$DayOfWeek[i]))
  if(file.exists(as.character(paste0(month_weekday$Month[i], '_', toupper(substring(month_weekday$DayOfWeek[i], 1, 3))))) || attempt >= 20) {
    i = i + 1
    attempt = 0
  } else {
    attempt = attempt + 1
    # cat(paste0(attempt, " attempt(s)\n"))
    if(attempt >= 20) cat(paste0('TomTom data download for ', month_weekday$Month[i], '_', month_weekday$DayOfWeek[i], " is failed!\n"))
  }
}

