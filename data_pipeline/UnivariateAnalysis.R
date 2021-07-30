library(ggplot2)
library(corrplot)
library(sqldf)
source('DefaultValues.R')

# Create a folder to save univariate analysis results
if(!file.exists('./univariate_analysis/')) {
  dir.create('./univariate_analysis/')
}
final_train_data = read.csv('final_train_data.csv', stringsAsFactors = F)
var_names = c("Temperature", "Wind Speed", "Precipitation", "Snow", "Station Id", "Date", "Direction", "Longitude", "Latitude", "Number of Lanes", 
              "FHWA FC", "Speed Limit", "TomTom FRC", "Day of Week", "Month", "Hour", "Average Speed", "Probe Count", "Volume")
names(var_names) = names(final_train_data)[1:19]

# Plot variable correlations
selected_features = subset(final_train_data, select = -c(StationId, Date, Dir, FC, DayOfWeek, PenRate, fold))
names(selected_features) = var_names[names(selected_features)]
jpeg(filename = "./univariate_analysis/correlation.jpg", width = 720, height = 800, quality=100, res=100)
corrplot(cor(selected_features), type="lower")
dev.off()

# Define a function to plot univariate analysis for categorical variables
categorical_plot = function(var, target, order, text_angle) {
  plot_df = sqldf(paste0('select "', var, '" as x, avg("', target, '") as y, count(*) as count from final_train_data group by "', var, '"'))
  p = ggplot(data=plot_df, aes(x=as.character(x), y=y, group=1)) + 
    geom_bar(fill = 'orange', stat="identity") + 
    scale_x_discrete(limits=as.character(order)) +
    xlab(var_names[var]) + ylab(target) + 
    theme(plot.title = element_text(size=18, face="bold", hjust=0.5), 
          text = element_text(size=16),
          axis.text.x = element_text(angle=text_angle)) +
    scale_y_continuous(limits = y_limits)
  jpeg(filename = paste0('./univariate_analysis/', var, ".jpg"), width = 720, height = 480, quality=100, res=100)
  print(p)
  dev.off()
  return(plot_df)
}

# Define a function to plot univariate analysis for numeric variables
numeric_plot = function(var, target, min, max, interval, text_angle){
  cutoff_pts = seq(min, max, interval)
  lower = seq(min, max-interval, interval)
  upper = seq(min+interval, max, interval)
  label = cut(final_train_data[[var]], breaks=cutoff_pts, labels=upper, include.lowest = T, right = F)
  plot_df = data.frame(label = as.numeric(as.character(label)), y = final_train_data[[target]])
  plot_df = sqldf('select label as x, avg(y) as y, count(*) as count from plot_df group by label')
  plot_df = na.omit(plot_df)
  p = ggplot(data=plot_df, aes(x = x, y = y, group = 1)) + 
    geom_line(colour = 'orange') + geom_point(shape=16, size = 3, alpha=0.5, color = 'orange') +
    xlab(var_names[var]) + ylab(target) +
    theme(plot.title = element_text(size=18, face="bold", hjust=0.5), 
          text = element_text(size=16),
          axis.text.x = element_text(angle=text_angle)) +
    scale_x_continuous(breaks=upper, labels=paste0(lower, '-', upper)) +
    scale_y_continuous(limits = y_limits)
  jpeg(filename = paste0('./univariate_analysis/', var, ".jpg"), width = 720, height = 480, quality=100, res=100)
  print(p)
  dev.off()
  return(plot_df)
}

# Plot univariate analysis for each variable
univariate_plots = list()
univariate_plots$DayOfWeek = tryCatch(categorical_plot('DayOfWeek', 'Volume', c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), 0), 
                                      error=function(err) NA)
univariate_plots$FRC = tryCatch(categorical_plot('FRC', 'Volume', sort(unique(final_train_data$FRC)), 0), 
                                error=function(err) NA)
univariate_plots$Month = tryCatch(categorical_plot('Month', 'Volume', sort(unique(final_train_data$Month)), 0), 
                                  error=function(err) NA)
univariate_plots$Dir = tryCatch(categorical_plot('Dir', 'Volume', sort(unique(final_train_data$Dir)), 0), 
                                error=function(err) NA)
univariate_plots$FC = tryCatch(categorical_plot('FC', 'Volume', sort(unique(final_train_data$FC)), 0), 
                               error=function(err) NA)
univariate_plots$NumberOfLanes = tryCatch(categorical_plot('NumberOfLanes', 'Volume', sort(unique(final_train_data$NumberOfLanes)), 0), 
                                          error=function(err) NA)
univariate_plots$Temp = tryCatch(numeric_plot('Temp', 'Volume', as.integer(quantile(final_train_data$Temp, 0.01)/10)*10, ceiling(quantile(final_train_data$Temp, 0.99)/10)*10, 10, 0), 
                                 error=function(err) NA)
univariate_plots$Precip = tryCatch(numeric_plot('Precip', 'Volume', 0, ceiling(quantile(final_train_data$Precip, 0.99)/0.02)*0.02, 0.02, 0), 
                                   error=function(err) NA)
univariate_plots$WindSp = tryCatch(numeric_plot('WindSp', 'Volume', 0, ceiling(quantile(final_train_data$WindSp, 0.99)/3)*3, 3, 0), 
                                   error=function(err) NA)
univariate_plots$Snow = tryCatch(numeric_plot('Snow', 'Volume', 0, ceiling(max(final_train_data$Snow)/0.2)*0.2, 0.2, 45), 
                                 error=function(err) NA)
univariate_plots$Long = tryCatch(numeric_plot('Long', 'Volume', as.integer(min(final_train_data$Long)/long_bin)*long_bin, ceiling(max(final_train_data$Long)/long_bin)*long_bin, long_bin, 45), 
                                 error=function(err) NA)
univariate_plots$Lat = tryCatch(numeric_plot('Lat', 'Volume', as.integer(min(final_train_data$Lat)/lat_bin)*lat_bin, ceiling(max(final_train_data$Lat)/lat_bin)*lat_bin, lat_bin, 45), 
                                error=function(err) NA)
univariate_plots$SpeedLimit = tryCatch(numeric_plot('SpeedLimit', 'Volume', as.integer(min(final_train_data$SpeedLimit)/10)*10, ceiling(max(final_train_data$SpeedLimit)/10)*10, 10, 0), 
                                       error=function(err) NA)
univariate_plots$Hour = tryCatch(numeric_plot('Hour', 'Volume', 0, 23, 1, 45), 
                                 error=function(err) NA)
univariate_plots$AvgSp = tryCatch(numeric_plot('AvgSp', 'Volume', as.integer(min(final_train_data$AvgSp)/10)*10, ceiling(max(final_train_data$AvgSp)/10)*10, 10, 0), 
                                  error=function(err) NA)
univariate_plots$ProbeCount = tryCatch(numeric_plot('ProbeCount', 'Volume', as.integer(quantile(final_train_data$ProbeCount, 0.01)/probe_bin)*probe_bin, ceiling(quantile(final_train_data$ProbeCount, 0.99)/probe_bin)*probe_bin, probe_bin, 45), 
                                       error=function(err) NA)
saveRDS(univariate_plots, 'univariate_plots.rds')

