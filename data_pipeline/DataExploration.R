library(foreign)
library(rgdal)
library(ggspatial)
library(leaflet)
library(ggplot2)
library(htmlwidgets)
library(ggspatial)
library(corrplot)
library(sqldf)
source('DefaultValues.R')

# Create a folder to save data exploration results and read  data
if(!file.exists('./data_exploration/')) {
  dir.create('./data_exploration/')
}
final_data = readRDS('final_data.rds')
final_data = subset(final_data, !StationId %in% removed_stations)

# Visualization of volume, probe counts, penetration rate
plot_df = sqldf('select "StationId", "Long", "Lat", "FC", "FRC", "NumberOfLanes", avg("ProbeCount") as "AvgProbeCount", avg("PenRate") as "AvgPenRate", avg("Volume") as "AvgVolume" 
                from final_data group by "StationId"')

pal = colorNumeric("YlOrRd", plot_df$AvgVolume)
m1=leaflet(plot_df[order(plot_df$AvgVolume, decreasing = T), ]) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(~Long, ~Lat, radius = ~log(AvgVolume+1)*volume_radius_weight, weight = 1, color = "#777777", fillColor = ~pal(AvgVolume), fillOpacity = 0.7,
             popup = ~paste0("Station ID: ", as.character(StationId), "<br/>",
                             "FC: ", as.character(FC), "<br/>",
                             "FRC: ", as.character(FRC), "<br/>",
                             "Average Volume: ", as.character(AvgVolume), "<br/>")) %>%
  addLegend(position = "bottomright",
            pal = pal, values = ~AvgVolume,
            title = "Volume (veh/h)")
saveWidget(m1, file = "volume.html")
system('mv volume.html ./data_exploration/')

pal = colorNumeric("YlGnBu", 0:(max(plot_df$AvgProbeCount)+1))
m2=leaflet(plot_df[order(plot_df$AvgProbeCount, decreasing = T), ]) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(~Long, ~Lat, radius = ~log(AvgProbeCount+1)*ProbeCount_radius_weight, weight = 1, color = "#777777", fillColor = ~pal(AvgProbeCount), fillOpacity = 0.7,
             popup = ~paste0("Station ID: ", as.character(StationId), "<br/>",
                             "FC: ", as.character(FC), "<br/>",
                             "FRC: ", as.character(FRC), "<br/>",
                             "Average Probe Count: ", as.character(AvgProbeCount), "<br/>")) %>%
  addLegend(position = "bottomright",
            pal = pal, values = ~AvgProbeCount,
            title = "Probe Count (veh/h)")
saveWidget(m2, file = "probe_count.html")
system('mv probe_count.html ./data_exploration/')

pal = colorNumeric("BuPu", plot_df$AvgPenRate)
m3=leaflet(plot_df[order(plot_df$AvgPenRate, decreasing = T), ]) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(~Long, ~Lat, radius = ~AvgPenRate*PenRate_radius_weight, weight = 1, color = "#777777", fillColor = ~pal(AvgPenRate), fillOpacity = 0.7,
             popup = ~paste0("Station ID: ", as.character(StationId), "<br/>",
                             "FC: ", as.character(FC), "<br/>",
                             "FRC: ", as.character(FRC), "<br/>",
                             "Average Penetration Rate: ", as.character(AvgPenRate), "<br/>")) %>%
  addLegend(position = "bottomright",
            pal = pal, values = ~AvgPenRate,
            title = "Penetration Rate")
saveWidget(m3, file = "pen_rate.html")
system('mv pen_rate.html ./data_exploration/')


# Visualize distribution of volume and probe counts
jpeg(filename = "./data_exploration/volume_hist.jpg", width = 720, height = 480, quality=100, res=100)
ggplot(data=final_data, aes(x=Volume)) + 
  geom_histogram(alpha=.7, aes(y=..count../sum(..count..)), fill="#FF6666", breaks=seq(0, ceiling(quantile(final_data$Volume, 0.99)/volume_hist_bin)*volume_hist_bin, volume_hist_bin)) +
  ggtitle("Volume Distribution") + 
  xlab("Volume (Vehs/hr)") + ylab("Frequency") +
  theme(plot.title = element_text(size=18, face="bold", hjust=0.5), 
        legend.position = "none",
        text = element_text(size=18)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=seq(0, ceiling(quantile(final_data$Volume, 0.99)/volume_break_interval)*volume_break_interval, volume_break_interval))
dev.off()

jpeg(filename = "./data_exploration/probe_hist.jpg", width = 720, height = 480, quality=100, res=100)
ggplot(data=final_data, aes(x=ProbeCount)) + 
  geom_histogram(alpha=.7, aes(y=..count../sum(..count..)), fill="#377EB8", breaks=seq(0, ceiling(quantile(final_data$ProbeCount, 0.99)/probe_hist_bin)*probe_hist_bin, probe_hist_bin)) +
  ggtitle("Probe Count Distribution") + 
  xlab("Probe Count (Vehs/hr)") + ylab("Frequency") +
  theme(plot.title = element_text(size=18, face="bold", hjust=0.5), 
        legend.position = "none",
        text = element_text(size=18)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=seq(0, ceiling(quantile(final_data$ProbeCount, 0.99)/prob_break_interval)*prob_break_interval, prob_break_interval))
dev.off()


# Explore relationship between probe counts and traffic volume
for(i in 1:5) {
  seed = sample(1:(nrow(final_data)-snapshot_hours), 1)
  p=ggplot() + 
    geom_line(data= final_data[seed:(seed+snapshot_hours-1),], aes(x=1:snapshot_hours, y = ProbeCount*10, colour = "#FF6666")) + 
    geom_line(data= final_data[seed:(seed+snapshot_hours-1),], aes(x=1:snapshot_hours, y = Volume, colour = "#56B4E9")) +
    xlab("Time (hour)") + ylab("Vehicle Count") +
    ggtitle(paste0("Probe Count vs. Volume (Row ", seed, "-", seed+snapshot_hours-1, ")")) + 
    scale_colour_discrete(name="", labels=c("Volume", "Probe Count * 10"))
  jpeg(filename = paste0("./data_exploration/probe_vs_volume", i, ".jpg"), width = 1920, height = 480, quality=100, res=100)
  print(p)
  dev.off()
}

# Log results of data exploration
data_log = readRDS('data_log.rds')
data_log$FinalDataStationIDs = sort(plot_df$StationId)
data_log$FinalDataStationFCFreq = table(plot_df$FC)
data_log$FinalDataStationFRCFreq = table(plot_df$FRC)
data_log$FinalDataStationLaneCountFreq = table(plot_df$NumberOfLanes)
data_log$ObservationsByStation = table(final_data$StationId)
saveRDS(data_log, 'data_log.rds')
cat('\nStation FC frequency: \n')
data_log$FinalDataStationFCFreq
cat('\nStation FRC frequency: \n')
data_log$FinalDataStationFRCFreq
cat('\nStation Lane Count frequency: \n')
data_log$FinalDataStationLaneCountFreq
cat('\nObservationsByStation: \n')
data_log$ObservationsByStation
