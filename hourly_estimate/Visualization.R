library(leaflet)
library(htmlwidgets)
source('DefaultValues.R')

args = commandArgs(trailingOnly=TRUE) # yyyy-mm-dd hour hour hour ...
query_date = args[1]
hours = args[-1]

network = readRDS(paste0(query_date, '/network.rds'))
if(is.na(max_vol)) max_vol = ceiling(max(network@data[, 9:32])/500)*500
for (h in hours) {
  pal = colorNumeric(grDevices::heat.colors(10), 0:max_vol)
  m1 = leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    addPolylines(data = network, weight = ~(network[[paste0('Vol_', h)]]+50)/500, color = ~pal(network[[paste0('Vol_', h)]]), opacity = 0.4, 
                 popup = ~paste0("Segment ID: ", as.character(Segment.Id), "<br/>",
                                 "FRC: ", as.character(FRC), "<br/>",
                                 "Speed Limit: ", as.character(SpeedLimit), "<br/>",
                                 "Average Speed: ", as.character(as.integer(network[[paste0('AvgSp_', h)]])), "<br/>",
                                 "Traffic Volume: ", as.character(as.integer(network[[paste0('Vol_', h)]])), "<br/>")) %>%
    addLegend("bottomleft", pal = pal, values = 0:max_vol, title = "Volume")
  saveWidget(m1, file = paste0("volume_", h, ".html"))
}
system(paste0('mv *.html ./', query_date))