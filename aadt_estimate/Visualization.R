library(leaflet)
library(htmlwidgets)
source('DefaultValues.R')

network = readRDS('AADT_estimation.rds')


pal = colorNumeric(grDevices::heat.colors(10), 0:max(network$FHWA_AADT, na.rm = T))
m1 = leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolylines(data = network, weight = ~(network$FHWA_AADT)/10000, color = ~pal(network$FHWA_AADT), opacity = 0.4, 
               popup = ~paste0("Segment ID: ", as.character(Segment.Id), "<br/>",
                               "FRC: ", as.character(FRC), "<br/>",
                               "AADT: ", as.character(as.integer(network$FHWA_AADT)), "<br/>")) %>%
  addLegend("bottomleft", pal = pal, values = 0:max(network$FHWA_AADT, na.rm = T), title = "AADT")
saveWidget(m1, file = "AADT.html")

