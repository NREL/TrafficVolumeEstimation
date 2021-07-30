library(leaflet)
library(htmlwidgets)
library(sqldf)

args = commandArgs(trailingOnly=TRUE)
query_date1 = args[1]
query_date2 = args[2]
hour = args[3]

network1 = readRDS(paste0(query_date1, '/network.rds'))
network1_df = network1@data
network2 = readRDS(paste0(query_date2, '/network.rds'))
network2_df = network2@data

network_diff_df = sqldf(paste0('select network1_df."Segment.Id", network1_df."FRC", network1_df."Vol_', hour, '" as "Vol_', 8, '_before", network2_df."Vol_', hour, '" as "Vol_', hour, '_after" 
                        from network1_df left join network2_df on network1_df."Segment.Id" = network2_df."Segment.Id"'))
col_name = 'Vol_'
network_diff_df[[paste0(col_name, hour, "_decrease")]] = network_diff_df[[paste0(col_name, hour, "_before")]] - network_diff_df[[paste0(col_name, hour, "_after")]]
max_value = ceiling(max(network_diff_df[paste0(col_name, hour, "_decrease")])/100)*100
min_value = as.integer(min(network_diff_df[paste0(col_name, hour, "_decrease")])/100)*100 - 100
wgt = 300
network_diff = network1
network_diff@data = network_diff_df

pal = colorNumeric(RColorBrewer::brewer.pal(9, 'YlGnBu'), 0:max_value, reverse = T)
m1 = leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolylines(data = network_diff, weight = ~ifelse(network_diff[[paste0(col_name, hour, "_decrease")]]>0, (network_diff[[paste0(col_name, hour, "_decrease")]]+wgt/10)/wgt, 0), 
               color = ~pal(network_diff[[paste0(col_name, hour, "_decrease")]]), opacity = 0.4, 
               popup = ~paste0("Segment ID: ", as.character(Segment.Id), "<br/>",
                               "Traffic Volume Decrease: ", as.character(as.integer(network_diff[[paste0(col_name, hour, "_decrease")]])), "<br/>")) %>%
  addLegend("bottomleft", pal = pal, values = 0:max_value, title = "Volume Decrease")
saveWidget(m1, file = paste0("volume_diff_", hour, ".html"))
system(paste0('mv *.html ./', query_date2))

decrease_ptg = sum(network_diff[[paste0(col_name, hour, "_decrease")]])/sum(network_diff[[paste0(col_name, hour, "_before")]])
cat(paste0('Decrease: ', decrease_ptg, '\n'))
