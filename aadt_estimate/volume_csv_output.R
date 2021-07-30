library(sf)
library(plyr)
library(foreach)
library(doParallel)
library(leaflet)

network = readRDS('AADT_estimation.rds')
network1 = st_as_sf(network)
network1 = network1[, c(1, 5, 8:10)]
network1$wkt = st_as_text(network1$geometry)
st_geometry(network1) <- NULL

registerDoParallel(6)
network2 = ddply(network1, .(Id),
                 function(x){coords = strsplit(x$wkt, '\\(|\\, |\\)')[[1]];
                 x$coords = coords[ceiling(1+(length(coords)-1)/2)];
                 return(x)}, .parallel = T)
network2$coords = paste0('POINT (', network2$coords, ')')
network2$wkt = NULL

# network3 = st_as_sf(network2, wkt = 'coords')
# leaflet(data=network3) %>% addProviderTiles(providers$Stamen.TonerLite) %>% addCircles(weight = 1)
# saveRDS(network3, paste0(query_date, '/estimated_traffic_volume_ORNL.rds'))
write.table(network2, 'AADT_estimation.csv', sep = ',', row.names = F)
