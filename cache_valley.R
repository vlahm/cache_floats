library(sf)
library(osmdata)
library(leaflet)
library(dplyr)
library(htmlwidgets)

setwd('~/git/maps/rivers/cache_valley/')

logan_bbox <- st_bbox(c(xmin = -112, xmax = -111.4, ymin = 41.4, ymax = 42), crs = st_crs(4326))

# get waterways and dams
river_query <- opq(bbox = logan_bbox) %>%
    add_osm_feature(key = 'waterway',
                    value = c('river', 'canal', 'stream', 'dam'))

river_data <- osmdata_sf(river_query)

rivers <- river_data$osm_lines[river_data$osm_lines$waterway == 'river', ]
streams <- river_data$osm_lines[river_data$osm_lines$waterway == 'stream', ]
canals <- river_data$osm_lines[river_data$osm_lines$waterway == 'canal', ]
dams <- river_data$osm_lines[river_data$osm_lines$waterway == 'dam', ]
    # filter(name %in% c("Bear River", "Logan River"))

# get roads
roads_query <- opq(bbox = logan_bbox) %>%
    add_osm_feature(key = 'highway',
                    value = c('motorway', 'primary', 'secondary'))#, 'tertiary'))

roads_data <- osmdata_sf(roads_query)

roads <- roads_data$osm_lines

#get dam midpoints and specify their markers
calculate_midpoint <- function(geometry) {
    coords <- sf::st_coordinates(geometry)
    midpoint_index <- floor(nrow(coords) / 2)
    midpoint <- coords[midpoint_index, c("X", "Y")]
    return(sf::st_point(midpoint))
}

dams_midpoints <- dams %>%
    rowwise() %>%
    mutate(geometry = st_sfc(calculate_midpoint(geometry), crs = st_crs(dams))) %>%
    ungroup() %>%
    st_as_sf()

dam_icons <- leaflet::makeAwesomeIcon(
    icon = "glyphicon-plus",
    markerColor = "red",
    iconColor = "white",
    library = "glyphicon"
)

#build map
map <- leaflet() %>%
    addProviderTiles(providers$OpenTopoMap, group = 'Open Topo') %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = 'ESRI Topo') %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Minimal") %>%
    addPolylines(data = rivers, color = 'blue', weight = 4, group = "Rivers",
                 label = ~name) %>%
    addPolylines(data = streams, color = 'green', weight = 4, group = "Streams",
                 label = ~name) %>%
    addPolylines(data = canals, color = 'brown', weight = 4, group = "Canals",
                 label = ~name) %>%
    addCircleMarkers(data = dams_midpoints,
                     fillOpacity = 0,
                     color = 'red',
                     label = rep('+', nrow(dams_midpoints)),
                     group = "Dams",
                     weight = 2,
                     opacity = 1) %>%
    addPolylines(data = roads, color = 'black', weight = 1.5, group = "Roads",
                 label = ~name) %>%
    addLayersControl(overlayGroups = c("Rivers", "Roads", "Streams", "Dams", "Canals"),#, "Put-ins/Take-outs"),
                     baseGroups = c("Open Topo", 'ESRI Topo', "ESRI Satellite", "Minimal"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    setView(lng = -111.8, lat = 41.7, zoom = 10)

#write map as standalone html file
htmlwidgets::saveWidget(map, file = "index.html", selfcontained = TRUE)
