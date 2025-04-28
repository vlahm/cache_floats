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

#get bridges
bridge_query <- opq(bbox = logan_bbox) %>%
    add_osm_feature(key = 'bridge')

bridges_data <- osmdata_sf(bridge_query)

bridges_lines <- bridges_data$osm_lines
bridges_points <- bridges_data$osm_points

# get roads
roads_query <- opq(bbox = logan_bbox) %>%
    add_osm_feature(key = 'highway',
                    value = c('motorway', 'trunk', 'primary', 'secondary',
                              'tertiary', 'unclassified', 'residential'))

roads_data <- osmdata_sf(roads_query)

major_roads <- roads_data$osm_lines[roads_data$osm_lines$highway %in% c('motorway', 'trunk', 'primary'), ]
medium_roads <- roads_data$osm_lines[roads_data$osm_lines$highway %in% c('secondary', 'tertiary'), ]
wee_roads <- roads_data$osm_lines[roads_data$osm_lines$highway %in% c('unclassified', 'residential'), ]

#get dam and bridge midpoints and specify their markers
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

bridges_midpoints <- bridges_lines %>%
    rowwise() %>%
    mutate(geometry = st_sfc(calculate_midpoint(geometry), crs = st_crs(bridges_lines))) %>%
    ungroup() %>%
    st_as_sf() %>%
    bind_rows(bridges_points)

dam_icons <- leaflet::makeAwesomeIcon(
    icon = "glyphicon-plus",
    markerColor = "red",
    iconColor = "white",
    library = "glyphicon"
)

bridge_icons <- leaflet::makeAwesomeIcon(
    icon = "glyphicon-plus",
    markerColor = "orangered",
    iconColor = "white",
    library = "glyphicon"
)

#build map
map <- leaflet() %>%
    addProviderTiles(providers$OpenTopoMap, group = 'Open Topo') %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = 'ESRI Topo') %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Minimal") %>%
    addPolylines(data = rivers, color = '#00688B', weight = 4, group = "Rivers",
                 label = ~name) %>%
    addPolylines(data = streams, color = '#009ACD', weight = 3, group = "Streams",
                 label = ~name) %>%
    addPolylines(data = canals, color = '#00FFFF', weight = 2, group = "Canals",
                 label = ~name) %>%
    addCircleMarkers(data = dams_midpoints,
                     fillOpacity = 0,
                     color = 'red',
                     label = dams_midpoints$name,
                     group = "Dams",
                     weight = 2,
                     opacity = 1) %>%
    addCircleMarkers(data = bridges_midpoints,
                     fillOpacity = 0,
                     color = 'orangered',
                     label = bridges_midpoints$name,
                     group = "Bridges",
                     weight = 2,
                     opacity = 1) %>%
    addPolylines(data = major_roads, color = '#473C8B', weight = 3, group = "Major roads",
                 label = ~name) %>%
    addPolylines(data = medium_roads, color = '#7A67EE', weight = 2, group = "Medium roads",
                 label = ~name) %>%
    addPolylines(data = wee_roads, color = '#BA55D3', weight = 1.5, group = "Wee roads",
                 label = ~name) %>%
    addLayersControl(overlayGroups = c("Rivers", "Major roads", 'Medium roads',
                                       'Wee roads', "Streams", "Dams", 'Bridges', "Canals"),#, "Put-ins/Take-outs"),
                     baseGroups = c("Open Topo", 'ESRI Topo', "ESRI Satellite", "Minimal"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    setView(lng = -111.8, lat = 41.7, zoom = 10)

#write map as standalone html file
htmlwidgets::saveWidget(map, file = "index.html", selfcontained = TRUE)
