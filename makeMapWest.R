makeMapsWest <- function() {
  
  data <- read.csv("data/western_us_avg_linkage.csv")
  data$geoid <- sprintf("%05d", data$X)
  data$X <- NULL
  
  data <- data[,c(length(colnames(data)), 2:length(colnames(data))-1)]
  
  # Keep only western states
  data <- data[grep("(^53|^16|^41|^30|^56|^06|^32|^49|^08|^04|^35)", data$geoid),]
  
  vMakeMapCol(data = data, col = 2:length(colnames(data)))
}

makeMapCol <- function(data, col) {
  library(ggplot2)
  library(maptools)
  library(RColorBrewer)
  library(plyr)
  library(spdep)
  library(maps)
  library(ggmap)
  
  sub <- data[,c(1, col)]
  colnames(sub) <- c("geoid","clusters")
  sub <- sub[order(sub$geoid),]
  
  county <- readShapeSpatial("geo/cb_2013_us_county_20m/cb_2013_us_county_20m.shp")
  gpclibPermit()
  
  county <- subset(county, grepl("(^53|^16|^41|^30|^56|^06|^32|^49|^08|^04|^35)", county@data$GEOID))

  county.order <- as.data.frame(as.character(county@data$GEOID))
  colnames(county.order) <- "geoid"
  
  sub <- merge(county.order, sub, by = "geoid", sort = FALSE)
  
  metros <- unionSpatialPolygons(county, sub$clusters)
  metros <- fortify(metros)
  county <- fortify(county, region="GEOID")
  
  plot <- ggplot() + 
    geom_map(data = sub, aes(map_id = geoid), 
             fill = NA,
             map = county, 
             color = "gray", size = 0.25) + 
    #scale_fill_gradientn(colours=brewer.pal(9,"YlGnBu"), name="County population", trans = "log") +
    expand_limits(x = county$long, y = county$lat) +
    coord_map(projection="albers", lat0 = 39, lat1 = 45) +
    theme_nothing(legend = TRUE) +
    geom_polygon(data = metros, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.7)
  
  filename  <- paste("exports/west/outline/", col, ".pdf", sep="", collapse="")
  ggsave(filename = filename,
         plot = plot,
         width = 21,
         height = 29.7,
         unit = 'cm')
}

vMakeMapCol <- Vectorize(makeMapCol, "col")

makeMapsWest()