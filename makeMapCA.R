makeMaps <- function() {
  
  data <- read.csv("data/cluster_table.csv")
  colnames(data) <- c("geoid", paste("c", 1:68, sep=""))
  data$geoid <- sprintf("%05d", data$geoid)
  data <- data[grep("^06", data$geoid),]
  
  pop <- read.csv("data/county_pop.csv")
  pop <- pop[,c(2,5)]
  colnames(pop) <- c("geoid", "pop2010")
  pop$geoid <- sprintf("%05d", pop$geoid)
  pop <- pop[grep("^06", pop$geoid),]
  
  vMakeMapCol(data = data, col = 2:69, pop = pop)
}

makeMapCol <- function(data, col, pop) {
  library(ggplot2)
  library(maptools)
  library(RColorBrewer)
  library(plyr)
  library(spdep)
  library(maps)
  library(ggmap)
  
  sub <- data[,c(1, col)]
  colnames(sub) <- c("geoid","clusters")
  
  county <- readShapeSpatial("geo/ca_county/ca_county.shp")
  gpclibPermit()
  
  county.order <- as.data.frame(as.character(county@data$GEOID))
  colnames(county.order) <- "geoid"
  
  sub <- merge(sub, pop, by = "geoid")
  sub <- merge(county.order, sub, by = "geoid", sort = FALSE)
  
  metros <- unionSpatialPolygons(county, sub$clusters )
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
  
  filename  <- paste("exports/", col, ".pdf", sep="", collapse="")
  ggsave(filename = filename,
         plot = plot,
         width = 21,
         height = 29.7,
         unit = 'cm')
}

vMakeMapCol <- Vectorize(makeMapCol, "col")

makeMaps()