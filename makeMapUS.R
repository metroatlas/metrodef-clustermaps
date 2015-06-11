library(ggplot2)
library(maptools)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(spdep)
library(maps)
library(ggmap)

makeMaps <- function() {
  
  data <- read.csv("data/us_single_linkage.csv")
  data$geoid <- sprintf("%05d", data$X)
  data$X <- NULL
  
  data <- data[,c(length(colnames(data)), 2:length(colnames(data))-1)]
  
  # Keep only contiguous states
  data <- subset(data, !grepl("(^15|^02|^72)", data$geoid))
  
  # Population by county
  pop <- read.csv("data/county_pop.csv")
  pop <- pop[,c(2,5)]
  colnames(pop) <- c("geoid","pop")
  pop$geoid <- sprintf("%05d", pop$geoid)
  
  # Keep only contiguous states
  pop <- subset(pop, !grepl("(^15|^02|^72)", geoid))
  
  vMakeMapCol(data = data, pop = pop, col = 2:length(colnames(data)))
}

makeMapCol <- function(data, pop, col) {

  sub.regions <- data[,c(1, 3099)]
  colnames(sub.regions) <- c("geoid","clusters")
  sub.regions <- arrange(sub.regions, geoid)
  
  sub <- data[,c(1, col)]
  colnames(sub) <- c("geoid","clusters")
  
  # Population by cluster
  pop.clusters <- inner_join(sub, pop, by="geoid") %>%
    select(clusters, pop) %>%
    group_by(clusters) %>%
    summarise(totalpop = sum(pop)) %>%
    filter(totalpop > 50000)
  
  sub <- arrange(sub, geoid)
  
  county <- readShapeSpatial("geo/cb_2013_us_county_20m/cb_2013_us_county_20m.shp")
  gpclibPermit()
    
  county <- subset(county, !grepl("(^15|^02|^72)", county@data$GEOID))
  
  county.order <- as.data.frame(as.character(county@data$GEOID))
  colnames(county.order) <- "geoid"
  
  sub <- merge(county.order, sub, by = "geoid", sort = FALSE)
  sub.regions <- merge(county.order, sub.regions, by = "geoid", sort = FALSE)
  
  metro.counties <- full_join(pop.clusters, sub, by = "clusters")
  metro.counties[is.na(metro.counties$totalpop),"clusters"]  <-  0
  metro.counties <- select(metro.counties, clusters, geoid)
  
  metro.counties <- merge(county.order, metro.counties, by = "geoid", sort = FALSE)
  
  regions <- unionSpatialPolygons(county, sub.regions$clusters)
  regions <- fortify(regions)
  
  metros <- unionSpatialPolygons(county, metro.counties$clusters)
  metros <- fortify(metros)
  metros <- filter(metros, id != 0)
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
    geom_polygon(data = metros, aes(x = long, y = lat, group = group), fill = "dark grey", alpha = 1/3, color = "black", size = 0.7) +
    geom_polygon(data = regions, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 1)
  
  filename  <- paste("exports/us/single-50000/", col, ".pdf", sep="", collapse="")
  ggsave(filename = filename,
         plot = plot,
         width = 42,
         height = 29.7,
         unit = 'cm')
}

vMakeMapCol <- Vectorize(makeMapCol, "col")

makeMaps()