# Dissertation Map
# 18/03/2024
# Carla Leone

# Load the required library
library(leaflet)
library(readxl)
install.packages("ggmap")
library(ggmap)
sites <- read_excel("data/meta_richness.xlsx", 
                                                       sheet = "big_sheet (2)")
View(sites)
sites$habitat<- as.factor(sites$habitat)

# Create a leaflet map
map2 <- leaflet(sites) %>%
  addTiles()  # Add default OpenStreetMap tiles

habitat_colors <- c("1" = "brown", "2" = "darkblue", "3" = "darkgreen")

# Define icons with colors for each habitat
icons <- awesomeIcons(
  icon = "ios-close",
  iconColor = "black",
  library = "ion",
  markerColor = habitat_colors
)
# Add site markers to the map
map2 <- map2 %>%
  addMarkers(
    lng = ~long,  # Longitude
    lat = ~lat,  # Latitude
    popup = paste("Site:", sites$site, "<br>",
                  "Habitat:", sites$habitat, "<br>"),
    icon = icons[sites$habitat]  # Use icons with colors based on habitat
  )
map2

# Google MapS with Alice advice----
long<- c(-6.042067,-5.557733, -4.559717, -4.726467, -5.1875, -6.225667, -6.48805, -5.94378, -5.575921, -5.601294)
lat<- c(55.667783, 56.107717, 52.942633, 54.0774, 55.9264, 57.158333, 57.058967, 57.14924, 56.155607, 55.735104)
habitat<- c(1,1,1,3,3,3,3,2,2,2)
df <- cbind(long, lat, habitat)
df <- as.data.frame(df)
df$habitat<- as.factor(df$habitat)

?get_googlemap
?registergooglekey
#register the API
register_google(key = "AIzaSyCn54wTMSrPhS6xcBKQBlV-3SaAYbbsQ8g", write =TRUE)
sites<- get_googlemap(
  center= c(lon = -5.0632715, lat = 55.1632836),
  zoom =6,
  maptype= "satellite",
  apiKey = "AIzaSyCn54wTMSrPhS6xcBKQBlV-3SaAYbbsQ8g",
)

?ggmap
install.packages("devtools")
library(devtools)
devtools::install_github('oswaldosantos/ggsn')
install.packages("ggsn")

# make the map
ggmap(sites)+
  geom_point(aes(x=long, y= lat, color = habitat), data=df, alpha=0.5)
  
site_map
site_map +
  blank() +
  north(df) +
  scalebar(df, dist = 4, dist_unit = "km",
           transform = TRUE, model = "WGS84")
install.packages("ggsn")

geocode("sites", output = "all")

#not working 

## Trying genera, ggmap code ----
# Coordinates of your site (example)
site_lat <- 54.407335  # Latitude
site_lon <- -4.698386 # Longitude

points <- data.frame(
  lon = c(-6.042067,-5.557733, -4.559717, -4.726467, -5.1875, -6.225667, -6.48805, -5.94378, -5.575921, -5.601294),
  lat = c(55.667783, 56.107717, 52.942633, 54.0774, 55.9264, 57.158333, 57.058967, 57.14924, 56.155607, 55.735104)
)

site_map <- get_map(location = c(lon = site_lon, lat = site_lat), zoom = 15, maptype = "satellite")
ggmap(site_map) +
  geom_point(data = points, aes(x = lon, y = lat), color = "red", size = 3) +
  ggtitle("Satellite View Site Map with Points")
