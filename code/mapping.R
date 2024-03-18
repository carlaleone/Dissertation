# Dissertation Map
# 18/03/2024
# Carla Leone

# Load the required library
library(leaflet)
library(readxl)
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

# Display the map
map2

