library(leaflet)
library(leafem)
library(raster)

raster_path <- "C:/Users/Derek/Google Drive/RCrk/BSU_ScholorWorks_Archive/Pierson_RCrk_Soil-C-modeling_data/Spatial Data/Spatial forcing rasters/RCrk_MAST_estimate.tif"


# Create leaflet map and set map location + zoom
map <- leaflet() %>%       
  addProviderTiles("Esri.WorldImagery", 
                   group = "Esri.WorldImagery",
                   options = providerTileOptions(opacity = 1)) %>%
  setView(lng = -116.75, lat = 43.16, zoom = 11)

# OPTION A
### Adding raster using addGeotiff
map %>% addGeotiff(raster_path,
           colorOptions = colorOptions(
            palette = colorRampPalette(c("#05475e", "#faff78", "#ff9a00", "#ff3500"))(100) ,
             breaks = c(5:15),
             na.color = "transparent"),
           opacity = 1,
           group = "MAST")

# OPTION B
### Adding raster using addRasterImage
# Load raster and make a color palette
raster_img <- raster(raster_path)
raster_vals <- values(raster_img)
MAST_pal <- colorBin(palette = c("#05475e", "#faff78", "#ff9a00", "#ff3500"),
                     domain = raster_vals,
                     bins = c(5:15), 
                     pretty = TRUE,
                     na.color = "transparent")
# Add raster to map
map %>% addRasterImage(raster_img, colors = MAST_pal, opacity = 1, group = "MAST")


