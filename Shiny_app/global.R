library(raster)
library(rgdal)
library(viridis)
library(dplyr)
library(shinythemes)
library(plotly)
library(leaflet)
#library(leafem)
  #library(devtools)
  #devtools::install_github("r-spatial/leafem")
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)

### For local testing
setwd("C:/GitHub/GeoMicro_shiny")

### LOAD DATABASE & VARIABLES
########################################################
app_db <- readRDS("data/GeoMicro_database.rds")
app_db  <- type.convert(app_db)
app_db$uniqueID <- paste0("ID",seq(1,nrow(app_db),1))
app_db <- app_db %>% filter(!is.na(lat)) %>% filter(!is.na(long))


### LOAD TABLES
########################################################
var.info <- read.csv("data/SOM_data_key.csv", as.is=T)

# RC database summary
app_data_n <- data.frame(n=colSums(!is.na(app_db)))
app_data_n$var <- row.names(app_data_n)
app_data_summary <- left_join(app_data_n, var_info)
app_data_summary <- app_data_summary %>% 
                    filter(is.na(Level) | Level != "location") %>%
                    select(var, Var_short, Var_long, n) %>%
                    filter(!var %in% c("L1", "L2", "L3", "observation_date",
                                       "layer_top", "layer_bot", "coarse_tot",
                                       "veg_note_profile", 	"profile_texture_class"))
colnames(app_data_summary) <- c("Variable", "Short_Desc", "Description", "Data_Count")

# Unique locations text line
uniq_loc_text <- paste0("Includes ", as.character(count(unique(app_db[c("lat", "long")]))), " unique sample locations")

# Rename database columns
app_db <- app_db %>% rename(Dataset = google_dir)

#Reynolds creek watersheds
app_watersheds <- readOGR("./map/watersheds_2014.shp", layer="watersheds_2014")
app_watersheds <- spTransform(app_watersheds, CRS("+init=epsg:4326"))

#Reynolds creek boundary
app_boundary <- readOGR("./map/RCrk_Boundary.shp")
app_boundary <- spTransform(app_boundary, CRS("+init=epsg:4326"))
# Above step should be removed by saving and loading the transformed shapefile directly


#Reynolds creek met stations
app_met <- read.csv("./map/ARS_climate_station_locs.csv", as.is=T) %>% 
            filter(Group == "MET")

app_weir <- read.csv("./map/ARS_climate_station_locs.csv", as.is=T) %>% 
  filter(Group == "WEIR")

# Reynolds Creek CZCN soil pits
app_CZCN_pits <- read.csv("./map/RCrk_CZCN_soil_pit_locs.csv")

# Reynolds Creek rasters
raster_MAST <- raster("./map/RCrk_MAST_estimate.tif")
raster_GEP <- raster("./map/RCrk_GEP_estimate_3857.tif") 
raster_DEM <- raster("./map/RCrk_DEM_3857.tif")
raster_HILLSH <- raster("./map/RCrk_DEM_Hillshade_3857.tif")

#################################################################
### Manual steps to convert raster CRS to work with leaflet
#raster_utm <- raster("./map/RCrk_DEM_Hillshade.tif")

#raster_proj <- projectRaster(raster_utm,
#              crs = crs("+init=epsg:3857"))

#writeRaster(raster_proj, filename="./map/RCrk_DEM_Hillshade_3857.tif", format="GTiff", overwrite=TRUE)


# Global app varibales
########################
# Choices for map panel drop-down
num_vars <- app_data_summary %>% 
              filter(!is.na(Variable)) %>%
              filter(!is.na(Short_Desc)) %>%
              pull(Variable)
names(num_vars) <- app_data_summary %>% 
                    filter(!is.na(Variable)) %>%
                    filter(!is.na(Short_Desc)) %>%
                    pull(Short_Desc)
# Add a "None" option
#num_vars <- c(num_vars, setNames("None", "None"))
            
raster_lyrs <- c(
  "None" = 0,
  "Soil Temperature" = 1,
  "Gross Ecosystem Productivity" = 2,
  "Elevation" = 3
)

# Set names for character type columns
char_vars <- c(
  "Watershed" = "L1"
)


# Create icons for map markers
favicons <- iconList(
  "pit" = makeIcon(
    iconUrl = "./images/pit_icon_2.png",
    iconWidth = 35
  ),
  "weir" = makeIcon(
    iconUrl = "./images/weir_icon.png",
    iconWidth = 25,
    iconHeight = 25
  ),
  "met" = makeIcon(
    iconUrl = "./images/met_icon_1.png",
    iconWidth = 25,
    iconHeight = 25
  ))

### Load sensor data
# sen_NBT <- read.csv("./data/NB_T_sensor_clean.csv")
# sen_NBT$DateTime15 <- as.POSIXct(sen_NBT$DateTime15,
#                                  tryFormats = c("%Y-%m-%d %H:%M:%OS",
#                                                 "%Y/%m/%d %H:%M:%OS",
#                                                 "%Y-%m-%d %H:%M",
#                                                 "%Y/%m/%d %H:%M",
#                                                 "%Y-%m-%d",
#                                                 "%Y/%m/%d"))

sensor_files <- list.files("./data/Sensors")
sensor_filenames <- gsub(".dat", "", sensor_files)
