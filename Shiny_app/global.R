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
#setwd("C:/GitHub/EDaH/Shiny_app")

### LOAD DATABASE & VARIABLES
########################################################
app_db <- readRDS("data/GeoMicro_database.rds")
app_db  <- suppressWarnings(type.convert(app_db))
app_db$uniqueID <- paste0("ID",seq(1,nrow(app_db),1))
app_db <- app_db %>% filter(!is.na(lat)) %>% filter(!is.na(long))


### LOAD TABLES
########################################################
var_info <- read.csv("data/key_var_tbl.csv", as.is=T)
var_data_n <- data.frame(n=colSums(!is.na(app_db)))
var_data_n$var <- row.names(var_data_n)
var_info <- left_join(var_info, var_data_n, by=c("var" = "var"))

# RC database summary
#app_data_n <- data.frame(n=colSums(!is.na(app_db)))
#app_data_n$var <- row.names(app_data_n)
# app_data_summary <- left_join(var_info, app_data_n, by=c("var" = "var"))
# app_data_summary <- app_data_summary %>% 
#                     filter(is.na(level) | level != "location") %>%
#                     filter(class != "exp_lvl") %>%
#                     select(var, var_long, hardUnit, level, class, minValue, maxValue, n) #%>%
#                     # filter(!var %in% c("L1", "L2", "L3", "observation_date",
#                     #                    "layer_top", "layer_bot", "coarse_tot",
#                     #                    "veg_note_profile", 	"profile_texture_class"))
# colnames(app_data_summary) <- c("Variable", "Description", "Unit", "Level", "Class", "QC_min", "QC_max", "Data_Count")

# Unique locations text line
uniq_loc_text <- paste0("Includes ", as.character(count(unique(app_db[c("lat", "long")]))), " unique sample locations")


# CZN soil pit locations
app_CZCN_pits <- app_db %>% distinct(lat, long, location_name, L1)

# Rename database columns
#app_db <- app_db %>% rename(Dataset = google_dir)

#Reynolds creek watersheds
#app_watersheds <- readOGR("./map/watersheds_2014.shp", layer="watersheds_2014")
#app_watersheds <- spTransform(app_watersheds, CRS("+init=epsg:4326"))

#Reynolds creek boundary
#app_boundary <- readOGR("./map/RCrk_Boundary.shp")
#app_boundary <- spTransform(app_boundary, CRS("+init=epsg:4326"))
# Above step should be removed by saving and loading the transformed shapefile directly


#Reynolds creek met stations
#app_met <- read.csv("./map/ARS_climate_station_locs.csv", as.is=T) %>% 
#            filter(Group == "MET")

#app_weir <- read.csv("./map/ARS_climate_station_locs.csv", as.is=T) %>% 
#  filter(Group == "WEIR")

# Reynolds Creek CZCN soil pits
#app_CZCN_pits <- read.csv("./map/RCrk_CZCN_soil_pit_locs.csv")



# Reynolds Creek rasters
#raster_MAST <- raster("./map/RCrk_MAST_estimate.tif")
#raster_GEP <- raster("./map/RCrk_GEP_estimate_3857.tif") 
#raster_DEM <- raster("./map/RCrk_DEM_3857.tif")
#raster_HILLSH <- raster("./map/RCrk_DEM_Hillshade_3857.tif")

#################################################################
### Manual steps to convert raster CRS to work with leaflet
#raster_utm <- raster("./map/RCrk_DEM_Hillshade.tif")

#raster_proj <- projectRaster(raster_utm,
#              crs = crs("+init=epsg:3857"))

#writeRaster(raster_proj, filename="./map/RCrk_DEM_Hillshade_3857.tif", format="GTiff", overwrite=TRUE)


# Global app varibales
########################
# Choices for map panel drop-down
all_vars <- var_info %>% 
  filter(!is.na(var)) %>%
  filter(!is.na(var_long)) %>%
  pull(var)
names(all_vars) <- var_info %>% 
  filter(!is.na(var)) %>%
  filter(!is.na(var_long)) %>%
  pull(var_long) %>%
  gsub("[<].*[>]", "", .)

num_vars <- var_info %>% 
  filter(!is.na(var)) %>%
  filter(!is.na(var_long)) %>%
  filter(class == "numeric") %>%
  pull(var)
names(num_vars) <- var_info %>% 
  filter(!is.na(var)) %>%
  filter(!is.na(var_long)) %>%
  filter(class == "numeric") %>%
  pull(var_long) %>%
  gsub("[<].*[>]", "", .)

cat_vars <- var_info %>% 
  filter(!is.na(var)) %>%
  filter(!is.na(var_long)) %>%
  filter(class != "numeric") %>%
  pull(var)
names(cat_vars) <- var_info %>% 
  filter(!is.na(var)) %>%
  filter(!is.na(var_long)) %>%
  filter(class != "numeric") %>%
  pull(var_long)

site_names <- gsub("([a-z])([A-Z])","\\1 \\2", unique(app_db$location_name))
max_depth <- max(app_db$layer_top)


# Add a "None" option
#num_vars <- c(num_vars, setNames("None", "None"))
            
raster_lyrs <- c(
  "None" = 0#,
  # "Soil Temperature" = 1,
  # "Gross Ecosystem Productivity" = 2,
  # "Elevation" = 3
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

#sensor_files <- list.files("./data/Sensors")
#sensor_filenames <- gsub(".dat", "", sensor_files)

# Sensor data
sensor_df <- readRDS("data/Sensors/sensor_data.rds")# %>% mutate_if(is.numeric, round, 1)
sensor_analytes <- colnames(sensor_df[6:12])

# Clean up memory
#gc()




#DEBUG

# fig <- plot_ly(data, x = ~x) 
# fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines') 
# fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') 
# fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

# df <- sensor_df %>% filter(Site == "CA") %>%
#   filter(Subsite == "MC") %>%
#   filter(Pit == "R")
# 
# df_surf <- df %>% filter(Depth == "Shallow")
# 
# 
# fig <- plot_ly(type = 'scatter', mode = 'lines+markers')
# 
# TIMESTAMP <- "TIMESTAMP"
# 
# fig <- fig %>% add_trace(x = df_surf[[`TIMESTAMP`]], y = df_surf$CO2, name = paste0("CO2", " Shallow"), 
#                          fill="tozeroy", fillcolor='rgba(26,150,65,0.5)',               
#                          line = list(color = 'rgba(26,150,65,0.7)', width = 2),
#                          marker = list(color = 'rgba(26,150,65,0.8)', size = 3))
# fig
# 
