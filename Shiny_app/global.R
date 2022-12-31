library(raster)
library(rgdal)
library(viridis)
library(dplyr)
library(shinythemes)
library(plotly)


#setwd("C:/GitHub/RC_shiny")

RC_database <- readRDS("data/RC_database_current.rds")
RC_database  <- type.convert(RC_database)
RC_database$uniqueID <- paste0("ID",seq(1,nrow(RC_database),1))
RC_database <- RC_database %>% filter(!is.na(lat)) %>% filter(!is.na(long))


#KeyKey summary
var_info <- read.csv("data/key_var_info.csv")

# RC database summary
RC_data_n <- data.frame(n=colSums(!is.na(RC_database)))
RC_data_n$var <- row.names(RC_data_n)
RC_data_summary <- left_join(RC_data_n, var_info)
RC_data_summary <- RC_data_summary %>% 
                    filter(Level != "location") %>%
                    select(var, Var_short, Var_long, n) %>%
                    filter(!var %in% c("L1", "L2", "L3", "observation_date",
                                       "layer_top", "layer_bot", "coarse_tot",
                                       "veg_note_profile", 	"profile_texture_class"))
colnames(RC_data_summary) <- c("Variable", "Short_Desc", "Description", "Data_Count")

# Unique locations text line
uniq_loc_text <- paste0("Includes ", as.character(count(unique(RC_database[c("lat", "long")]))), " unique sample locations")

# Rename database columns
RC_database <- RC_database %>% rename(Dataset = google_dir)

#Reynolds creek watersheds
rc_watersheds <- readOGR("./map/watersheds_2014.shp", layer="watersheds_2014")
rc_watersheds <- spTransform(rc_watersheds, CRS("+init=epsg:4326"))

#Reynolds creek boundary
rc_boundary <- readOGR("./map/RCrk_Boundary.shp")
rc_boundary <- spTransform(rc_boundary, CRS("+init=epsg:4326"))
# Above step should be removed by saving and loading the transformed shapefile directly


#Reynolds creek met stations
#rc_met <- read.csv("./data/ARS_climate_station_locs.csv", as.is=T)

# Reynolds Creek rasters
raster_MAST <- raster("./map/tsoi_est2_3857.tif")
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
num_vars <- c(
  "None" = "none",
  "SOC %" = "lyr_soc",
  "SOC Stock" = "lyr_soc_stock",
  "SIC %" = "lyr_sic",
  "SIC Stock" = "lyr_sic_stock",
  "Total Soil C" = "lyr_c_tot",
  "Total Soil N" = "lyr_n_tot"
)

num_vars <- RC_data_summary$Variable
names(num_vars) <- RC_data_summary$Short_Desc 
            
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

