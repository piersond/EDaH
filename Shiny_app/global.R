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
library(shinycssloaders)
#install.packages("shinycssloaders")

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

# Unique locations text line
uniq_loc_text <- paste0("Includes ", as.character(count(unique(app_db[c("lat", "long")]))), " unique sample locations")

# CZN soil pit locations
app_CZCN_pits <- app_db %>% distinct(lat, long, location_name, L1)


# Global app variables
#--------------------------------------------------------

# Choices for panel drop-downs
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

names(cat_vars) <- replace(names(cat_vars), names(cat_vars)=="Experimental Level 1(top level)", "Landscape position")

# Change any names for character type columns
# cat_vars <- c(
#   "Watershed" = "L1"
# )

site_names <- unique(app_db$location_name)
names(site_names) <- gsub("([a-z])([A-Z])","\\1 \\2", unique(app_db$location_name))

# Get max depth for depth filter slider input
max_depth <- max(app_db$layer_top)

# Add a "None" option
#num_vars <- c(num_vars, setNames("None", "None"))


# Map panel options
#---------------------------------------------------------------------------
raster_lyrs <- c(
  "None" = 0#,
  # "Soil Temperature" = 1,
  # "Gross Ecosystem Productivity" = 2,
  # "Elevation" = 3
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

# Load sensor data
#--------------------------------------------------------

# Sensor data
sensor_df <- readRDS("data/Sensors/sensor_data.rds")# %>% mutate_if(is.numeric, round, 1)

# Summarize sensor data to daily timestep, if file doesn't exist
if(!file.exists("data/Sensors/sensor_data_daily.rds")) {
  sensor_df_daily <- sensor_df %>% mutate(Date = format(sensor_df$TIMESTAMP, "%m/%d/%Y"))
  sensor_df_daily <- sensor_df_daily %>%
    group_by(Date, Site, Subsite, Pit, Depth) %>%
    summarise_at(vars(CO2:ECw), mean, na.rm = TRUE)
  colnames(sensor_df_daily)[1] <- "TIMESTAMP"
  sensor_df_daily$TIMESTAMP <- as.POSIXct(sensor_df_daily$TIMESTAMP, format = "%m/%d/%Y")
  saveRDS(sensor_df_daily, "data/Sensors/sensor_data_daily.rds")
} else {
  sensor_df_daily <- readRDS("data/Sensors/sensor_data_daily.rds")
}

if(!file.exists("data/Sensors/sensor_data_hourly.rds")) {
  sensor_df_hourly <- sensor_df %>% mutate(Date = format(sensor_df$TIMESTAMP, "%m/%d/%Y %H:00"))
  sensor_df_hourly <- sensor_df_hourly %>%
    group_by(Date, Site, Subsite, Pit, Depth) %>%
    summarise_at(vars(CO2:ECw), mean, na.rm = TRUE)
  colnames(sensor_df_hourly)[1] <- "TIMESTAMP"
  sensor_df_hourly$TIMESTAMP <- as.POSIXct(sensor_df_hourly$TIMESTAMP, format = "%m/%d/%Y %H:%M")
  saveRDS(sensor_df_hourly, "data/Sensors/sensor_data_hourly.rds")
} else {
  sensor_df_hourly <- readRDS("data/Sensors/sensor_data_hourly.rds")
}

# Gran analyte column names for selection dropdown
sensor_analytes <- colnames(sensor_df[6:12])


# Misc
#----------------------------------------------------------

# Clean up memory, might help app run faster...?
gc()




