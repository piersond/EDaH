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

### Sensor files
#sensor_files <- list.files("./data/Sensors")
#sensor_filenames <- gsub(".dat", "", sensor_files)