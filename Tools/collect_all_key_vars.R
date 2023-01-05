library(dplyr)
library(readxl)
library(tidyr)

# Set working dir to top level data folder
setwd("C:/github/EDaH/Data/Geomicro")

# Find all key file paths by pattern in filename
key_paths <- data.frame(path = list.files(pattern = "Key_V3", recursive = T))

# Function to return vars and var info from a key
get_key_vars <- function(key_path) {
  key_loc <- read_xlsx(key_path[1,1], sheet="Location_data") %>% select(value, var_long, var, level, hardUnit, class, minValue, maxValue)
  key_prof <- read_xlsx(key_path[1,1], sheet= "Profile_data") %>% select(header_name, var_long, var, level, hardUnit, class, minValue, maxValue)
  colnames(key_prof)[1] <- "value"
  
  all_vars <- rbind(key_loc, key_prof)
  used_vars <- all_vars %>% filter(!is.na(value))
  
  return(used_vars)
}

# Map across key paths and collect vars with values
key_var_tbl <- key_paths %>% split(1:nrow(key_paths)) %>% 
  map(~get_key_vars(key_path = .)) %>% bind_rows() %>% 
  select(!value) %>%
  distinct()

# Save tbl
write.csv(key_var_tbl, "key_var_tbl.csv", row.names = F)

