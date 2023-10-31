# Author: Derek Pierson
# Date created: July 28, 2023

# Script purpose is to bind together all HMGZD data found in the sub-directories
# of the specified parent folder
#------------------------------------------------------------------------------

# USER INPUT
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Set parent directory
parent_dir <- "C:/Users/DerekPierson/Box/ltspWorkspace/ltspData"

# Set database output path
db_output_path <- "C:/Users/DerekPierson/Box/ltspWorkspace/harmonizedDatabase"

#-------------------------------------------------------------------------------
# Then run script --------------------------------------------------------------


#-------------------------------------------------------------------------------
library(data.table)

# Find all key file paths
hmgzd_files <- list.files(path = parent_dir, 
                               pattern = "HMGZD_data_output", 
                               recursive = TRUE,
                               full.names = TRUE)

# Bind all HMGZD output data into one database
HMGZD_db <- rbindlist(lapply(hmgzd_files, fread), fill=TRUE)

### Alternate method
# library(tidyverse)
# HMGZD_db2 <- NULL
# 
# for(i in 1:length(hmgzd_files)){
#   df <- NULL
#   df <- read.csv(hmgzd_files[i])
#   df <- df %>% mutate(across(everything(), as.character))
#   HMGZD_db2 <- bind_rows(HMGZD_db2, df)
# }
# 

# Export database as .csv and .rds
write.csv(HMGZD_db, paste0(db_output_path, "/HMGZD_database_", Sys.Date(), ".csv"))
saveRDS(HMGZD_db, paste0(db_output_path, "/HMGZD_database_", Sys.Date(), ".rds"))
