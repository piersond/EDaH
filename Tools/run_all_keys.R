# Author: Derek Pierson
# Date created: July 27, 2023

# Script purpose is to homog() all key-data pairs found in the sub-diretcories
# of the specified parent folder
#------------------------------------------------------------------------------

# USER INPUT
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Set parent directory
parent_dir <- "C:/Users/DerekPierson/Box/ltspWorkspace/ltspData/priestRiver/data/year_0"

#-------------------------------------------------------------------------------
# Then run script, enter Y/N to start/stop loop --------------------------------

library(dplyr)
library(time)

# Load EDaH scripts
edah_dir <- "C:/GitHub/EDaH"
source(paste0(edah_dir, "/Homog_ftns.R"))

# Find all key file paths
key_dirs <- dirname(list.files(path = parent_dir, 
                        pattern = "KEY_", 
                        recursive = TRUE,
                        full.names = TRUE))

# Run homog on all key files, one at a time
for(i in 1:length(key_dirs)){
  
  # Print status
  print(paste0("Running homog() at path: ", key_dirs[i]))
  print("----------------------------------------------------")
  
  # Homogenize key-data pair
  homog(key_dirs[i], edah_dir)
  
  # Ask to proceed to next key?
  q1 <- readline("Proceed to next key? (Y/N)")
  if(toupper(q1) == "Y") {next} else {break}
}


