# Homog_apply

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Homog_ftns.R")

data_dir <- "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"

key_path <- find_key_path(data_dir)
locationData <- read_key_location(key_path)
profileData <- read_key_profile(key_path)
notes <- build_key_notes(key_path, locationData, profileData)
unitsConversions <- get_unit_conversions(key_path)
conversionNotes <- build_unitConv_notes()
LDU_UCL <- locationData_to_convert(locationData, unitsConversions)
locUnitsConverted <- apply_locData_UnitConv(locationData, LDU_UCL, conversionNotes)
locationData <- as.data.frame(locUnitsConverted[[1]])
conversionNotes <- as.data.frame(locUnitsConverted[[2]])



<br>
  
#### ****
  
  
Input: 
  
Output: 