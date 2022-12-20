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

conversion_Notes <- as.data.frame(locUnitsConverted[[2]]) #output is notes
locationDataQC_Notes <- locationData_QC(locationData) #output is notes

data_to_homog <- collect_data_to_homog(data_dir, locationData)
data_to_homog_w_lvls <- add_exp_trt_levels(data_to_homog, profileData)

# Switch data column names to var names

# Apply unit conversions  +  notes/warnings
  # Can this be consolidated into one unit conversion ftn, pulling from a simplified table in key

# QC - type, min/max  +  notes

# Add location data columns to profile data, export homogenized file


### Next steps
# QA PDF report?

# Simplified starter key file

# Alignment with other HMGZD files?






# <br>
#   
# #### ****
#   
#   
# Input: 
#   
# Output: 