# Homog_apply

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Homog_ftns.R")

data_dir <- "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"


# Load sheets from key file
#-----------------------------------------------------------------------
key_path <- find_key_path(data_dir)
locationData <- read_key_location(key_path)
profileData <- read_key_profile(key_path)
notes <- build_key_notes(key_path, locationData, profileData)
unitConversions <- read_key_units(key_path)


# Location data unit conversion
#-----------------------------------------------------------------------
###REVAMP TO USE unit conversion sheet in key
unitsConversions <- get_unit_conversions(key_path) 
conversionNotes <- build_unitConv_notes() 
LDU_UCL <- locationData_to_convert(locationData, unitsConversions)
unitConv_locationOutput <- apply_locData_UnitConv(locationData, LDU_UCL, conversionNotes, print_msg = T)
unitConv_locationData <- as.data.frame(unitConv_locationOutput[[1]])
loc_conversion_Notes <- as.data.frame(unitConv_locationOutput[[2]]) #output is notes


# Location data QC
#-----------------------------------------------------------------------
locationDataQC_Notes <- locationData_QC(unitConv_locationData) #output is notes


# Standardize data
#-----------------------------------------------------------------------
data_to_homog <- collect_data_to_homog(data_dir, locationData)
data_to_homog_w_lvls <- add_exp_trt_levels(data_to_homog, profileData)
stdzd_data <- standardize_col_names(data_to_homog_w_lvls, profileData)


# Profile data unit conversion
#-----------------------------------------------------------------------
stdzd_unitConv_profileOutput <- profileUnitConversion(stdzd_data , profileData, unitConversions, print_msg = T)
stdzd_unitConv_profileData <- as.data.frame(stdzd_unitConv_profileOutput[[1]])
prof_conversion_Notes <- as.data.frame(stdzd_unitConv_profileOutput[[2]]) #output is notes


# Profile data QC
#-----------------------------------------------------------------------
profileData_QC_Notes <- profileData_QC(profileData, stdzd_unitConv_profileData)


#profileDataQC_Notes <- profileData_QC(stdzd_unitConv_profileData) #output is notes


# Combine location and profile data, completes homogenization
#-----------------------------------------------------------------------
# Add location data columns to profile data, export homogenized file


# Profile data QA report
#------------------------------------------------------------------------------------
# Include homog notes and analysis plots, then export as PDF
# Include option for analyte specific plots by importing script with ggplot code?


### Future steps:
# Simplified starter key file

# Alignment with other HMGZD files?

# Handling sensor data?

# Key check function
# e.g. ensure no duplicate vars
# set/check required data fields



# <br>
#   
# #### ****
#   
#   
# Input: 
#   
# Output: 