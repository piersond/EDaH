
# Run homogenization script on specified folder containing a key file-data fiel pair.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_dir <- "C:/Users/DerekPierson/Box/derek.pierson Workspace/Home Office/Temp/example"
edah_dir <- "C:/GitHub/EDaH"

source(paste0(edah_dir, "/Homog_ftns.R"))

### Compiler ftn (full homog in on ftn)
homog_data <- homog(data_dir, edah_dir, remove_HMGZD = T)


###############################################################################
###############################################################################
#-----------------------------------
### STEP BY STEP CODE
#-----------------------------------

# Load sheets from key file
#-----------------------------------------------------------------------
remove_HMGZD(data_dir) # Removes all HMGZD files from data directory
key_path <- find_key_path(data_dir)
locationData <- read_key_location(key_path)
profileData <- read_key_profile(key_path)
notes <- build_key_notes(key_path, locationData, profileData)


# Location data unit conversion
#-----------------------------------------------------------------------
unitsConversions <- get_unit_conversions(key_path) 
conversionNotes <- build_unitConv_notes() 
LDU_UCL <- locationData_to_convert(locationData, unitsConversions)
unitConv_locationOutput <- apply_locData_UnitConv(locationData, LDU_UCL, conversionNotes, print_msg = F)
unitConv_locationData <- as.data.frame(unitConv_locationOutput[[1]])
loc_conversion_Notes <- as.data.frame(unitConv_locationOutput[[2]]) #output is notes


# Location data QC
#-----------------------------------------------------------------------
locationDataQC_Notes <- locationData_QC(unitConv_locationData, unitsConversions) #output is notes


# Standardize profile data
#-----------------------------------------------------------------------
data_to_homog <- collect_data_to_homog(data_dir, locationData)
data_to_homog_w_lvls <- add_exp_trt_levels(data_to_homog, profileData)
stdzd_data <- standardize_col_names(data_to_homog_w_lvls, profileData)


# Profile data unit conversion
#-----------------------------------------------------------------------
stdzd_unitConv_profileOutput <- profileUnitConversion(stdzd_data, profileData, unitsConversions, print_msg = F)
stdzd_unitConv_profileData <- as.data.frame(stdzd_unitConv_profileOutput[[1]])
prof_conversion_Notes <- as.data.frame(stdzd_unitConv_profileOutput[[2]]) #output is notes


# Profile data QC
#-----------------------------------------------------------------------
profileData_QC_Notes <- profileData_QC(profileData, stdzd_unitConv_profileData, unitsConversions) #output is notes


# Combine location and profile data, export data (completes data homogenization)
#----------------------------------------------------------------------------------
output_path <- data_dir
homog_data <- hmgz(unitConv_locationData, stdzd_unitConv_profileData, output_path, out_csv=T, out_rds=T)


# Save homogenization notes to HTML file
#------------------------------------------------------------------------------------
notes_to_html(EDaH_path = edah_dir, 
              output_dir = output_path, 
              base_notes = notes, 
              loc_conv_Notes = loc_conversion_Notes, 
              prof_conv_Notes = prof_conversion_Notes, 
              locDataQC_Notes = locationDataQC_Notes, 
              profData_QC_Notes = profileData_QC_Notes)

