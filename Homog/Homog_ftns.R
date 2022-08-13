# Homogenization functions

#Packages
library(dplyr)
library(readxl)
library(tibble)

# Set working drive to script directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source lookup tables
# load("LookupTbls/unitsConversions.rda")
# write.csv(unitsConversions, "LookupTbls/unitsConversions.csv", row.names = F)
# units2 <- read.csv("LookupTbls/unitsConversions.csv")
# 
# str(unitsConversions)
# str(units2)

#Homog Functions
find_key_path <- function(target_dir) {
  #DEBUG
  #target_dir <- "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"
  
  key_dir <- gsub("\\\\", "/", target_dir)
  
  if(substr(key_dir, nchar(key_dir), nchar(key_dir)) != "/") {
    key_dir <- paste0(key_dir, "/")
  }
  
  # list files in Google directory
  dirFileList <- list.files(target_dir)
  
  # isolate names from Google directory
  #DEBUG there's a better way to do this --> filenames do not include any of vector of patterns
  dirFileList <- dirFileList[!duplicated(dirFileList)] #remove duplicates
  dirFileList <- dirFileList[!grepl("\\.zip", dirFileList)]# remove zip files
  dirFileList <- dirFileList[!grepl("\\.pdf", dirFileList)] # remove PDF files
  dirFileList <- dirFileList[!grepl("\\.html", dirFileList)] # remove html files
  dirFileList <- dirFileList[!grepl("\\.txt", dirFileList)]# remove txt files
  dirFileList <- dirFileList[!grepl("\\.R", dirFileList)]# remove R files
  
  # isolate key file, and extract details in location and profile tabs
  keyFileName <- dirFileList[grepl("KEY|Key|key", dirFileList, ignore.case = F)]
  
  return(paste0(key_dir, keyFileName))
}


read_key_location <- function(keyPath, req_fields="default") {
  # Read sheet
  locationData <- read_excel(keyPath, sheet=1)
  
  # Default required fields
  if(req_fields == "default") {
    req_fields <- c( #IMPROVE get this from column in the key
      "curator_PersonName",
      "curator_organization",
      "curator_email",
      "time_series",
      "gradient",
      "experiments",
      "merge_align"
    )
  }
  
  # Check if values exist in required fields
    # Print message if missing
  if(any(is.na(locationData[locationData[["var"]] %in% req_fields,]["Value"]))) {
    print("debug")
    print("At least one required field in location tab is missing (see output for missing (NA) value)")
    print(locationData[locationData[["var"]] %in% req_fields,][c("var", "Value")])
    break
  }
  
  # Remove missing fields 
  locationData <- locationData %>% dplyr::filter(!is.na(Value)) 
  
  return(locationData)
}


read_key_profile <- function(keyPath) {
  # Read sheet
  profileData <- read_excel(keyPath, sheet=2)
  
  # Remove missing fields
  profileData <- profileData %>% dplyr::filter(!is.na(header_name)) 
  
  return(profileData)
}


build_key_notes <- function(keyPath, locationData, profileData) {
  notes <- dplyr::bind_rows(
    tibble(
      source = "location",
      Var_long = "Homogenization path",
      var = NA,
      var_notes = keyPath
    ),
    locationData %>% # Grab necessary info/values from location data
      filter(var %in% c("site_code", "location_name")) %>% 
      dplyr::mutate(source = "location") %>%
      dplyr::select(source, Var_long, var, var_notes = Value),
    locationData %>% # Pulls in any var (row) with notes in location data
      dplyr::filter(!is.na(var_notes)) %>% 
      dplyr::mutate(source = "location") %>%
      dplyr::select(source, Var_long, var, var_notes),
    profileData %>% # Pulls in any var (row) with notes or comments in profile data
      dplyr::filter(!is.na(Notes) | !is.na(Comment)) %>% 
      mutate(var_notes = case_when(
        !is.na(Notes) & !is.na(Comment) ~ paste0(Notes, "; ", Comment),
        !is.na(Notes) & is.na(Comment) ~ Notes,
        is.na(Notes) & !is.na(Comment) ~ paste0(Comment, ""))
      ) %>%
      dplyr::mutate(source = "profile") %>%
      dplyr::select(source, Var_long, var, var_notes)
  )
  return(notes)
}

build_unitConv_notes <- function() {
  conversionNotes <- tibble(
    source = as.character(),
    var = as.character(),
    Var_long = as.character(),
    given_unit = as.character(),
    target_unit = as.character(),
    unit_conversion_factor = as.numeric(),
    varNotes = "converted"
  )
  
  return(conversionNotes)
}


get_unit_conversions <- function(keyPath) {
  unitsConversions <- read_excel(keyPath, sheet=4)
  suppressWarnings(
    unitsConversions$unitConversionFactor <- as.numeric(unitsConversions$unitConversionFactor)
    )
  
  return(unitsConversions)
}


locationData_to_convert <- function(locationData) {
  # capture only location tab DATA with specified units 
  locationDataUnits <- locationData %>%
    dplyr::filter(!is.na(Unit)) %>%
    dplyr::select(Value, unit_levels = Unit, Var_long, var)
  
  # join location DATA with units and corresponding vars in conversion table
  LDU_UCL <- dplyr::left_join(locationDataUnits, unitsConversions, #DEBUG where does this get loaded?
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1  # Resulting data is only vars that need to be converted
    )
  
}


apply_locData_UnitConv <- function(locationData, LDU_UCL) {
  # standardize location data units
  for (varValue in c(LDU_UCL$var)) { 
    # for each var, multiply the value by the conversion factor
    locationData[locationData$var == varValue,]["Value"] <- as.numeric(locationData[locationData$var == varValue,]["Value"]) * LDU_UCL[LDU_UCL$var == varValue,]["unitConversionFactor"]
  }
  
  return(locationData)
}


update_locDataConv_Notes <- function(conversionNotes, LDU_UCL) {
  # Record note of data conversion
  conversionNotes <- conversionNotes %>%
    add_row(source = "location",
            var = varValue,
            Var_long = LDU_UCL[LDU_UCL$var == varValue,]$Var_long.PD,
            given_unit = LDU_UCL[LDU_UCL$var == varValue,]$givenUnit,
            target_unit = LDU_UCL[LDU_UCL$var == varValue,]$unit_levels,
            unit_conversion_factor = LDU_UCL[LDU_UCL$var == varValue,]$unitConversionFactor,
            varNotes = 'converted'
    )
  
  return(conversionNotes)
}



### Homog apply
#---------------------------------------------------------------

data_dir <- "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"

key_path <- find_key_path(data_dir)
locationData <- read_key_location(key_path)
profileData <- read_key_profile(key_path)
notes <- build_key_notes(key_path, locationData, profileData)
unitsConversions <- get_unit_conversions(key_path)
conversionNotes <- build_unitConv_notes()
LDU_UCL<- locationData_to_convert(locationData)
locationData <- apply_locData_UnitConv(locationData, LDU_UCL)
conversionNotes <- update_locDataConv_Notes(conversionNotes, LDU_UCL)  
