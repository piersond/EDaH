### Data Homogenization - Bare bones script
# Recoded from RCsoilHarmonization: data_homogenization.R
# Created: 8/3/2022
# Author: Derek Pierson, based from Steven Earl's original SoDaH code

#Packages
library(dplyr)
library(readxl)


# ISSUES:
#1) Format of the key data? .xslx --> tibbles would jive best with original code

#2) Are unit conversion var specific? 
#   Does adding a var require adding rows to unitConversion table?

#3) Split up into smaller functions? Segments the operations, easier to make changes?
#   Perhaps break into ftns based on headings below?

# Requires:
#--------------------------------------------
#unitConversions <- #DEBUG what format is this in from Steven? Simplify editing?
#profileQC <- #What/where is it?
#source ftn (add_exp_trt_levels) # Where is this stored in package code?


### ----- Key file load, check and unit conversion for values in key  ---

### Set target directory, load key file ------------------------------------------------

# Set target folder
directoryName <- "C:/GitHub/CZnetGM_SoDaH/Homog/Test_dir/AND_10YR_CN/" #DEBUG (will be function input)

# list files in Google directory
dirFileList <- list.files(directoryName)

# isolate names from Google directory
#DEBUG there's a better way to do this --> filenames do not include any of vector of patterns
dirFileList <- dirFileList[!duplicated(dirFileList)] #remove duplicates
dirFileList <- dirFileList[!grepl("\\.zip", dirFileList)]# remove zip files
dirFileList <- dirFileList[!grepl("\\.pdf", dirFileList)] # remove PDF files
dirFileList <- dirFileList[!grepl("\\.html", dirFileList)] # remove html files
dirFileList <- dirFileList[!grepl("\\.txt", dirFileList)]# remove txt files
dirFileList <- dirFileList[!grepl("\\.R", dirFileList)]# remove R files

# isolate key file, and extract details in location and profile tabs
keyFileName <- dirFileList[grepl("KEY", dirFileList, ignore.case = F)]

# extract location and profile tabs of key file
locationData <- read_xlsx(paste0(target_dir, keyFileName), sheet=1)
profileData <- read_xlsx(paste0(target_dir, keyFileName), sheet=2)


### Check location tab for missing req'd fields ------------------------------------------------

# (1) confirm requisite input to location tab
locationRequiredFields <- c( #IMPROVE get this from column in the key
  "curator_PersonName",
  "curator_organization",
  "curator_email",
  "time_series",
  "gradient",
  "experiments",
  "merge_align"
)

#DEBUG check if value exists in required rows 
# if (any(is.na(locationData[locationData[["var"]] %in% locationRequiredFields,]["Value"]))) {
#   
#   stop("at least one required field in location tab is missing (see output for missing (NA) value)")
#   print(locationData[locationData[["var"]] %in% locationRequiredFields,][c("var", "Value")])
# }



### Remove blank rows from location and profile data ------------------------------------------------

# remove missing fields and add...
locationData <- locationData %>%
  dplyr::filter(!is.na(Value)) #%>% #DEBUG Any info useful to add to key here?


# remove missing fields from profile
profileData <- profileData %>%
  dplyr::filter(!is.na(header_name)) 


### Generate note file from key file input ----------------------------------

#DEBUG need to run this code to better understand it
# Capture notes from key file location and profile tabs

notes <- dplyr::bind_rows( #DEBUG expects tibble here
  tibble(
    source = "location",
    Var_long = "Google Directory", #DEBUG this may not exist now
    var = NA,
    var_notes = directoryName
  ),
  locationData %>%
    filter(var %in% c("network", "site_code", "location_name")) %>%
    dplyr::mutate(source = "location") %>%
    dplyr::select(source, Var_long, var, var_notes = Value),
  locationData %>%
    dplyr::filter(!is.na(var_notes)) %>%
    dplyr::mutate(source = "location") %>%
    dplyr::select(source, Var_long, var, var_notes),
  profileData %>%
    dplyr::filter(!is.na(Notes) | !is.na(Comment)) %>%
    mutate(var_notes = case_when(
      !is.na(Notes) & !is.na(Comment) ~ paste0(Notes, "; ", Comment),
      !is.na(Notes) & is.na(Comment) ~ Notes,
      is.na(Notes) & !is.na(Comment) ~ Comment)
    ) %>%
    dplyr::mutate(source = "profile") %>%
    dplyr::select(source, Var_long, var, var_notes)
)


### Convert units for location data --------------------------------------

# Create an empty tibble to house unit conversion notes 
conversionNotes <- tibble(
  source = as.character(),
  var = as.character(),
  Var_long = as.character(),
  given_unit = as.character(),
  target_unit = as.character(),
  unit_conversion_factor = as.numeric(),
  varNotes = "converted"
)

# begin standardize units::location data 

# capture location tab DATA containing units only
locationDataUnits <- locationData %>%
  dplyr::filter(!is.na(Unit)) %>%
  dplyr::select(Value, unit_levels = Unit, Var_long, var)

# join location DATA with units and corresponding vars in conversion table
# #DEBUG This provides column with conversion factor?
LDU_UCL <- dplyr::left_join(locationDataUnits, unitsConversions, #DEBUG where does this get loaded?
                            by = c("var", "unit_levels"),
                            suffix = c(".PD", ".UT")) %>%
  dplyr::filter(
    !is.na(unitConversionFactor),
    unitConversionFactor != 1  # Resulting data is only vars that need to be converted
  )

# standardize location data units
for (varValue in c(LDU_UCL$var)) { 
  
  # AKA for each var, multiply the value by the conversion factor
  # standardize values per the units_conversion_table
  locationData[locationData$var == varValue,]["Value"] <- as.numeric(locationData[locationData$var == varValue,]["Value"]) * LDU_UCL[LDU_UCL$var == varValue,]["unitConversionFactor"]
  
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
  
} # close standardize location data units
# end standardize units::location data




### Location data value QC --------------------------------------------------------

# establish empty tibble to log location QC errors
location_QC_report <- tibble(
  var = as.character(NULL),
  error = as.character(NULL)
)

# function to check for location vars in prescribed range
#DEBUG Better place to store these functions? Separate file? --> Data_type_check_ftns.R?
location_range_check <- function(locationVar) {
  
  tryCatch({
    
    targetValue <- locationQC %>%
      filter(!is.na(minValue)) %>%
      inner_join(locationData, by = c("var")) %>%
      filter(var == locationVar) %>%
      mutate(Value = as.numeric(Value))
    
    if (targetValue$Value < targetValue$minValue | targetValue$Value > targetValue$maxValue) {
      
      location_QC_report %>%
        add_row(
          var = locationVar,
          error = "out of range"
        )
      
    }
    
  },
  warning = function(cond) {
    
    return(
      location_QC_report %>%
        add_row(
          var = locationVar,
          error = "expected numeric"
        )
    )
    
  })
  
} # close location_range_check

# function to check provided data are appropriate type (numeric, character)
location_type_check <- function(locationVar) {
  
  targetValue <- locationQC %>%
    filter(!is.na(class)) %>%
    inner_join(locationData, by = c("var")) %>%
    filter(var == locationVar)
  
  if (targetValue[["class"]] == "numeric") {
    
    tryCatch({
      
      locationData %>%
        filter(var == locationVar) %>%
        mutate(Value = as.numeric(Value))
      
      NULL
      
    },
    warning = function(cond) {
      
      return(
        location_QC_report %>%
          add_row(
            var = locationVar,
            error = "expected numeric"
          )
      )
      
    })
    
  } else if (targetValue[["class"]] == "character") {
    
    tryCatch({
      
      locationData %>%
        filter(var == locationVar) %>%
        mutate(Value = as.character(Value))
      
      NULL
      
    },
    warning = function(cond) {
      
      return(
        location_QC_report %>%
          add_row(
            var = locationVar,
            error = "expected character"
          )
      )
      
    })
    
  } else {
    
    NULL
    
  } # close if character
  
} # close location_type_check

# map through range and type checks for location data
location_range_report <- map_df(.x = locationQC %>% filter(!is.na(minValue)) %>% inner_join(locationData, by = c("var")) %>% filter(!is.na(Value)) %>% pull(var),
                                .f = location_range_check)
location_type_report <- map_df(.x = locationQC %>% filter(!is.na(class)) %>% inner_join(locationData, by = c("var")) %>% filter(!is.na(Value)) %>% pull(var),
                               .f = location_type_check)

location_QC_report <- bind_rows(location_range_report, location_type_report)

# report if location QC errors detected
if (nrow(location_QC_report) > 0) {
  
  location_QC_report <- location_QC_report %>%
    group_by(var, error) %>%
    distinct() %>%
    mutate(
      dataset = directoryName,
      source = "location"
    ) %>%
    select(dataset, source, var, error)
  
  warning("location QC errors dectected, see NOTES file")
  print(location_QC_report)
  
}
# end qc check::location data



### ----- Starts working with data files here ---

### Get data import parameters -----------------------------------------------------

# Isolate rows to skip from locationData for data import. 
# This was originally intended to be an input as to the number of rows to skip but
# it seems to have been interpreted by users as the row number of the header.
if (length(locationData[locationData$var == "header_row",]$var) == 1) {
  
  skipRows = as.numeric(locationData[locationData$var == "header_row",]$Value) - 1
  
} else {
  
  skipRows = 0
  
}

# isolate missing value codes from locationData for data import
if (length(locationData[locationData$var == "NA_1",]$var) == 1) {
  
  mvc1 = locationData[locationData$var == "NA_1",]$Value }

if (length(locationData[locationData$var == "NA_2",]$var) == 1) {
  
  mvc2 = locationData[locationData$var == "NA_2",]$Value }

#missingValueCode = "NA"
missingValueCode = NA  #DEBUG
if (exists("mvc1")) { missingValueCode = mvc1 }
if (exists("mvc2")) { missingValueCode = mvc2 }
if (exists("mvc1") && exists("mvc2")) { missingValueCode = c(mvc1, mvc2) }




### Import data file(s) -----------------------------------------------------

# import all (data + key) files from google dir
# DEBUG --> Change code to pull from local folder, not GDrive
# What type for var comes out of this? = A group of dataframes (tibble?)
googleDirData <- lapply(dirFileList[dirFileList$name %in% dirFileNames, ]$id,
                        sheet_download, #DEBUG
                        missingValueCode = missingValueCode,
                        skipRows = skipRows)

# add filenames
names(googleDirData) <- dirFileNames

# as key file is already loaded, remove it from the list of data frames
googleDirData <- googleDirData[!grepl("key", names(googleDirData), ignore.case = T)]

# generate a vector of dataframe columns to keep from key file input to
# header_name
varsToKeep <- profileData %>%
  dplyr::select(header_name) %>%
  dplyr::pull()

# pull targeted vars from each data frame based on header_names in key file
suppressWarnings(
  googleDirData <- purrr::map(googleDirData, select, one_of(varsToKeep))
)

# rename investigator names to key file names
#DEBUG what does this do?
googleDirData <- lapply(googleDirData, function(frame) {
  setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })






### Add experiment and treatment units to data files --------------------

# generate a vector of ALL POSSIBLE experiment and treatment var names from
# the key file
experimentTreatmentVarSet <- c(
  "L1",
  "L2",
  "L3",
  "L4",
  "L5",
  "L6",
  "tx_L1",
  "tx_L2",
  "tx_L3",
  "tx_L4",
  "tx_L5",
  "tx_L6"
)

if (profileData %>% filter(var %in% experimentTreatmentVarSet) %>% nrow() > 0) {
  
  googleDirData <- lapply(googleDirData,
                          add_exp_trt_levels, #DEBUG Where does this function come from?
                          profileData = profileData,
                          experimentTreatmentVarSet = experimentTreatmentVarSet)
  
}


### Standardize units for profile data -----------------------------------

# profile tab DATA containing units only
profileDataUnits <- profileData %>% # e.g. profile data with units
  dplyr::filter(!is.na(unit_levels)) %>%
  dplyr::select(header_name, unit_levels, Var_long, var)

# join profile DATA with units and corresponding vars in conversion table
PDU_UCP <- dplyr::left_join(profileDataUnits, unitsConversions,
                            by = c("var", "unit_levels"),
                            suffix = c(".PD", ".UT")) %>%
  dplyr::filter(
    !is.na(unitConversionFactor),
    unitConversionFactor != 1
  )

# loop through all data frames in google dir
# = for each dataframe, for each column with a conversin factor != 1, 
#   multiply column values by the conversion factor
#   ...and note what was done
for (i in 1:length(googleDirData)) {
  for (dataCol in c(PDU_UCP$var)) {
    if (!is.null(googleDirData[[i]][[dataCol]])) {
      
      #apply conversion factor, ensure numeric
      googleDirData[[i]][[dataCol]] <- as.numeric(googleDirData[[i]][[dataCol]]) * PDU_UCP[PDU_UCP$var == dataCol, ][["unitConversionFactor"]]
      
      # collect notes for data conversion
      conversionNotes <- conversionNotes %>%
        add_row(source = "profile",
                var = dataCol,
                Var_long = PDU_UCP[PDU_UCP$var == dataCol, ]$Var_long.PD,
                given_unit = PDU_UCP[PDU_UCP$var == dataCol, ]$givenUnit,
                target_unit = PDU_UCP[PDU_UCP$var == dataCol, ]$unit_levels,
                unit_conversion_factor = PDU_UCP[PDU_UCP$var == dataCol, ]$unitConversionFactor,
                varNotes = "converted"
        )
    } # close add mention of conversions to notes
  } # close loop through PDU_UCP
} # close loop through googleDirData
# END STANDARDIZE UNITS::profile data




### Profile data value QC ---------------------------------------------------------

# isolate profile data vars with a specified range
profileRanges <- profileQC %>% #DEBUG where does profileQC come from? WHat is it?
  filter(!is.na(minValue)) %>%
  pull(var)

# DEBUG move ftn to separate script
# DEBUG not exactly sure how this ftn works...what does it return?
profile_range_check <- function(frame) {
  
  profileRangeData <- frame %>%
    select(one_of(profileRanges)) #DEBUG what does this ftn do: one_of() 
  
  if (ncol(profileRangeData) > 0) { 
    
    profileRangeData %>%
      summarise_all(funs(min), na.rm = TRUE) %>%
      gather(key = "var", value = "min") %>% #DEBUG gather()?
      inner_join(
        frame %>%
          select(one_of(profileRanges)) %>%
          summarise_all(funs(max), na.rm = TRUE) %>%
          gather(key = "var", value = "max"),
        by = c("var")
      ) %>%
      inner_join(
        profileQC %>%
          select(var, minValue, maxValue),
        by = c("var")
      ) %>%
      mutate(
        min = as.numeric(min),
        max = as.numeric(max)
      ) %>%
      filter(min < minValue | max > maxValue) %>%
      mutate(error = "out of range") %>%
      select(var, min, max, minValue, maxValue, error)
    
  } # close IF profile range vars to test are present
} # close profile_range_check

# Apply profile range check ftn to all data(frames)
suppressWarnings( 
  profile_range_report <- map_df(.x = googleDirData,
                                 .f = profile_range_check)
)



# CHECK DATA TYPE
# vector of all possible numeric vectors from QC template
numericProfileVars <- profileQC %>%
  filter(class == "numeric") %>%
  pull(var)

#DEBUG move ftn to separate script?
profile_type_check <- function(frame) {
  
  # data frame cols expected to be numeric
  expectedNumeric <- frame %>%
    select(one_of(numericProfileVars)) %>%
    select_if(Negate(is.numeric))
  
  # build tibble of non-numeric cols expected to be numeric
  tibble(
    var = colnames(expectedNumeric),
    error = "expected numeric"
  )
  
}

# Apply profile numeric type check ftn to all data(frames)
suppressWarnings(
  profile_type_report <- map_df(googleDirData, profile_type_check)
)

#Update QC report
profile_QC_report <- bind_rows(profile_range_report, profile_type_report)

# report if profile QC errors detected
if (nrow(profile_QC_report) > 0) {
  
  profile_QC_report <- profile_QC_report  %>%
    mutate(
      dataset = directoryName,
      source = "profile"
    ) %>%
    select(dataset, source, var, error, min, min_allowed = minValue, max, max_allowed = maxValue)
  
  warning("profile errors detected, see NOTES file")
  print(profile_QC_report)
  
}
# END QC CHECK::profile data



### DEREK STOPPED RECODE AT: ---------------------------------
# add location metadata and generate HMGZD filenames 