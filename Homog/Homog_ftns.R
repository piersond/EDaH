# Homogenization functions
## Key file used to store all information necessary to homogenize data file stored in the same folder

#Packages
library(dplyr)
library(readxl)
library(tibble)
library(purrr)

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
  dirFileList <- dirFileList[!grepl("[~$]", dirFileList)]# remove open excel files
  
  # isolate key file, and extract details in location and profile tabs
  keyFileName <- dirFileList[grepl("KEY|Key|key", dirFileList, ignore.case = F)]
  
  return(paste0(key_dir, keyFileName))
}


read_key_location <- function(keyPath, req_fields="default") {
  # Read sheet
  locationData <- read_excel(keyPath, sheet=1, na="NA")
  
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
  profileData <- read_excel(keyPath, sheet=2, na="NA")
  
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


locationData_to_convert <- function(locationData, unitsConv) {

  # capture only location tab DATA with specified units 
  locationDataUnits <- locationData %>%
    dplyr::filter(!is.na(Unit)) %>%
    dplyr::select(Value, unit_levels = Unit, Var_long, var)
  
  # join location DATA with units and corresponding vars in conversion table
  LDU_UCL <- dplyr::left_join(locationDataUnits, unitsConv, 
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1  # Resulting data is only vars that need to be converted
    )
  
  ### DEBUG: Add error message if unit conversion not found
  
  LDU_UCL$Value <- as.numeric(LDU_UCL$Value)
  LDU_UCL$unitConversionFactor <- as.numeric(LDU_UCL$unitConversionFactor)
  
  return(LDU_UCL)
}


apply_locData_UnitConv <- function(locationData, LDU_UCL, conversionNotes) {
  # standardize location data units
  for (varValue in c(LDU_UCL$var)) { 
    # for each var, multiply the value by the conversion factor
    locationData[locationData$var == varValue,]["Value"] <- as.character(as.numeric(locationData[locationData$var == varValue,]["Value"]) * as.numeric(LDU_UCL[LDU_UCL$var == varValue,]["unitConversionFactor"]))
    
    #Print message showing conversion applied
    print(paste0("Location var unit conversion: '",varValue, "' --> multiplied by ", as.character(LDU_UCL[LDU_UCL$var == varValue,]["unitConversionFactor"])))  
  
  
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
  }
  
  return(list(locationData, conversionNotes))
}


### Location data QC here --------------------------------------------------------

# function to check for location vars in prescribed range
location_range_check <- function(locVarData) {

  tryCatch({
    
    locVarData$Value <- as.numeric(locVarData$Value)
    locVarData$minValue <- as.numeric(locVarData$minValue)
    locVarData$maxValue <- as.numeric(locVarData$maxValue)
    
    if (locVarData$Value < locVarData$minValue | locVarData$Value > locVarData$maxValue) {
      return(
        data.frame(
          var = locVarData$Var,
          error = "out of defined range"
        )
      )
    } 
  },
  warning = function(cond) {
    return(
      data.frame(
        var = locVarData$Var,
        error = "expected the var, min, and max values to be numeric"
        )
      )
  })
} # close location_range_check


# function to check provided data are appropriate type (numeric, character)
location_type_check <- function(locVarData) {
  
  #DEBUG
  #locVarData <- locationData %>% filter(var == "slope_shape") 
  
  if (locVarData[["class"]] == "numeric") {
    if (!is.numeric(as.numeric(locVarData$Value))) {
      return(data.frame(var = locVarData$Var,
                        error = "expected numeric value"))
    }
  } else if (locVarData[["class"]] == "character") {
    if (!grepl("\\D", locVarData$Value)) {
      return(data.frame(var = locVarData$Var,
                        error = "expected character value"))
    }
  }
}


# Function to map through range and type checks for location data
locationData_QC <- function(locData) {
  
  range_data <- locData %>% filter(!is.na(minValue) | !is.na(maxValue)) %>% filter(!is.na(Value))
  location_range_report <- range_data %>% split(1:nrow(range_data)) %>% map(~location_range_check(.)) %>% bind_rows()
  
  locData = locationData
  
  type_data <- locData %>% filter(!is.na(class)) %>% filter(!is.na(Value)) 
  location_type_report <- type_data %>% split(1:nrow(type_data)) %>% map(~location_type_check(.)) %>% bind_rows()
  
  return(bind_rows(location_range_report, location_type_report))
}


### ----- Starting work with data files here ---

# Collect raw data and apply skip rows and NA values defined in location data tab
collect_data_to_homog <- function(target_dir, locData) {
  
  #Fix slash errors in target folder path
  target_dir <- gsub("\\\\", "/", target_dir)
  
  if(substr(target_dir, nchar(target_dir), nchar(target_dir)) != "/") {
    target_dir <- paste0(target_dir, "/")
  }
  
  #get data formatting parameters
  skip_rows <- ifelse(length(locData[locData$var == "header_row",]$var) > 0,
                      as.numeric(locData[locData$var == "header_row",]$Value) - 1,
                      0)
  
  na_val1 <- locData[locData$var == "NA_1",]$Value
  na_val2 <- locData[locData$var == "NA_2",]$Value
  
  if(na_val1 == "blank") {na_val1 = NA}
  if(na_val2 == "blank") {na_val2 = NA}
  if(is.na(na_val1)){na_val1 = ""}
  if(is.na(na_val2)){na_val2 = " "}
   
  #Find .csv data file   ###DEBUG: Can we make this work across multiple files?
  csvFile <- list.files(target_dir, pattern = ".csv")
  if(length(csvFile) == 0) {
    excelFileList <- list.files(target_dir, pattern = ".xls")
    excelFileList <- excelFileList[!grepl("KEY|Key|key", excelFileList)] # remove open excel files 
    target_file <- excelFileList[!grepl("[~$]", excelFileList)] # remove open excel files
    data_raw <- read_excel(paste0(target_dir, target_file), sheet=1, skip=skip_rows, na=c(na_val1, na_val2))
  } else {
    data_raw <- read.csv(paste0(target_dir, csvFile, skip=skip_rows, header=T, as.is=T, na.strings = c(na_val1, na_val2))) 
  }
  
  return(data_raw)
}


# Add columns to profile data with the experimental levels
add_exp_trt_levels <- function(df_in, profileData) {

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
  
  profileDataExpTrt <- profileData %>%
    filter(var %in% experimentTreatmentVarSet,) %>%
    mutate(var_level = paste0(var, "_level")) %>%
    select(unit_levels, var, var_level, header_name)
  
  #create treatment levels dataframe to add to profileData
  for (i in 1:nrow(profileDataExpTrt)) {
    if (i == 1) {
      trt_lvls_df = data.frame(L1 = df_in[[as.character(profileDataExpTrt[i, 4])]],
                               L1_level = as.character(profileDataExpTrt[i, 1]))
      data_cols_replaced <- c(as.character(profileDataExpTrt[i, 4]))
    } else {
      row_to_df <-
        data.frame(data = df_in[[as.character(profileDataExpTrt[i, 4])]],
                   level = as.character(profileDataExpTrt[i, 1]))
      colnames(row_to_df) <-
        c(as.character(profileDataExpTrt[i, 2]),
          as.character(profileDataExpTrt[i, 3]))
      trt_lvls_df <- cbind(trt_lvls_df, row_to_df)
      data_cols_replaced <-
        c(data_cols_replaced, as.character(profileDataExpTrt[i, 4]))
    }
  }
  
  #Add trt level columns to data, removing data columns that were included in 
  # treatment level dataframe
  df_out <-
    cbind(trt_lvls_df, df_in[,-which(names(df_in) %in% data_cols_replaced)])
  
  return(df_out)
} 





