# Homogenization functions
## Key file used to store all information necessary to homogenize data file stored in the same folder

#Packages
library(dplyr)
library(readxl)
library(purrr)  #IMPROVE: Does dplyr have a similar map ftn?
library(rmarkdown)
library(DT)

# Get the path to the homog ftns
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

print(getCurrentFileLocation())


# Homogenization Functions
#------------------------------------------------------------------------

# Fix formatting of path to dir with key and data files
format_dir_path <- function(target_dir) {

  #DEBUG
  #target_dir = "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"
  
  key_dir <- gsub("\\\\", "/", target_dir)
  
  if(substr(key_dir, nchar(key_dir), nchar(key_dir)) != "/") {
    key_dir <- paste0(key_dir, "/")
  }
  
  return(key_dir)
}

# Get full path to key file
find_key_path <- function(target_dir) {
  
  #DEBUG
  #target_dir = "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"
  
  target_dir <- format_dir_path(target_dir)
  
  # list files in Google directory
  dirFileList <- list.files(target_dir)
  
  # isolate names from Google directory
  #IMPROVE CODE: there's a better way to do this --> filenames do not include any of vector of patterns
  dirFileList <- dirFileList[!duplicated(dirFileList)] #remove duplicates
  dirFileList <- dirFileList[!grepl("\\.zip", dirFileList)]# remove zip files
  dirFileList <- dirFileList[!grepl("\\.pdf", dirFileList)] # remove PDF files
  dirFileList <- dirFileList[!grepl("\\.html", dirFileList)] # remove html files
  dirFileList <- dirFileList[!grepl("\\.txt", dirFileList)]# remove txt files
  dirFileList <- dirFileList[!grepl("\\.R", dirFileList)]# remove R files
  dirFileList <- dirFileList[!grepl("[~$]", dirFileList)]# remove open excel files
  
  # isolate key file, and extract details in location and profile tabs
  keyFileName <- dirFileList[grepl("KEY|Key|key", dirFileList, ignore.case = F)]
  
  return(paste0(target_dir, keyFileName))
}


read_key_location <- function(keyPath, req_fields="default") {
  
  #DEBUG
  #keyPath = key_path
  #req_fields="default"
  
  # Read sheet
  locationData <- read_excel(keyPath, sheet=1, na="NA")
  
  # Pull required fields from key
  req_fields <- locationData %>% filter(require == "TRUE")
  
  # Check if values exist in required fields
  if(any(is.na(req_fields$value))) {
    print("At least one required field in location tab is missing value")
    print(req_fields[,c(1,3:5)])
    break
  }
  
  # Remove missing fields 
  locationData <- locationData %>% dplyr::filter(!is.na(value)) 
  
  return(locationData)
}

read_key_profile <- function(keyPath) {
  # Read sheet
  profileData <- read_excel(keyPath, sheet=2, na="NA")
  
  # Remove missing fields
  profileData <- profileData %>% dplyr::filter(!is.na(header_name)) 

  #Replace column name spaces with "."
  profileData$header_name <- gsub(" ", ".", profileData$header_name)
    
  #Replace special characters in column name
  profileData$header_name <- gsub("[^\\w.-]", ".", profileData$header_name, perl=TRUE)
  
  return(profileData)
}


build_key_notes <- function(keyPath, locationData, profileData) {
  #DEBUG
  #keyPath <- key_path
  
  # Add file path to notes as first row
  notes_base <- data.frame(
    source = "location",
    var_long = "Homogenization path",
    var = NA,
    var_notes = keyPath
  )
  
  # Pull in any var (row) with notes in location data
  notes_location <- locationData %>% filter(!is.na(var_notes))
  if (nrow(notes_location) > 0) {
    notes_location <- notes_location %>%
      mutate(source = "location") %>%
      select(source, var_long, var, var_notes)
  } else {
    notes_location <- data.frame(
      source = "location",
      var_long = "NA",
      var = NA,
      var_notes = "No var notes found for location data"
    )
  }
  
  # Pull in any var (row) with notes in profile data
  notes_profile <-
    profileData %>% filter(!is.na(notes) | !is.na(comment))
  if (nrow(notes_profile) > 0) {
    notes_profile <- notes_profile %>%
      mutate(
        var_notes = case_when(
          !is.na(notes) & !is.na(comment) ~ paste0(notes, "; ", comment),!is.na(notes) &
            is.na(comment) ~ notes,
          is.na(notes) & !is.na(comment) ~ paste0(comment, "")
        )
      ) %>%
      mutate(source = "profile") %>%
      select(source, var_long, var, var_notes)
  } else {
    notes_profile <- data.frame(
      source = "profile",
      var_long = "NA",
      var = NA,
      var_notes = "No var notes found for profile data"
    )
  }

  notes <- bind_rows(notes_base, notes_location, notes_profile)
  
  return(notes)
}


build_unitConv_notes <- function() {
  conversionNotes <- tibble(
    source = as.character(),
    var = as.character(),
    var_long = as.character(),
    given_unit = as.character(),
    hard_unit = as.character(),
    unit_conversion_factor = as.numeric(),
    varNotes = "converted"
  )
  
  return(conversionNotes)
}


get_unit_conversions <- function(keyPath) {
  
  #DEBUG
  #keyPath <- key_path
  
  unitsConversions <- read_excel(keyPath, sheet="unitConversions", na="NA")
  suppressWarnings(
    unitsConversions$ConversionFactor <- as.numeric(unitsConversions$ConversionFactor)
    )
  
  return(unitsConversions)
}


locationData_to_convert <- function(locationData, unitsConv) {

  #DEBUG
  #unitsConv = unitsConversions
  
  # capture only location tab DATA with specified units 
  locationDataUnits <- locationData %>%
    filter(!is.na(unit)) %>%
    filter(!is.na(hardUnit)) %>%    
    select(value, unit, var_long, var, hardUnit)
  
  # join location data with units (LDU) with conversion table
  LDU_conversions <- left_join(locationDataUnits, unitsConv, 
                                      by = "hardUnit", relationship = "many-to-many") %>%
                      filter(unit == givenUnit) %>%
                      filter( # Resulting data is only vars that need to be converted
                        !is.na(ConversionFactor),
                        ConversionFactor != 1  
                      )
  
  # halt if unit conversion is missing
  if(nrow(LDU_conversions) > 0){
    if(!all(LDU_conversions$var %in% LDU_conversions$var)) {
      print("ERROR: Missing hard unit conversion factor.")
      print("Review units and conversion table in key file.")
      print(paste0("Missing conversion factors for: ", 
                   setdiff(locationDataUnits$var, LDU_conversions$var)))
      #BREAK...send HOMOG UNSUCCESSFUL
    }
    
    # Make sure values to be converted are numeric
    # Check to make sure value is numeric or NA
    if(any(is.na(suppressWarnings(as.numeric(as.character(LDU_conversions$value)))))) {
      print(paste0("FAILED location unit conversion: Expecting numeric value"))
      print("Fix value in location tab and retry.")
      return()
    }
    
    LDU_conversions$value <- as.numeric(LDU_conversions$value)
    LDU_conversions$ConversionFactor <- as.numeric(LDU_conversions$ConversionFactor)
  }
  
  return(LDU_conversions)
}

#FIX: Change to structure used below for profile data
apply_locData_UnitConv <- function(locationData, LDU_UCL, conversionNotes, print_msg = T) {
  
  #DEBUG
  #print_msg = T
  
  # standardize location data units
  for (varvalue in c(LDU_UCL$var)) { 
    #Check to make sure value is numeric
    if(is.na(suppressWarnings(as.numeric(as.character(locationData[locationData$var == varvalue,]["value"]))))) {
      print(paste0("FAILED location unit conversion: '", varvalue, "' value is not numeric"))
      print("Fix value in key file and retry.")
    } else {
      
      # for each var, multiply the value by the conversion factor
      locationData[locationData$var == varvalue,]["value"] <- as.character(as.numeric(locationData[locationData$var == varvalue,]["value"]) * as.numeric(LDU_UCL[LDU_UCL$var == varvalue,]["ConversionFactor"]))
      
      #Print message showing conversion applied
      if(print_msg){
        print(paste0("Location var unit conversion: '",varvalue, "' --> multiplied by ", as.character(LDU_UCL[LDU_UCL$var == varvalue,]["ConversionFactor"])))  
      }


    # Record note of data conversion
    conversionNotes <- conversionNotes %>%
      add_row(source = "location",
              var = varvalue,
              var_long = LDU_UCL[LDU_UCL$var == varvalue,]$var_long,#.PD,
              given_unit = LDU_UCL[LDU_UCL$var == varvalue,]$givenUnit,
              hard_unit = LDU_UCL[LDU_UCL$var == varvalue,]$unit,
              unit_conversion_factor = LDU_UCL[LDU_UCL$var == varvalue,]$ConversionFactor,
              varNotes = 'converted'
      )
    }
  }
  return(list(locationData, conversionNotes))
}


### Location data QC here --------------------------------------------------------

# function to check for location vars in prescribed range
locationData_range_check <- function(varData) {
  
  tryCatch({
    
    varData$value <- as.numeric(varData$value)
    varData$minValue <- as.numeric(varData$minValue)
    varData$maxValue <- as.numeric(varData$maxValue)
    
    if (varData$value < varData$minValue | varData$value > varData$maxValue) {
      return(
        data.frame(
          var = varData$Var,
          error = "Out of defined range"
        )
      )
    } 
  },
  warning = function(cond) {
    return(
      data.frame(
        var = varData$Var,
        error = "value is not numeric"
      )
    )
  })
} 


# function to check provided data are appropriate type (numeric, character)
locationData_type_check <- function(varData) {
  
  #DEBUG
  #print(varData$var)
  #varData <- locationData %>% filter(var == "modification_date")

  if(varData[["class"]] == "numeric") {
    if(!is.numeric(suppressWarnings(as.numeric(varData$value)))) {
      return(data.frame(var = varData$var,
                        error = "expected numeric value"))
    }
  } else if(varData[["class"]] == "character") {
    if(!grepl("\\D", varData$value)) {
      return(data.frame(var = varData$var,
                        error = "expected character value"))
    }
  }
}


# Function to map through range and type checks for location data
locationData_QC <- function(locData) {
  
  #DEBUG
  #locData = unitConv_locationData
  
  if(nrow(locData) > 0){
    range_data <- locData %>% filter(!is.na(minValue) | !is.na(maxValue)) %>% filter(!is.na(value))
    if(nrow(range_data) > 0) {
      location_range_report <- range_data %>% split(1:nrow(range_data)) %>% map(~locationData_range_check(.)) %>% bind_rows()
    } else {
      location_range_report = data.frame(
                                var = "ALL",
                                error = "No range errors found.")
    }
    
    type_data <- locData %>% filter(!is.na(class)) %>% filter(!is.na(value)) 
    if(nrow(type_data) > 0) {
      location_type_report <- type_data %>% split(1:nrow(type_data)) %>% map(~locationData_type_check(.)) %>% bind_rows()
    } else {
      location_type_report = data.frame(
                    var = "ALL",
                    error = "No data class errors found.")
    }
    
    if(nrow(location_type_report) == 0) {
      location_type_report = data.frame(
        var = "ALL",
        error = "No data class errors found.")
    }
    
    return(bind_rows(location_range_report, location_type_report))
  } else {
    return("Error: No location data to QC")
  }
}


### ----- Starting work with data files here ---

# Collect raw data and apply skip rows and NA values defined in location data tab
collect_data_to_homog <- function(target_dir, locData) {
  
  #DEBUG
  #target_dir = data_dir
  #locData = unitConv_locationData
  
  #Fix slash errors in target folder path
  target_dir <- format_dir_path(target_dir)
  
  #get data formatting parameters
  skip_rows <- ifelse(length(locData[locData$var == "header_row",]$var) > 0,
                      as.numeric(locData[locData$var == "header_row",]$value) - 1,
                      0)
  
  na_val1 <- locData[locData$var == "NA_1",]$value
  na_val2 <- locData[locData$var == "NA_2",]$value
  
  if(length(na_val1) == 0) {na_val1 = NA} else if(toupper(na_val1) == "BLANK"){na_val1 = NA} 
  if(length(na_val2) == 0) {na_val2 = NA} else if(toupper(na_val2) == "BLANK"){na_val2 = NA} 
  if(is.na(na_val1)){na_val1 = ""}
  if(is.na(na_val2)){na_val2 = " "}
   
  #Find .csv data file   ###IMPROVE: Can we make the homog work across multiple files?
  csvFile <- list.files(target_dir, pattern = "\\.csv$")
  csvFile <- csvFile[!grepl('HMGZD', csvFile)] # Remove any previously HMGZD output files
  if(length(csvFile) == 0) {
    excelFileList <- list.files(target_dir, pattern = ".xls")
    excelFileList <- excelFileList[!grepl("KEY|Key|key", excelFileList)] # remove open excel files 
    target_file <- excelFileList[!grepl("[~$]", excelFileList)] # remove open excel files
    data_raw <- read_excel(paste0(target_dir, target_file), sheet=1, skip=skip_rows, na=c(na_val1, na_val2))
  } else {
    data_raw <- read.csv(paste0(target_dir, csvFile), skip=skip_rows, header=T, as.is=T, na.strings = c(na_val1, na_val2), encoding="UTF-8") 
  }
  
  # Fix Excel remnant characters in first column name
  colnames(data_raw)[1] <- gsub("X.U.FEFF.","", colnames(data_raw)[1])
  
  # Replace X.. left by Excel in column names
  colnames(data_raw) <- gsub("X..", "..", colnames(data_raw), perl=TRUE)
  
  return(data_raw)
}


# Add columns to profile data with the experimental levels
add_exp_trt_levels <- function(df_in, profileData) {

  #DEBUG
  #df_in = data_to_homog
  
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
    select(unit, var, var_level, header_name)
  
  #create treatment levels dataframe to add to profileData
  if(nrow(profileDataExpTrt) > 0) {
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
  
  } else {
    return(df_in)
  }
} 


standardize_col_names <- function(df_in, profileData) {
  
  #DEBUG
  #df_in = data_to_homog_w_lvls
  
  # Select headers that need to be renamed
  header_vars <- profileData %>% select(header_name, var, class)
  
  # Send error if key header name not foudn in data file
  missing_data_columns <- setdiff(header_vars$header_name, colnames(df_in))
  if(length(missing_data_columns) > 0) {
    print("*** ERROR: MISSING DATA. DO NOT PROCEED ***")
    print("------------------------------------------------------------")
    for(i in 1:length(missing_data_columns)){
      print(paste0("Data column not found for key entry: ", missing_data_columns[i]))
    }
    print("------------------------------------------------------------")
  }
  
  ### NEED TO HAVE KILL SWITCH HERE ###
  
  # Find data columns not included in key
  diff1 <- setdiff(colnames(df_in), header_vars$header_name)
  diff2 <- setdiff(diff1, c(header_vars$var, paste0(header_vars$var, "_level")))
  
  # Send warning message if data column not found in key
  if(length(diff2) > 0) {
    for(i in 1:length(diff2)){
    print(paste0("Key entry not found for data column: ", diff2[i]))
    }
    print("------------------------------------")
    print("NOTE: Data columns not included in the key file will not be transferred to the homogenized data file.")
  }
  
  # Remove exp and tx level columns
  rename_vars <- header_vars  %>% filter(class != "exp_lvl") %>% filter(class != "tx_lvl")
  
  # Replace colnames with standardized names
  for(i in 1:nrow(rename_vars)) {
    names(df_in)[names(df_in) == rename_vars$header_name[i]] <- rename_vars$var[i]
  }
  
  # Remove data columns not included in key file
  df_out <- df_in[,!(names(df_in) %in% diff2)]
  
  return(df_out)
}


profileUnitConversion <- function(df_in, profileData, unitConv, print_msg = T) {
  
  #DEBUG
  #df_in = stdzd_data
  #profileData = profileData
  #unitConv = unitsConversions
  
  # Find which columns need unit conversions
  unit_data <- profileData %>% filter(!is.na(hardUnit)) %>%
    filter(var != "observation_date")
  
  # Merge unit conversions
  unit_data_to_convert <-
    left_join(unit_data, unitConv, 
              by = "hardUnit", relationship = "many-to-many") %>%
    filter(unit == givenUnit) %>%
    filter( # Resulting data is only vars that need to be converted
      !is.na(ConversionFactor),
      ConversionFactor != 1  
    )
  
  # Break and report if conversion factor is missing
  missing_conversion <-
    unit_data_to_convert %>% filter(is.na(ConversionFactor))
  
  if (nrow(missing_conversion) > 0) {
    print("Missing conversion factor:")
    for (i in 1:nrow(missing_conversion)) {
      print_df <-
        missing_conversion %>% select(header_name,
                                      var,
                                      unit,
                                      hardUnit,
                                      ConversionFactor)
      print(print_df[i, ])
    }
    print("FAILED homogenization. Add unit conversion and retry.")
    return()
  }
  
  # Do not convert if conversion factor = 1
  unit_data_to_convert$ConversionFactor <-
    as.numeric(unit_data_to_convert$ConversionFactor)
  cols_to_convert <-
    unit_data_to_convert %>% filter(ConversionFactor != 1)
  
  # Create a blank notes frame
  profConvNotes <- build_unitConv_notes()

  # Apply unit conversion
  if(nrow(cols_to_convert) > 0) {
    for (i in 1:nrow(cols_to_convert)) {
      
      # Check to make sure value is numeric or NA
      if(all(is.na(suppressWarnings(as.numeric(as.character(df_in[[cols_to_convert$var[i]]])))))) {
        print(paste0("FAILED profile unit conversion: '", cols_to_convert$var[i], "' contains non-numeric value"))
        print("Fix value in data file and retry.")
        return()
      }
      
      # Apply conversion factor
      df_in[[cols_to_convert$var[i]]] = df_in[[cols_to_convert$var[i]]] * cols_to_convert$ConversionFactor[i]
      
      # Update unit in df to help avoid confusion
      df_in[[cols_to_convert$unit[i]]] = df_in[[cols_to_convert$hardUnit[i]]]
      
      # Record note of data conversion
      profConvNotes <- profConvNotes %>%
        add_row(source = "profile",
                var = cols_to_convert$var[i],
                var_long = cols_to_convert$var_long[i],
                hard_unit = cols_to_convert$hardUnit[i],
                given_unit = cols_to_convert$unit[i],
                unit_conversion_factor = cols_to_convert$ConversionFactor[i],
                varNotes = 'Conversion applied'
        )
      
      
      if (print_msg) {
        print(paste0(cols_to_convert$var[i]," unit conversion applied (* ",cols_to_convert$ConversionFactor[i],")")
        )
      }
    }
  } 
  return(list(df_in, profConvNotes))
}


#### Begin profile date QC

# Check that each var data column fits within min/max specified in key file.
## WATCH: Ftn intended to also ensure that the var data type is numeric
profileData_var_QC <- function(profDataRow, df_in){
  
  #DEBUG
  #profData <- profileData %>% filter(!is.na(minValue) | !is.na(maxValue))
  #profDataRow = profData[1,]
  #df_in = stdzd_unitConv_profileData
  
  tryCatch({
    
    minVal = as.numeric(profDataRow$minValue)
    maxVal = as.numeric(profDataRow$maxValue)
    data_to_check = as.numeric(df_in[[profDataRow$var]]) 
    
    if(any(data_to_check < minVal | data_to_check > maxVal)) {
       return(
        data.frame(
          var = profDataRow$var,
          error = "Out of defined range"
        )
      )
    } 
  },
  warning = function(cond) {
    return(
      data.frame(
        var = profDataRow$var,
        error = "value is not numeric"
      )
    )
  })
}

# function to check for location vars in prescribed range
profileData_QC <- function(profData, df_in) {
  
  #DEBUG
  #profData = profileData
  #df_in = stdzd_unitConv_profileData
  
  range_data <- profData %>% filter(!is.na(minValue) | !is.na(maxValue))
  
  if(nrow(range_data) > 0) {
    profile_QC_report <- range_data %>% split(1:nrow(range_data)) %>% map(~profileData_var_QC(profDataRow = ., df_in = df_in)) %>% bind_rows()
  } else {
    profile_QC_report <- data.frame(Notes="No min/max QC values found")
  }
  
  return(profile_QC_report)
}


### Create and save homogenized output 

hmgz <- function(prepared_locData, prepared_profData, out_path, out_csv=T, out_rds=T) {
  
  #DEBUG
  #prepared_locData = unitConv_locationData
  #prepared_profData = stdzd_unitConv_profileData
  #out_path <- getwd()
  
  # Filter loc data to keep for homogenized file, then select required columns
  locData <- prepared_locData %>% filter(include == "TRUE") %>%
              select(var, value)
  
  # Transpose location data into a single row
  locData_T_row <- setNames(data.frame(t(locData[,-1])), locData[,1])
  
  # Add current date and time
  locData_T_row$homog_date <- Sys.time()
  
  # Prep loc data rows to align with profile data (i.e. create equal number of rows)
  rep_loc_data <- as.data.frame(lapply(locData_T_row, rep, nrow(prepared_profData)))
  
  # Join location and profile data
  full_data <- cbind(rep_loc_data, prepared_profData)
  
  #write homogenized data to .csv and/or .rds
  if(out_csv) {
    write.csv(full_data, paste0(out_path, "/HMGZD_data_output_",   format(Sys.time(), "%m-%d-%Y"), ".csv"), row.names=F)
  }
  
  if(out_rds) {
    saveRDS(full_data, paste0(out_path, "/HMGZD_data_output_",   format(Sys.time(), "%m-%d-%Y"), ".rds"))
  }
  
  print("------------------------------------")
  print("Data homogenization complete!")
  print("------------------------------------")
  return(full_data)
}

notes_to_html <- function(EDaH_path, output_dir, base_notes, loc_conv_Notes, 
                          prof_conv_Notes, locDataQC_Notes, profData_QC_Notes){
  
  #DEBUG
  # EDaH_path <- EDaH_dir
  # output_dir <- output_path
  # base_notes <- notes
  # loc_conv_Notes <- loc_conversion_Notes
  # prof_conv_Notes <- prof_conversion_Notes
  # locDataQC_Notes <- locationDataQC_Notes
  # profData_QC_Notes <- profileData_QC_Notes
  
  # Export note tables as html
  #-----------------------------------------------------------------------------------
  
  # Prep notes tables
  if(nrow(loc_conv_Notes) > 0){locConvNotes = datatable(loc_conv_Notes)} else {locConvNotes = "No notes found."}  
  if(nrow(prof_conv_Notes) > 0){profConvNotes = datatable(prof_conv_Notes)} else {profConvNotes = "No notes found."}  
  if(nrow(locDataQC_Notes) > 0){locQCnotes = datatable(locDataQC_Notes)} else {locQCnotes = "No notes found."}
  if(nrow(profData_QC_Notes) > 0){profQCnotes = datatable(profData_QC_Notes)} else {profQCnotes = "No notes found."}
  
  # Get path to config folder
  notes_template_path <- paste0(EDaH_path, "/config/HMGZD_notes_template.Rmd") 
  
  # render html, send data through params
  rmarkdown::render(
    notes_template_path,#'config/HMGZD_notes_template.Rmd',
    output_file = paste0(output_dir, 'HMGZD_data_notes.html'),
    params = list(data_filename = basename(base_notes[1,4]),
                  data_path = output_dir,
                  notes_tbl = datatable(base_notes), 
                  locConvNotesTbl = locConvNotes,
                  profConvNotesTbl = profConvNotes,
                  locQCnotesTbl = locQCnotes,
                  profQCnotesTbl = profQCnotes
    ),
    quiet = T
  )
  
  # Print success
  print("Homgenization notes saved to HTML file in:") 
  print(output_dir)
}


### Compilers
#----------------------------------------------------------------------------

homog <- function(data_dir, EDaH_dir){
  
  #DEBUG
  #data_dir = "C:\\GitHub\\CZnetGM_SoDaH\\Homog\\Test_dir\\AND_10YR_CN"
  #EDaH_dir = "C:/GitHub/EDaH"
  
  # Load sheets from key file
  #-----------------------------------------------------------------------
  key_path <- find_key_path(data_dir)
  locationData <- read_key_location(key_path)
  profileData <- read_key_profile(key_path)
  notes <- build_key_notes(key_path, locationData, profileData)
  #unitConversions <- read_key_units(key_path)
  
  
  # Location data unit conversion
  #-----------------------------------------------------------------------
  ###REVAMP TO USE unit conversion sheet in key
  unitsConversions <- get_unit_conversions(key_path) 
  conversionNotes <- build_unitConv_notes() 
  LDU_UCL <- locationData_to_convert(locationData, unitsConversions)
  unitConv_locationOutput <- apply_locData_UnitConv(locationData, LDU_UCL, conversionNotes, print_msg = F)
  unitConv_locationData <- as.data.frame(unitConv_locationOutput[[1]])
  loc_conversion_Notes <- as.data.frame(unitConv_locationOutput[[2]]) #output is notes
  
  
  # Location data QC
  #-----------------------------------------------------------------------
  locationDataQC_Notes <- locationData_QC(unitConv_locationData) #output is notes
  
  
  # Standardize profile data
  #-----------------------------------------------------------------------
  data_to_homog <- collect_data_to_homog(data_dir, unitConv_locationData)
  data_to_homog_w_lvls <- add_exp_trt_levels(data_to_homog, profileData)
  stdzd_data <- standardize_col_names(data_to_homog_w_lvls, profileData)
  
  
  # Profile data unit conversion
  #-----------------------------------------------------------------------
  stdzd_unitConv_profileOutput <- profileUnitConversion(stdzd_data, profileData, unitsConversions, print_msg = F)
  stdzd_unitConv_profileData <- as.data.frame(stdzd_unitConv_profileOutput[[1]])
  prof_conversion_Notes <- as.data.frame(stdzd_unitConv_profileOutput[[2]]) #output is notes
  
  
  # Profile data QC
  #-----------------------------------------------------------------------
  profileData_QC_Notes <- profileData_QC(profileData, stdzd_unitConv_profileData) #output is notes
  
  
  # Combine location and profile data, export data (completes data homogenization)
  #----------------------------------------------------------------------------------
  output_path <- format_dir_path(data_dir)
  homog_data <- hmgz(unitConv_locationData, stdzd_unitConv_profileData, output_path, out_csv=T, out_rds=F)
  
  # Output all homogenization notes to html file
  notes_to_html(EDaH_path = EDaH_dir, 
                output_dir = output_path, 
                base_notes = notes, 
                loc_conv_Notes = loc_conversion_Notes, 
                prof_conv_Notes = prof_conversion_Notes, 
                locDataQC_Notes = locationDataQC_Notes, 
                profData_QC_Notes = profileData_QC_Notes)

  return(homog_data)
}
