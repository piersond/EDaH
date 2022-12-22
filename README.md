# Environmental data harmonization

Refactor of the soilHarmonization package to allow for easier addition of new location and profile variables in the data key. Data homogenization is now soley dependent on information provided in the key, including variable properties and constraints, unit conversions, and homogenization settings.

Noteworthy changes:

- Adding variable to the key is as simple as adding a new row and filling in the required information. A key_check and update feature will be added in the future to allow for QC of such key changes, updating of the master key, and key field requirments/locks.

- Key file has been updated ('V3'). Future update will likely remove many of the less common var fields. Note: Key file 'V2' no longer works. Key file versioning will now be complicated by data dependent var additions. Additional updates will be needed to handle this. See comment above.

- Additional unit conversions may be added via a sheet in the key. Unit conversion are now only variable specific if specified.

- No longer dependent on Google Drive. Current version requires local directory input. Source/sync from Google Drive directory (or Box, etc.) may be added in the future. 

- By compartmentalizing the code into discrete functions, it's now easier to debug errors and update code. Also hope that this will ease the processs of adding further functionality.

- Quality control checks are performed and notes are created in the code, but they are not yet exported to a 'notes.PDF.' Coming soon! (...also hoping to add variable specific QC plots)

- With the refactor and no dependencies on Google Drive, the homogenization code runs much faster. Thus, re-homgenizing a large set of files is a breeze. 

- Key file now contains three alignment variables. A future code update is planned to test data alignment across 'HMGZD' files. Expected that such functionality will work well for data from a single project (e.g. align by IGSN and sample date for the CZN GeoMicrobiology project), but will require more thought, code functionality and scrutiny to be implimented across separate projects (e.g. separate datasets from an LTER site, ...original SoDaH database). 
 
<br>

## Code Overview

The harmonization code is split up into a series of easily managed functions.

<br>

## Example single function usage

```
source("Homog_ftns.R")
data_dir <- "C:\\GitHub\\CZnetGM_SoDaH\\Example_dir"
homog_data <- homog(data_dir)
```
Homogenized output files will be saved to the input data directory ('data_dir' in code above).

<br>

## Example usage of the sequential functions

```
#-----------------------------------
### STEP BY STEP CODE
#-----------------------------------

# Load sheets from key file
#-----------------------------------------------------------------------
key_path <- find_key_path(data_dir)
locationData <- read_key_location(key_path)
profileData <- read_key_profile(key_path)
notes <- build_key_notes(key_path, locationData, profileData)
unitConversions <- read_key_units(key_path)


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
locationDataQC_Notes <- locationData_QC(unitConv_locationData) #output is notes


# Standardize profile data
#-----------------------------------------------------------------------
data_to_homog <- collect_data_to_homog(data_dir, locationData)
data_to_homog_w_lvls <- add_exp_trt_levels(data_to_homog, profileData)
stdzd_data <- standardize_col_names(data_to_homog_w_lvls, profileData)


# Profile data unit conversion
#-----------------------------------------------------------------------
stdzd_unitConv_profileOutput <- profileUnitConversion(stdzd_data, profileData, unitConversions, print_msg = F)
stdzd_unitConv_profileData <- as.data.frame(stdzd_unitConv_profileOutput[[1]])
prof_conversion_Notes <- as.data.frame(stdzd_unitConv_profileOutput[[2]]) #output is notes


# Profile data QC
#-----------------------------------------------------------------------
profileData_QC_Notes <- profileData_QC(profileData, stdzd_unitConv_profileData) #output is notes


# Combine location and profile data, export data (completes data homogenization)
#----------------------------------------------------------------------------------
output_path <- getwd()
homog_data <- hmgz(unitConv_locationData, stdzd_unitConv_profileData, output_path, out_csv=T, out_rds=T)

```


<br>

## Homogenization functions

---

#### **find_key_path()**
Collects the file path to the key file. Searches for filename containing the word: "key"  

Input: Folder path

Output: Full path to key file

<br>

#### **read_key_location(req_fields="default")**

Loads the location data tab from the key file. Includes a set of required fields or returns error message. Required fields can be ignored by setting *req_fields = False*

Input: Key file path

Output: Dataframe with location tab data

<br>

#### **read_key_profile()**
Loads the profile tab data.

Input: Key file path

Output: Dataframe with profile tab data

<br>

#### **build_key_notes()**
Collects all notes/comments in both the location and profile tabs of the key file.

Input: Key file path, location dataframe, profile dataframe

Output: Dataframe with combined notes

<br>

#### **build_key_notes()**
Collects all notes in both the location and profile tabs of the key file.

Input: Key file path, location dataframe, profile dataframe

Output: Dataframe with combined notes

<br>

#### **get_unit_conversions()**
Pulls unit conversions from tab in the key file. If required unit conversions are missing, they can be added to the key file tab.  

Input: Key file path

Output: Dataframe of unit conversions by row

<br>
  
#### **build_unitConv_notes()**
Creates empty dataframe for logging unit conversions  
  
Input: none
  
Output: Empty dataframe with specified column names

<br>
  
#### **locationData_to_convert()**
Collects all location data with specified units, then applies unit conversion if specified unit does not equal "givenUnit" specified in unitConversions tab
  
Input: Location dataframe, unit conversions dataframe
  
Output: Dataframe with only the location vars that need to be converted 

<br>
  
#### **apply_locData_UnitConv()**
Applies conversion factors to location data vars specified by *locationData_to_convert()*
  
Input: Location dataframe, output from *locationData_to_convert()* 
  
Output: List containing two dataframes: 1) Location dataframe with converted units, 2) Updated conversionNotes dataframe.

## **DEBUG: Needs to send error message if unit missing or cannot be converted.**




