![alt text](https://github.com/piersond/EDaH/blob/main/Doc/EDaH_bannerBW.png?raw=true)

# Environmental Data Harmonization Framework 
**Code forked from the [soilHarmonization](https://lter.github.io/soilHarmonization/index.html) package.** 

**Extends work completed by the [SOils DAta Harmonization (SoDaH) & Synthesis project](https://lter.github.io/som-website/).**

#### VERSION: alpha v1.0, pre-release. 
Core functionality complete. Further development and testing required.

[Development Jamboard](https://jamboard.google.com/d/1HWn4mVZ4OYa2J6sptLG2d7qirtVryNWYJfHaEapndEA/edit?usp=sharing)

---

## Description

###### *Paraphrased from soilHarmonization 0.1.0*

Environmental data vary vastly in their structure, units of measure, granularity and other details. To facilitate their use in models and synthesis research, the data must be homogenized to a sufficient degree such that cross-site, -project, -time comparisons are feasible.

To facilitate data harmonization, a key file must be generated for each raw data file, which serves as a guide to translate the user-provided data into a common, database-wide structure and format. For each data set provided, the key file should contain general details about the data provider, the project from which the data were generated, and generalized details that apply to the data broadly (e.g., mean annual precipitation at the study site). Such generalized information is referred to as location or locational data in this project. At a finer resolution, the key file should contain mappings between the provided data and common terminology and units employed by the project for that data type. For example, the project-designated term for the standing stock of soil organic matter is soc_stock in units of g/m2. If the provided data included information about the standing stock of soil organic matter in a column titled soil C with units of %, that translation will be noted by the data provided on the Profile_data tab of the key file. When run, the script will rename the column titled soil C to som_stock and apply the appropriate units conversion.

Example code below pulls from 'Example' directory with data and working key file. 

###### Note: The terms harmonization and homogenization are often used synonymously across text and code in the repository. 

### Major changes from soilHarmonization v0.1.0:

- Simplified adding new variables to the key file.

- Homogenization code is now soley dependent on information provided in the key, including variable properties and constraints, unit conversions, and homogenization settings.

- Removed dependency on Google Drive.

- Split code up into discrete functions to aid debugging and improvements.

Current state of code is pre-alpha (as of 12/22/2022). Please contact Derek Pierson if interested in using this code prior to the initial release. While functional, the code requires more robust testing and additional core functionality. Official alpha 1.0 release expected Spring 2023.


#### Further notes:

- Adding variable to the key is as simple as adding a new row and filling in the required information. A key_check and update feature will be added in the future to allow for QC of such key changes, updating of the master key, and key field requirments/locks.

- Key file has been updated ('V3'). Future update will likely remove many of the less common var fields. Note: Key file 'V2' no longer works. Key file versioning will now be complicated by data dependent var additions. Additional updates will be needed to handle this. See comment above.

- Additional unit conversions may be added via a sheet in the key. Unit conversion are now only variable specific if specified.

- No longer dependent on Google Drive. Current version requires local directory input. Source/sync from Google Drive directory (or Box, etc.) may be added in the future. 

- By compartmentalizing the code into discrete functions, it's now easier to debug errors and update code. Also hope that this will ease the processs of adding further functionality.

- Quality control checks are performed and notes are created in the code, but they are not yet exported to a 'notes.PDF.' Coming soon! (...also hoping to add variable specific QC plots)

- With further cunctionalization and removal of dependencies on Google Drive, the homogenization code runs much faster. Thus, re-homgenizing a large set of files is a breeze. 

- Key file now contains three alignment variables. A future code update is planned to test data alignment across 'HMGZD' files. Expected that such functionality will work well for data from a single project (e.g. align by IGSN and sample date for the CZN GeoMicrobiology project), but will require more thought, code functionality and scrutiny to be implimented across separate projects (e.g. separate datasets from an LTER site, ...original SoDaH database). 
 
### Acknowledgements
Work made possible by contributions and support from Steven Earl and Will Wieder (co-authors of the [soilHarmonization](https://lter.github.io/soilHarmonization/index.html) package and the [SoDaH database](https://lter.github.io/som-website/)), the [National Center for Ecological Analysis and Synthesis](https://www.nceas.ucsb.edu/) (NCEAS grant supported initial soilHarmonization code development), Kate Lajtha and Kathleen Lohse (project and funding support), and the Critical Zone Network Geomicrobiology project and the USDA Forest Service Rocky Mountain Research Station (project support). 
 
---

## Code Overview

The harmonization code is intentionally made up of a series of functions, designed to follow in sequence. The discrete functions aid and help limit errors when for adjusting or adding to the framework functionality, while also improving issues with debugging and code comprehension.

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



---
<a href="https://www.flaticon.com/free-icons/sheet" title="sheet icons">Sheet icons created by Freepik - Flaticon</a>

