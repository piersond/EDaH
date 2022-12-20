# Soil data harmonization

The harmonization code is split up into a series of easily managed functions.

---

## Example usage

```
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
```

---

<br>

## Functions

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




