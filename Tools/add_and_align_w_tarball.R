library(dplyr)
library(readxl)

### 1) NEED TO FIX DATE FORMATS! SET TO SOMETHING EXCEL WONT CHANGE!!!
### 2) ALIGN NEEDS TO BE OPTIONAL, SUCH THAT USERS CAN PRODUCE A COMPLETE DATABASE AND AN ALIGNED DATABASE
### 3) This script also needs a log and notes output

setwd("C:/GitHub/EDaH/Data/GeoMicro")


# Required inputs for ftn
#-----------------------------------------------------------------------------------
data_add <- read.csv("Kitty_microbiomass/HMGZD_data_output_12-30-2022.csv", as.is=T)
#tarball <- read.csv("IGSN/HMGZD_data_output_12-28-2022.csv", as.is=T) #Use the IGSN spreadsheet for first tarball
tarball <- readRDS("GeoMicro_aligned_tarball_2022-12-30.rds")
key_filepath <- "Kitty_microbiomass/geomicro_mb_Key_V3.xlsx"


# Pull alignment instructions from key file
#-----------------------------------------------------------------------------------
locKey <- read_xlsx(key_filepath, sheet = "Location_data")
profKey <- read_xlsx(key_filepath, sheet = "Profile_data")

# Set alignment variables
data_add_ID <- unique(data_add$dataset_id)
join_by <- c(unique(data_add$align_1), unique(data_add$align_2), unique(data_add$align_2)) 
align_how <- locKey %>% select(var, align_how) %>% rbind(profKey %>% select(var, align_how))
cols_not_ignore <- align_how %>% filter(align_how != "ignore" | var %in% join_by)

#Find align value to use
for(i in 1:1000) { #NOT 100% FUTURE PROOF, BUT SAFER THAN WHILE LOOP
  if(any(grepl(paste0("_ALIGN",i), colnames(tarball)))) {
    #print(paste0(paste0("_ALIGN",i , " found")))
    next
  } else {
    align_name <- paste0("_ALIGN",i)
    print(paste0("Align ID set to: ", align_name))
    break
  }
}

# Add alignment ID column for traceback
tarball[[paste0(gsub("_", "",align_name), "_ID")]] <- gsub("_", "", data_add_ID)


# Full join data
#------------------------------------------------------------------------------------
join_df <- full_join(tarball, data_add %>% select(one_of(cols_not_ignore$var)),
                     by=join_by,
                     suffix=c("",align_name))

# Verify columns match as specified, then if matched, remove dup column
#-----------------------------------------------------------------------------------
cols_to_match <- cols_not_ignore %>% filter(align_how == "match")

for(i in 1:nrow(cols_to_match)) {
  if(all(join_df[[cols_to_match$var[i]]] == join_df[[paste0(cols_to_match$var[i], align_name)]], na.rm = T)) {
    join_df <- join_df %>% select(-one_of(paste0(cols_to_match$var[i], align_name)))
    print(paste0("Matched with ", cols_to_match$var[i]))
  } else {
    print(paste0("ERROR: ", cols_to_match$var[i], " columns do not match"))
  }
}


### FIX: Add functionality for other alignment options
#------------------------------------------------------------------------------------
#replace
#combine
#use_avg


# Check that remaining duplicate columns are specified to be such in key
#------------------------------------------------------------------------------------


dup_cols <- colnames(join_df)[grepl(align_name, colnames(join_df))]
cols_to_add_dup <- cols_not_ignore %>% filter(align_how == "add_dup") %>% select(var)
extra_dups <- setdiff(gsub(align_name, "", dup_cols), cols_to_add_dup$var)

if(length(extra_dups) > 0) {
  print("ERROR: Unexpected duplicate var columns")
  print(paste0(extra_dups, align_name))
} else {

  # Export aligned tarball
  #------------------------------------------------------------------------------------
  saveRDS(join_df, paste0("GeoMicro_aligned_tarball_", Sys.Date(), ".rds"))
  print("Process complete! Data aligned and exported.")
}



#DEBUG
#dfx <- join_df %>% select(IGSN, layer_bot, layer_bot_ALIGN2)
#dfx$check <- ifelse(dfx$layer_bot == dfx$layer_bot_ALIGN2, "GOOD", "BAD")

#dfz <- join_df %>% filter(!is.na(lf_low_cut))

