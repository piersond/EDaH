library(dplyr)
library(readxl)
library(tidyr)

### Script to add IGSN nueocers to eocymes data
# --> Format IGSN data to match eocymes formats for sample site, pit, date and depth

setwd("C:/github/EDaH/Data/GeoMicro/")

eoc <- read_xlsx("Micah_EOC/eoc_all.xlsx")
IGSN <- read_xlsx("IGSN/IGSN number_IECZM_426_1669919052.xlsx", skip=1)

# Add top depth column to eoc
#eoc <- eoc %>% separate(`Depth _nterval`, into = c("top_depth_cm", "bottom_depth_cm"), sep = "-")
eoc$top_depth_cm <- as.numeric(eoc$depth.start)
eoc$bottom_depth_cm <- as.numeric(eoc$depth.end)

# Add order column to eoc data for tracking/merging later
eoc$order <- seq(1,nrow(eoc), 1)

# Prep IGSN data
#-------------------------------------------------------------------------------
#unique site codes
locs <- c("CA", "LUQ", "R8", "RC", "SN" )

# Loop through IGSN sample names and add '_' after location code
for(i in 1: length(locs)) {
  IGSN$`Sample Name` <- ifelse(grepl(locs[i], IGSN$`Sample Name`), gsub(locs[i], paste0(locs[i],"_"), IGSN$`Sample Name`), IGSN$`Sample Name`)
}

# Add loc and site code columns from sample name
codes_df <- IGSN %>% select('Sample Name') %>%
              separate(`Sample Name`, c("A", "B", "C", "D", "E"), "_")

IGSN$loc_code <- codes_df$A
IGSN$site_code <- codes_df$B
IGSN$landscape_code <- codes_df$C

# Format date columns
IGSN$sample_date <- as.POSIXlt(IGSN$`Collection date`, format = '%Y-%m-%d')

#Split out pieces of the sample ID in EOC data
#-------------------------------------------------------------------------------
eoc_ID <- eoc %>% select(id) %>%
  separate(id, c("A", "B", "C"), "-")

eoc$Location <- eoc_ID$A
eoc$Site <- eoc_ID$B 
eoc$Position <- eoc_ID$C 

unique(eoc$Position)
unique(IGSN$landscape_code)


# Create a macthing month-year column in each dataset
#-------------------------------------------------------------------------------
# Add date columns with only month-year
IGSN$date_mY <- format(IGSN$sample_date, format='%m-%Y')
eoc$date_mY <- format(paste0(eoc$month, "-", eoc$year), format='%m-%Y')

# Create IGSN dataframe to join to eocyme data, then match column names with eoc df
IGSN_join <- IGSN %>% select(IGSN, date_mY, loc_code, site_code, landscape_code, `Depth in Core (min)`)
colnames(IGSN_join) <- c("IGSN", "date_mY", "Location", "Site", "Position", "top_depth_cm")

# Join data
IGSN_eoc <- left_join(eoc, IGSN_join, by=c("date_mY", "Location", "Site", "Position", "top_depth_cm"))

# save modified eoc data
write.csv(IGSN_eoc, "Micah_EOC/geomicro_EOC_wIGSN.csv", row.names=F)



