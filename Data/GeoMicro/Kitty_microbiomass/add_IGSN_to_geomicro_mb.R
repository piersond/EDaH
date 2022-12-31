library(dplyr)
library(readxl)
library(tidyr)

### Script to add IGSN numbers to mbymes data
# --> Format IGSN data to match mbymes formats for sample site, pit, date and depth

setwd("C:/github/EDaH/Data/GeoMicro/")

mb <- read_xlsx("Kitty_microbiomass/geomicro_mb.xlsx")
IGSN <- read_xlsx("IGSN/IGSN number_IECZM_426_1669919052.xlsx", skip=1)

# Add top depth column to mb
#mb <- mb %>% separate(`Depth _nterval`, into = c("top_depth_cm", "bottom_depth_cm"), sep = "-")
mb$top_depth_cm <- as.numeric(mb$top_depth_cm)
mb$bottom_depth_cm <- as.numeric(mb$bottom_depth_cm)

# Add order column to mb data for tracking/merging later
mb$order <- seq(1,nrow(mb), 1)

#unique site codes
locs <- unique(mb$Location)

# Loop through IGSN sample names and add '_' after location code
for(i in 1: length(locs)) {
  IGSN$`Sample Name` <- ifelse(grepl(locs[i], IGSN$`Sample Name`), gsub(locs[i], paste0(locs[i],"_"), IGSN$`Sample Name`), IGSN$`Sample Name`)
}

# Add loc and site code columsn from sample name
codes_df <- IGSN %>% select('Sample Name') %>%
              separate(`Sample Name`, c("A", "B", "C", "D", "E"), "_")

IGSN$loc_code <- codes_df$A
IGSN$site_code <- codes_df$B
IGSN$landscape_code <- codes_df$C

# Format date columns
IGSN$sample_date <- as.POSIXlt(IGSN$`Collection date`, format = '%Y-%m-%d')
mb$Collection.date <- as.POSIXlt(as.character(mb$Collection.date), format = '%Y-%m-%d')

# Add date columns with only month-year
IGSN$date_mY <- format(IGSN$sample_date, format='%m-%Y')
mb$date_mY <- format(mb$Collection.date, format='%m-%Y')

# Create IGSN dataframe to join to mbyme data, then match column names with mb df
IGSN_join <- IGSN %>% select(IGSN, date_mY, loc_code, site_code, landscape_code, `Depth in Core (min)`)
colnames(IGSN_join) <- c("IGSN", "date_mY", "Location", "Site", "Position", "top_depth_cm")

# Join data
IGSN_mb <- left_join(mb, IGSN_join, by=c("date_mY", "Location", "Site", "Position", "top_depth_cm"))

# save modified mb data
#write.csv(IGSN_mb, "Kitty_microbiomass/geomicro_mb_wIGSN.csv", row.names=F)



