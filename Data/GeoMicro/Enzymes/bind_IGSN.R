library(dplyr)
library(readxl)

### Script to add IGSN numbers to enzymes data
# --> Format IGSN data to match enzymes formats for sample site, pit, date and depth

setwd("C:/github/EDaH/Data/GeoMicro")

enz <- read.csv("Enzymes/CZCN soil enzyme datasets.csv", as.is=T)
IGSN <- read_xlsx("IGSN/IGSN number_IECZM_426_1669919052.xlsx", skip=1)

# Add order column to enz data for tracking/merging later
enz$order <- seq(1,nrow(enz), 1)

### Go site by site and reformat Sample name to separate SITE code
#------------------------------------------------------------------------

#unique site codes
locs <- unique(enz$Location)

# Loop through IGSN sample names and add '_' after location code
for(i in 1: length(locs)) {
  IGSN$`Sample Name` <- ifelse(grepl(locs[i], IGSN$`Sample Name`), gsub(locs[i], paste0(locs[i],"_"), IGSN$`Sample Name`), IGSN$`Sample Name`)
}

# Add loc and site code columsn from sample name
codes_df <- t(as.data.frame(strsplit(IGSN$`Sample Name`, "_")))
IGSN$loc_code <- codes_df[,1]
IGSN$site_code <- codes_df[,2]
IGSN$landscape_code <- codes_df[,3]

# Format date columns
IGSN$sample_date <- as.POSIXlt(IGSN$`Collection date`, format = '%Y-%m-%d')
enz$Collection.date <- as.POSIXlt(as.character(enz$Collection.date), format = '%Y%m%d')

# Add date columns with only month-year
IGSN$date_mY <- format(IGSN$sample_date, format='%m-%Y')
enz$date_mY <- format(enz$Collection.date, format='%m-%Y')

# Create IGSN dataframe to join to enzyme data, then match column names with enz df
IGSN_join <- IGSN %>% select(IGSN, date_mY, loc_code, site_code, landscape_code, `Depth in Core (min)`)
colnames(IGSN_join) <- c("IGSN", "date_mY", "Location", "Site", "Landscape", "Upper.soil.depth..cm")

# Join data
IGSN_enz <- left_join(enz, IGSN_join, by=c("date_mY", "Location", "Site", "Landscape", "Upper.soil.depth..cm"))

# save modified enz data
write.csv(IGSN_enz, "Enzymes/CZCN soil enzyme datasets_wIGSN.csv", row.names=F)



