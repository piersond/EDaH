library(dplyr)
library(readxl)
library(tidyr)

### Script to add IGSN numbers to fracsymes data
# --> Format IGSN data to match fracsymes formats for sample site, pit, date and depth

setwd("C:/github/EDaH/Data/GeoMicro/")

fracs <- read_xlsx("Kitty_dfracs/geomicroLFiso.xlsx")
IGSN <- read_xlsx("IGSN/IGSN number_IECZM_426_1669919052.xlsx", skip=1)

# Add top depth column to fracs
fracs <- fracs %>% separate(`Depth _nterval`, into = c("top_depth_cm", "bottom_depth_cm"), sep = "-")
fracs$top_depth_cm <- as.numeric(fracs$top_depth_cm)
fracs$bottom_depth_cm <- as.numeric(fracs$bottom_depth_cm)

# Add order column to fracs data for tracking/merging later
fracs$order <- seq(1,nrow(fracs), 1)

#unique site codes
locs <- unique(fracs$Location)

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
fracs$Collection.date <- as.POSIXlt(as.character(fracs$Collection.date), format = '%Y-%m-%d')

# Add date columns with only month-year
IGSN$date_mY <- format(IGSN$sample_date, format='%m-%Y')
fracs$date_mY <- format(fracs$Collection.date, format='%m-%Y')

# Create IGSN dataframe to join to fracsyme data, then match column names with fracs df
IGSN_join <- IGSN %>% select(IGSN, date_mY, loc_code, site_code, landscape_code, `Depth in Core (min)`)
colnames(IGSN_join) <- c("IGSN", "date_mY", "Location", "Site", "Position", "top_depth_cm")

# Join data
IGSN_fracs <- left_join(fracs, IGSN_join, by=c("date_mY", "Location", "Site", "Position", "top_depth_cm"))

# save modified fracs data
#write.csv(IGSN_fracs, "Kitty_dfracs/geomicroLFiso_wIGSN.csv", row.names=F)



