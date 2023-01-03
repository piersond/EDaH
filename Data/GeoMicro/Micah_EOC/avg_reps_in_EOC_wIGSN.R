library(dplyr)

setwd("C:/github/EDaH/Data/Geomicro/Micah_EOC")

eoc <- read.csv("geomicro_EOC_wIGSN.csv", as.is=T)

eoc_avg <- eoc %>% group_by(short.id, IGSN, Location, Site, Position, 
                            id, year, month, top_depth_cm, bottom_depth_cm) %>% 
            summarize(ppm_C = mean(ppm.c),
                      eoc_dry_mass = mean(eoc.dry.mass),
                      c_per_g_soil = mean(c.per.g.soil),
                      eoc_method = "hot water",
                      ppm_C_sd = sd(ppm.c),
                      eoc_dry_mass_sd = sd(eoc.dry.mass),
                      c_per_g_soil_sd = sd(c.per.g.soil)) 

write.csv(eoc_avg, "geomicro_EOC_wIGSN_avg.csv")
