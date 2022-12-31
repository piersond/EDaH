library(dplyr)
library(plotly)
library(ggplot2)

setwd("C:/GitHub/RC_shiny")

sensor_files <- list.files("./data/Sensors")

sensor_filenames <- gsub(".dat", "", sensor_files)

data <- read.table(paste0(getwd(),"./data/Sensors/",sensor_files[1]), sep=",", skip=1, fill=T, header=T, na.strings = c("NAN", "NA"))

data <- data %>% distinct()

data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS", optional=T)
data <- data %>% filter(!is.na(TIMESTAMP)) %>% arrange(TIMESTAMP)
data <- data %>% mutate_at(c(2:ncol(data)), as.numeric)

str(data)

fig <- plot_ly(x = ~data$TIMESTAMP, y = ~data$CO2_1_Avg, type = 'scatter', mode = 'lines+markers', name = 'CO2_1_Avg', 
               fill="tozeroy", fillcolor='rgba(26,150,65,0.5)',
               line = list(color = 'rgba(255,11,65,0.7)', width = 2),
               marker = list(color = 'rgba(26,150,65,0.7)', size = 3))
fig <- fig %>% add_trace(x = ~data$TIMESTAMP, y = ~data$CO2_2_Avg, name = 'CO2_2_Avg', 
                         fill="tozeroy", fillcolor='rgba(16,110,25,0.5)', 
                         line = list(color = 'rgba(255,11,25,0.7)', width = 2),
                         marker = list(color = 'rgba(16,110,25,0.7)', size = 3))
fig <- fig %>% add_trace(x = ~data$TIMESTAMP, y = ~data$CO2_3_Avg, name = 'CO2_3_Avg', 
                         fill="tozeroy", fillcolor='rgba(8,50,5,0.5)', 
                         line = list(color = 'rgba(255,11,5,0.7)', width = 2),
                         marker = list(color = 'rgba(8,50,5,0.7)', size = 3))
fig <- fig %>% layout(xaxis = list(title = 'Date',
                                   rangeslider = list(type = "date")),
                      yaxis = list(title = 'Y-Axis Label'))

fig

