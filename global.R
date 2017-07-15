library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(googleVis)

# load mta data df1

load('mta2010_gathered.Rda')
df = df1 %>% filter(To.Date > '2010-06-10')



# create variable with colnames as choice
# fare type
fare_type = unique(df$fare_type)

# time period
period = list("year", "month")

# station name
station = unique(df$Station)

