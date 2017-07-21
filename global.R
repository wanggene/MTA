#library(shiny)
library(DT)
library(dplyr)
library(googleVis)
library(shinydashboard)

# load mta data df1
load('./mta2011_gathered.Rda')
#df = df1 %>% filter(!year %in% c(2010, 2017))
    
    # filter(To.Date > '2010-06-10' ) %>%
    # filter(To.Date < '2017-01-01') 



# create variable with colnames as choice

# time period
periods = list("year", "month")

units = list("count", "percentile")

# fare type
fare_types = length(unique(df$fare_type))
fare_type = df %>% 
    group_by(fare_type) %>% summarise(swipe_count = sum(fare_swipe)/1e6) %>% 
    arrange(desc(swipe_count)) %>% select(fare_type) # %>% head(15) 


# station name
stations = length(unique(df$Station))
station = df %>%
    group_by(Station) %>% summarise(swipe_count = sum(fare_swipe)/1e6) %>% 
    arrange(desc(swipe_count)) %>% select(Station) %>% head(30)
