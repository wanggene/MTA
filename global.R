library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(googleVis)
library(shinydashboard)

# load mta data df1

load('mta2010_gathered.Rda')
df = df1 %>% filter(To.Date > '2010-06-10' ) %>%
    filter(To.Date < '2017-01-01')



# create variable with colnames as choice

# time period
period = list("year", "month")



# fare type
#fare_type = sort(unique(df$fare_type))
fare_type = df %>% filter(year != 2017 & year != 2010) %>% 
    group_by(fare_type) %>% summarise(swipe_count = sum(fare_swipe)/1e6) %>% 
    arrange(desc(swipe_count)) %>% select(fare_type) # %>% head(10) 


# station name
#station = sort(unique(df$Station))
station = df %>% filter(year != 2017 & year != 2010) %>% 
    group_by(Station) %>% summarise(swipe_count = sum(fare_swipe)/1e6) %>% 
    arrange(desc(swipe_count)) %>% select(Station) %>% head(20)
