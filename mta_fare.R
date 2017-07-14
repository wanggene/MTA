# Fare Card History for MTA
library(tidyr)
library(dplyr)
library(ggplot2)

# load the data 
mta_fare = read.csv('mta_fare.csv', stringsAsFactors = F)
df = mta_fare
dim(df)
#colnames(df)


#----------------------------------------------------------------
#mta_station = read.csv('mta_station.csv', stringsAsFactors = F)
#df2 = mta_station


##################################################################
#####################  clean the mta_fare data ############################
# gather the columns of different fare type

df1 = gather(df, key = fare_type, value = fare_swipe, c(-1,-2,-3,-4))

# filter out fare type in list
no_type = c( "X14.Day.Reduced.Fare.Media.Unlimited",    
             "X1.Day.Unlimited",                            
             "X14.Day.Unlimited")

df1 = df1 %>% filter(!fare_type %in% no_type) 

# Trim the white space in data
df1$Station = trimws(df$Station)
df1$Remote.Station.ID = trimws(df$Remote.Station.ID)

# check the Station Info
length(unique(df1$Remote.Station.ID))
length(unique(df1$Station))

# change the data type
df1$From.Date = as.Date(df$From.Date, '%m/%d/%Y')
df1$To.Date = as.Date(df$To.Date, '%m/%d/%Y')

# add new columns: month, year
df1$month = strftime(df1$From.Date, "%m")
df1$month = as.factor(df1$month)

df1$year = strftime(df1$From.Date, "%Y")
df1$year = as.factor(df1$year)

df1$week = strftime(df1$From.Date, "%W")
df1$week = as.factor(df1$week)

save(df1, file='mta2010_gathered.Rda')

# there are some station has two StationID
two_stationID = df1 %>% select(Remote.Station.ID, Station) %>% 
    group_by(Remote.Station.ID, Station) %>%
    dplyr::summarise(Num_ID = n()) %>% 
    group_by(Station) %>%
    dplyr::summarise(Stat_ID = n()) %>%
    filter (Stat_ID > 1)

# check what station ID in previous list
df1 %>% group_by(Station, Remote.Station.ID) %>% 
    filter(Station %in% two_stationID$Station) %>% 
    dplyr::summarise(num=n()) %>% 
    select(Remote.Station.ID, Station, num) %>%
    tail(20)




#############################################################
######################    Look at the data    ###############
#load('mta2010_gathered.Rda')
load('mta2010_cleaned.Rda')
head(df1)
# Q1. what is the total full fare by year, month, week

## Total fare swipe by year

g1 = df1 %>% filter(!year %in% c(2010, 2017)) %>%
    group_by(year) %>% 
    summarise(totalfullfare_ym = mean(fare_swipe)) %>% 
    ggplot(aes(x=year, y = totalfullfare_ym))

g1 + geom_bar( stat='identity', show.legend = F)

#g1 + geom_bar(aes(x=year, fill=year), stat='identity', show.legend = F)


## Total fare swipe by month

g2 = df1 %>% filter(To.Date > '2010-06-10') %>%
    group_by(month) %>% 
    summarise(totalfullfare_ym = mean(fare_swipe)) %>% 
    ggplot(aes(x=month, y = totalfullfare_ym))

g2 + geom_bar(aes(x=month, fill=month), stat='identity', show.legend = F)


## Total fare swipe by week

g3 = df1 %>% filter(To.Date > '2010-06-10') %>%
    group_by(week) %>% 
    summarise(totalfullfare_ym = mean(fare_swipe)) %>% 
    ggplot(aes(x=week, y = totalfullfare_ym))

g3 + geom_bar(aes(x=week), stat='identity', show.legend = F)




## Total fare swipe by month
g = df1 %>% filter(To.Date > '2010-06-10') %>%
    group_by(year, month) %>% 
    summarise(totalfullfare_ym = sum(fare_swipe)) %>% 
    ggplot(aes(x=month, y = totalfullfare_ym))

g + geom_line(aes(x=month, group=year)) +
    facet_grid(. ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text = element_text(size=9))

## Total fare swipe by week
g = df1 %>% filter(To.Date > '2010-06-10') %>%
    group_by(year, week) %>%
    summarise(totalfullfare_ym = sum(fare_swipe)) %>% 
    ggplot(aes(x = From.Date, y = totalfullfare_ym))

g + geom_line(aes(x=week, group=year)) +
    facet_grid(. ~ year) +
    #facet_wrap(. ~ year, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text = element_text(size=9))

# Q2. Total swipe by fare type and year

g = df1 %>% group_by(year,fare_type) %>% 
    summarise(totalfullfare_year = sum(fare_swipe)) %>%
    filter(year != 2017 & year != 2010) %>% 
    ggplot(aes(x=year, y = totalfullfare_year)) 

g + geom_point(aes(x=year)) +
    facet_wrap(~ fare_type  , nrow = 5, scales = "free_y" )


# Q3. what happed when MTA rise the fare,

g = df1 %>% filter(year != 2017 & year != 2010) %>% 
    group_by(month, year) %>% 
    summarise(totalfullfare_year = sum(fare_swipe)) %>%
    ggplot(aes(x=month, y= totalfullfare_year)) 

g + geom_line(aes(color = year, group=year)) + 
    geom_smooth(method = 'lm', se =F) +
    facet_grid( year ~ . )
    

# 











