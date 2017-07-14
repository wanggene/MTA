library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(googleVis)

# load mta data df1

load('mta2010_gathered.Rda')
df = df1

# remove row names

# create variable with colnames as choice
#station = unique(df1$fare_type)

# create variabel for time period
fare_type = unique(df$fare_type)
