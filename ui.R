library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(googleVis)

shinyUI(dashboardPage(
    
# Dashboard Side Bar ---------------------------------
dashboardHeader(title = "MTA FARE"),

dashboardSidebar(
    
    
    sidebarUserPanel(h5("NYC DSA"),
    image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
   

    sidebarMenu(

           
            #1 Total count trend by period, group by period, count is the sum and mean, go to 
            tags$hr(style="border-color: black;"), 
            menuItem("Select Time Period", tabName = "swipe_count", icon = icon("bar-chart")), 
            radioButtons("period", label='', choices = period, inline = TRUE),
            
            #2 select fare type and train station
            tags$hr(style="border-color: black;"),
            menuItem("Select Fare Type And Station", tabName = "fare_type", icon = icon("subway")),
            selectInput("fare_type", h6("Fare Type"), fare_type),
            selectInput("station", h6("Station"), station),
            
            tags$hr(style="border-color: black;"),
            menuItem("Weekly Timeline", tabName = "timeline", icon = icon("line-chart")), #4
            # Input: Checkbox if check each station
            checkboxInput("Check Statioin", "Station", TRUE),
            tags$hr(style="border-color: black;"),
            
            menuItem("Explore Raw Data", tabName = "database", icon = icon("database")) #5


    )

),
   
# Dashboard Body ------------------------------------------
dashboardBody(
    # Head
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
        
    # Tabs
    tabItems(
        tabItem(tabName = "swipe_count",
            fluidRow(
                    valueBoxOutput("period_max"),
                    valueBoxOutput("period_rank_1"),
                    valueBoxOutput("period_mean")
                    
                ),
            fluidRow(box(htmlOutput("ggv_sum"), width=6, height = 400), 
                     box(htmlOutput("ggv_mean"), width=6, height = 400)
                    )),

        tabItem(tabName = "fare_type",
            fluidRow(
                    valueBoxOutput("top_1_station", width= 6),
                    valueBoxOutput("top_1_year", width =2),
                    valueBoxOutput("top_1_month",width =2),
                    valueBoxOutput("top_1_count",width =2)
                ),  
                
            fluidRow(box(htmlOutput("ggv_sum_type"), width=6, height=400 ),
                     box(htmlOutput("ggv_sum_type_station"), width=6, height=400)
                    )),
             
        tabItem(tabName = "timeline",
            fluidRow(htmlOutput("ggv_timeline"))   #, width=10, height=400)    
                    ),
            
        tabItem(tabName = "database")
            )
        
        )
    )
)
