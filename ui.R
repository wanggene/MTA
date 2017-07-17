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
            menuItem("Overview of Fare Swipe", tabName = "swipe_count", icon = icon("bar-chart")),
            
            #2 select fare type and train station
            tags$hr(style="border-color: black;"),
            menuItem("Station ~ Fare Type", tabName = "fare_type", icon = icon("subway")),
            radioButtons("period", label='', choices = period, inline = TRUE),
            selectizeInput(inputId="station", h6("Station"), station, 
                           selected = '42ND STREET & GRAND CENTRAL'),            
            selectizeInput(inputId="fare_type", h6("Fare Type"), fare_type,
                           selected = 'Full Fare'),
            
            
            tags$hr(style="border-color: black;"),
            menuItem("Weekly Timeline", tabName = "timeline", icon = icon("line-chart")), #4
            # Input: Checkbox if check each station
            # checkboxInput("Check Statioin", "Station", TRUE),

            
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
        
        fluidRow(box(htmlOutput("ggv_sum"),width=6, height = 350),
                 box(htmlOutput( "ggv_mean"), width=6, height = 350),
                 box(htmlOutput("ggv_stat_seq"),width=6, height = 350),
                 box(htmlOutput("ggv_type_seq"), width=6, height =350))
        ),   

        tabItem(tabName = "fare_type",
            fluidRow(box(htmlOutput("ggv_sum2"),width=6, height = 350),
                     box(htmlOutput("ggv_sum_station"),width = 6, height = 350),
                     box(htmlOutput("ggv_sum_type"), width = 6, height = 350),
                     box(htmlOutput("ggv_sum_type_station"), width = 6, height = 350))
        ),
             
        tabItem(tabName = "timeline",
            fluidRow(valueBoxOutput("timeline_station", width = 6),
                     valueBoxOutput("timeline_fare_type", width = 3),
                     valueBoxOutput("timeline_total_swipe", width = 3)),
            
            fluidRow(htmlOutput("ggv_timeline"), width=10, height=600)
        ),
            
        tabItem(tabName = "database")#,
            # numericInput("maxrows", "Rows to show", 25),
            # verbatimTextOutput("rawtable")
           
        )
        
  )
 )
)
