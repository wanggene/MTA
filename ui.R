library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(googleVis)

shinyUI(dashboardPage(
    
# Dashboard Side Bar ---------------------------------
dashboardHeader(title = "MTA Fare Card History", 
                titleWidth = 300),

dashboardSidebar(
    sidebarUserPanel(h4("NYC DSA"),
    image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    width = 300,
    sidebarMenu(
           
            #1 Total count trend by period, group by period, count is the sum and mean
            #tags$hr(style="border-color: black;"), 
            menuItem("Overview", tabName = "swipe_count", icon = icon("bar-chart")),

            #2 select fare type and train station
            tags$hr(style="border-color: black;"),
            menuItem("Station ~ Fare Type", tabName = "fare_type", icon = icon("subway")),

            menuItem("Filter", tabName = "Options", icon = icon("filter"),
                     radioButtons("period", label='', choices = periods, inline = TRUE),
                     selectizeInput(inputId="station", h6("Top 30 Busiest Stations"), station, 
                                    selected = '42ND STREET & GRAND CENTRAL'),            
                     selectizeInput(inputId="fare_type", h6("Fare Types (ranking by swipe count)"), fare_type,
                                    selected = 'Full Fare'), selected = 'fare_type'

            ),
            
            #3 time line chart
            tags$hr(style="border-color: black;"),
            menuItem("Timeline of Fare Swipe (weekly)", tabName = "timeline", icon = icon("line-chart")),
            checkboxInput(inputId="station_timeline", "Chooe Station" , TRUE),
            checkboxInput(inputId = "type_timeline", "Choose Fare Type",  TRUE),


            #4 Raw data table
            tags$hr(style="border-color: black;"),
            menuItem("Explore Raw Data", tabName = "data", icon = icon("database")) #5

    )

),
   
# Dashboard Body ------------------------------------------
dashboardBody(
    tags$head(tags$style(HTML(
    '.main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 20px;
    }'))),
    
    
    
    
    # Head
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
        
    # Tabs
    tabItems(
        tabItem(tabName = "swipe_count",
            fluidRow(valueBoxOutput("overview_total_swipe", width = 6),
                valueBoxOutput("overview_top_station", width = 3),
                valueBoxOutput("overview_top_fare_type", width = 3)),
          
        
            fluidRow(box(htmlOutput("ggv_sum"),width=6, height = 350),
                 box(htmlOutput( "ggv_mean"), width=6, height = 350),
                 box(htmlOutput("ggv_stat_seq"),width=6, height = 350),
                 box(htmlOutput("ggv_type_seq"), width=6, height =350))
        ),   

        tabItem(tabName = "fare_type",
            fluidRow(valueBoxOutput("timeline_station1", width = 6),
                     valueBoxOutput("timeline_fare_type1", width = 3),
                     valueBoxOutput("timeline_total_swipe1", width = 3)),
            
            fluidRow(box(htmlOutput("ggv_sum2"),width=6, height = 350),
                     box(htmlOutput("ggv_sum_station"),width = 6, height = 350),
                     box(htmlOutput("ggv_sum_type"), width = 6, height = 350),
                     box(htmlOutput("ggv_sum_type_station"), width = 6, height = 350))
        ),
             
        tabItem(tabName = "timeline",
            fluidRow(valueBoxOutput("timeline_station2", width = 6),
                     valueBoxOutput("timeline_fare_type2", width = 3),
                     valueBoxOutput("timeline_total_swipe2", width = 3)),
            
            fluidRow(htmlOutput("ggv_timeline"), width=10, height=700)


        ),
            
        #tabItem(tabName = "database")#,
            # numericInput("maxrows", "Rows to show", 25),
            # verbatimTextOutput("rawtable")
        
        tabItem(tabName = "data",
                fluidRow(box(DT::dataTableOutput("table"), width = 12, height = 700)))
        
           
        )
        
  )
 )
)
