library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(googleVis)

shinyUI(dashboardPage(skin = "black",
    
    # Dashboard Side Bar ---------------------------------
    dashboardHeader(title = "MTA FARE",
    titleWidth = 200),

    dashboardSidebar(
        
        sidebarUserPanel("NYC DSA",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        sidebarMenu(
            
            #1 Total count trend by period, group by period, count is the sum and mean, go to 
            menuItem("Fare Swipe Count", tabName = "swipe_count", icon = icon("bar-chart")),   #!!!!!!!! done
            
            # menuItem("Fare Type", tabName = "fare_type", icon = icon("ticket")), #!!!!!!!!!!!!!!!!!!!!
            
            #menuItem("Station", tabName = "station", icon = icon("subway")), #3
            
            menuItem("Time Line", tabName = "timeline", icon = icon("bar-chart")), #4
            
            menuItem("Raw Data", tabName = "database", icon = icon("table")) #5
            
        ),
        
        selectizeInput("period",
                       "By Time",
                       period),
        
        selectizeInput("fare_type",
                       "By Fare Type",
                       fare_type),
        
        selectizeInput("station",
                       "By Station",
                       station)
        
    ),
    
    # Dashboard Body ------------------------------------------
    dashboardBody(
        # extra 
        tags$head(tags$link(rel = "stylesheet", 
                            type = "text/css", 
                            href = "custom.css")),
        
        # Tabs
        tabItems(
            tabItem(tabName = "swipe_count",
                    fluidRow(box(htmlOutput("ggv_sum"), width=6, height = 400), 
                             box(htmlOutput("ggv_mean"), width=6, height = 400),
                             fluidRow(box(htmlOutput("ggv_sum_type"), width=12)),
                             fluidRow(box(htmlOutput("ggv_sum_type_station"), width=12))
                             
                             
                    ),
            
            
            # 
            # tabItem(tabName = "fare_type",
            #         fluidRow(box(htmlOutput("ggv_sum_type"), width=12))   
            #          ),
            # 
            # 
            # 
            # tabItem(tabName = "station",
            #         fluidRow(box(htmlOutput("ggv_sum_type_station"), width=12))
            #         ),

            tabItem(tabName = "timeline",
                    fluidRow(box(htmlOutput("ggv_timeline"), width=12))
                    ),
            

            tabItem(tabName = "database"))
        )

       ) 
    )
    
)