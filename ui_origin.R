# shiny app for citibike
library(shiny)
library(ggplot2)
library(dplyr)

fluidPage(
    titlePanel(h2("MTA fare history")),
    sidebarLayout(
        sidebarPanel(h4("Choose Bins"),
            
            radioButtons("period", "time period",
                list("year", "month", "week")),
              
            tags$hr(),
            
            selectInput(inputId = "fare_type", 
                label = "Fare Type", fare_type)
            
        ),
        mainPanel(
            tabsetPanel(type = "tabs", 
                tabPanel(h4("Total Swipe"), plotOutput("totalswipe")),
                
                tabPanel(h4("Average Swipe"), plotOutput("avgswipe")), 
                
                tabPanel(h4("Time"), plotOutput(""))
                
                
                
                )
        )
    )
)