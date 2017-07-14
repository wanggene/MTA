#server.R
library(shiny)
library(ggplot2)
library(dplyr)

function(input, output) {
    
    g1 = reactive({
        df %>% filter(To.Date > '2010-06-10') %>% 
            filter(!year %in% c(2010, 2017)) %>% filter(fare_type == input$fare_type)
    })
    
    g2 = reactive({
        df %>% filter(To.Date > '2010-06-10') %>% 
             filter(fare_type == input$fare_type)
    })
    
    
    
    # attach output object (id is TripDuration) to output
    output$totalswipe = renderPlot({
       g1() %>% group_by_(input$period) %>% 
            dplyr::summarise(totalfullfare_ym = sum(fare_swipe)) %>%
            ggplot(aes_string(x = input$period, y = 'totalfullfare_ym')) + 
            geom_bar( aes_string(fill = input$period), stat='identity', show.legend = F)
    })
    
    
    #output$TakeTrip = renderPlot({ })
    
    output$avgswipe = renderPlot({
        g2() %>% group_by_(input$period) %>% 
            dplyr::summarise(totalfullfare_ym = mean(fare_swipe)) %>%
            ggplot(aes_string(x = input$period, y = 'totalfullfare_ym')) + 
            geom_bar( aes_string(fill = input$period), stat='identity', show.legend = F)
    })
    
    
    
} 