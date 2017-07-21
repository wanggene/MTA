#server.R
#library(shiny)
library(dplyr)
library(shinydashboard)
library(googleVis)
function(input, output) {
    
################################ Reactive ##################################
    
#################### -----------ggv1_sum : total swipe count in each time period 
##                              From 2011 to 2016                          ----------
    
    g_sum = reactive({df %>% 
            group_by(year) %>%
            dplyr::summarise(swipe_count = round(sum(fare_swipe)/ 1e6,1))
        })

    
    ################--------------ggv1_mean : average swipe count in each time period 
    g_mean = reactive({ df %>% 
            group_by(month) %>% 
            dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6,1))
        })
    
    ############# ---------------ggv1_top station: find the top station in the year
    g_stat_seq = reactive({ g_stat_seq = df %>% 
        group_by(Station) %>% 
        summarise(swipe_count = sum(fare_swipe)/ (6 *1e6)) %>% 
        arrange(desc(swipe_count)) %>% head(10)
    
    # reorder Station based on count value    
        g_stat_seq$Station = factor(g_stat_seq$Station, levels = g_stat_seq$Station[order(g_stat_seq$swipe_count)])
        g_stat_seq
    
    })
    
    ##########---------------- moste common fare type
    g_type_seq = reactive({ g_type_seq = df %>% 
            group_by(fare_type) %>% 
            summarise(swipe_count = sum(fare_swipe)/ (6 * 1e6)) %>% 
            arrange(desc(swipe_count)) %>% head(5)
        
        # reorder type based on count value    
        g_type_seq$fare_type = factor(g_type_seq$fare_type, levels = g_type_seq$fare_type[order(g_type_seq$swipe_count)])
        g_type_seq
        
    })

    ###----------- ggv1 plots 
    
    #show timeline of fare count by period group by period, sum or mean of fare swipe count
    #total count by year
    output$ggv_sum = renderGvis({
        gvisColumnChart(g_sum(),
                        xvar = 'year',
                        yvar = 'swipe_count',
                        options=list(
                            #width = 4,
                            height= 300,
                            legend='none',
                            title="Total MTA Fare Card Swipe Number",
                            hAxis="{title:'Year'}",
                            vAxis="{title:'Count (Million)', minValue:0, maxValue: 'auto' }"
                            ))

    })
    
    
    # total count by month
    output$ggv_mean = renderGvis({
        gvisColumnChart(g_mean(), 
                        xvar = 'month', 
                        yvar = 'swipe_count', 
                        options=list(
                            #width = 4,
                            height= 300,
                            legend='none',
                            title="Avearage MTA Fare Card Swipe Number", 
                            #vAxis="{title:'Count'}",
                            hAxis="{title:'Month'}",
                            vAxis="{minValue:0 , maxValue:'auto', title:'Count (Million)'}"
                        ))
    }) 
    
    
   # find the top 10 station
    output$ggv_stat_seq = renderGvis({
        gvisBarChart(g_stat_seq(),
                        xvar = 'Station',
                        yvar = 'swipe_count',
                        options=list(
                            height= 300,
                            #width = 4,
                            legend='none',
                            title="Top 10 Busiest MTA Subway Stations",
                            hAxis="{title:'Average Annual Swipe Count (Million)'}"
                            #vAxis="{title:'Station'}
                            
                        ))
    })

    #####
    output$ggv_type_seq = renderGvis({
        gvisBarChart(g_type_seq(),
                     xvar = 'fare_type',
                     yvar = 'swipe_count',
                     options=list(
                         height= 300,
                         #width = 4,
                         legend='none',
                         title="Most Commonly Used MTA Fare Type",
                         hAxis="{title:'Average Annual Swipe Count (Million)'}",
                         vAxis="{title:'Fare Type'}"
                     ))
    })

       
 #####################-------ggv2: fare type  ------------------   
    #      Now let's look at different fare type ----------------
    #      only count the sum of fare swipe  ---------------
    
    g_sum2 = reactive({  df %>% filter(!year %in% c(2010, 2017)) %>% group_by_(input$period) %>%    
            dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6, 1)) 
    })
    
    g_sum_station = reactive({ df %>% filter(!year %in% c(2010, 2017)) %>% filter(Station == input$station) %>%
            group_by_(input$period) %>% dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6,1))
    })
    
    
    g_sum_type = reactive({ df %>% filter(!year %in% c(2010, 2017)) %>% filter(fare_type == input$fare_type) %>%
            group_by_(input$period) %>% dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6,1))
    })
    
    #      Now let's look at different station ----- only count the sum of fare swipe  ---------------
    
    g_sum_type_station = reactive({ df %>% filter(!year %in% c(2010, 2017)) %>% 
            filter(fare_type == input$fare_type) %>% filter(Station == input$station) %>% 
            group_by_(input$period) %>% dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6, 1))
    })
    
#---------- ggv2
    output$ggv_sum2 = renderGvis({
        gvisColumnChart(g_sum2(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            #width = 4,
                            height= 300,
                            legend='none',
                            title="Total MTA Fare Card Swipe Number", 
                            vAxis="{title:'Count (Million)', minValue:0, maxValue: 'auto' }"
                            
                        ))
        
    }) 
    
    output$ggv_sum_station = renderGvis({
        gvisColumnChart(g_sum_station(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title= input$station , 
                            vAxis="{title:'Count (Million)', minValue: 0, maxValue: 'auto' }" ) )#,
                            #hAxis="{title: input$period }")
    
    }) 
    
    output$ggv_sum_type = renderGvis({
        gvisColumnChart(g_sum_type(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title= input$fare_type, 
                            vAxis="{title:'Count (Million)', minValue:0, maxValue: 'auto' }" ) ) # ,
                            #hAxis="{title:'height (in)'}"))
        
    }) 
    
    
    output$ggv_sum_type_station = renderGvis({
        gvisColumnChart(g_sum_type_station(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title="Total Swipe Count Using Selected Fare Type in Selected Station", 
                            vAxis="{title:'Count (Million)', minValue:0, maxValue: 'auto' }" ) ) # ,
                            #hAxis="{title:'height (in)'}"))
        
    }) 
    
    
    
#####################-------ggv4: Time Line  ------------------   
    #      Now let's look at time line     --------------
    #                 for different station ---------------
    #      only count the sum of fare swipe  ---------------   
    
    
    # g_timeline = reactive({ df %>%  
    #         filter(fare_type == input$fare_type) %>%
    #         filter(Station == input$station) %>%
    #         group_by(To.Date) %>% 
    #         dplyr::summarise(swipe_count = sum(fare_swipe))
    # })
    
    
    g_timeline_both = reactive({ df %>%  
            filter(fare_type == input$fare_type) %>%
            filter(Station == input$station) %>%
            group_by(To.Date) %>% 
            dplyr::summarise(swipe_count = sum(fare_swipe))
    })
    
    g_timeline_type = reactive({ df %>%  
            filter(fare_type == input$fare_type) %>%
            group_by(To.Date) %>% 
            dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6,1))
    })
    
    g_timeline_station = reactive({ df %>%  
            filter(Station == input$station) %>%
            group_by(To.Date) %>% 
            dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6,1))
    })
    
    g_timeline_none = reactive({ df %>%  
            group_by(To.Date) %>% 
            dplyr::summarise(swipe_count = round(sum(fare_swipe)/1e6,1))
    })

    
#---------ggvs4 ------------time line
    
    output$ggv_timeline = renderGvis({
        gvisAnnotationChart( if (input$type_timeline  && input$station_timeline ) {g_timeline_both()}
                             else if (input$station_timeline ) { g_timeline_station()}
                             else if (input$type_timeline) {g_timeline_type() }
                            else { g_timeline_none() },
                            
                            datevar="To.Date",
                            numvar="swipe_count",
                            
                            options=list(
                                title= input$station,
                                width= '95%',
                                height= 500)
                                 
        )
        
    }) 
    
#----------valueBox------------------------------------------------
    
    output$overview_total_swipe <- renderValueBox({
        valueBox( value = tags$p(round(g_sum()$swipe_count %>% mean()/1e3, 1) , style = "font-size: 60%;"), 
                  subtitle = tags$p("2010 ~2016 MTA Annual Fare Swipes (billiion) ", style = "font-size: 150%;"), 
                  icon = icon("star"))
    }) 
    
    output$overview_top_station <- renderValueBox({
        valueBox(value = tags$p(stations, style = "font-size: 60%;"), 
                 subtitle = tags$p("Subway Stations",  style = "font-size: 150%;"),
                 icon = icon("star"))
    }) 
    
    output$overview_top_fare_type <- renderValueBox({
        valueBox( value = tags$p(fare_types, style = "font-size: 60%;"), 
                  subtitle = tags$p("Active Fare Types",  style = "font-size: 150%;"),
                  icon = icon("star"))
    })   
    
    
    
    
    
# ------------------------------------------------------------------    
    
    output$timeline_station1 <- renderValueBox({
         valueBox( value = tags$p(input$station, style = "font-size: 60%;"), 
                   subtitle = tags$p("Station", style = "font-size: 150%;"), 
                   icon = icon("star"))
        }) 
    
    output$timeline_fare_type1 <- renderValueBox({
        valueBox(value = tags$p(input$fare_type, style = "font-size: 60%;"), 
                  subtitle = tags$p("Fare Type",  style = "font-size: 150%;"),
                  icon = icon("star"))
        }) 
    
    output$timeline_total_swipe1 <- renderValueBox({
        valueBox( value = tags$p(round(g_timeline_both()$swipe_count/6,0), style = "font-size: 60%;"), 
                  subtitle = tags$p("Annual Swipe Count",  style = "font-size: 150%;"),
                  icon = icon("star"))
    }) 
    
 #------------------------   
    
    output$timeline_station2 <- renderValueBox({
        valueBox( value = tags$p(input$station, style = "font-size: 60%;"), 
                  subtitle = tags$p("Station", style = "font-size: 150%;"), 
                  icon = icon("star"))
    }) 
    
    output$timeline_fare_type2 <- renderValueBox({
        valueBox(value = tags$p(input$fare_type, style = "font-size: 60%;"), 
                 subtitle = tags$p("Fare Type",  style = "font-size: 150%;"),
                 icon = icon("star"))
    }) 
    
    output$timeline_total_swipe2 <- renderValueBox({
        valueBox( value = tags$p(round(g_timeline_both()$swipe_count/6,0), style = "font-size: 60%;"), 
                  subtitle = tags$p("Annual Swipe Count",  style = "font-size: 150%;"),
                  icon = icon("star"))
    }) 
    


# ---------------ggv5  tab database
    
    output$table <- DT::renderDataTable({
        datatable(df, rownames=FALSE) %>%
            formatStyle(input$selected, background="skyblue", fontWeight='normal')
    })


} #1