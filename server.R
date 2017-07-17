#server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(googleVis)
function(input, output) {
    
################################ Reactive ##################################
    
#################### -----------ggv1_sum : total swipe count in each time period 
##                              From 2011 to 2016                          ----------
    
    g_sum = reactive({   
        df %>% 
            filter(!year %in% c(2010, 2017)) %>% 
            group_by(year) %>%                                     # input$period
            dplyr::summarise(swipe_count = sum(fare_swipe)/1e6) 
        
        
    })
    
    ################--------------ggv1_mean : average swipe count in each time period 
    g_mean = reactive({   
        df %>% filter(!year %in% c(2010, 2017)) %>% 
            group_by(month) %>% 
            dplyr::summarise(swipe_count = mean(fare_swipe))
        
        
    })
    
    
    ############# ---------------ggv1_top station: find the top station in the year
    g_stat_seq = reactive({ 
        g_stat_seq = df %>% 
        filter(year != 2017 & year != 2010) %>% 
        group_by(Station) %>% 
        summarise(swipe_count = sum(fare_swipe)/1e6) %>% 
        arrange(desc(swipe_count)) %>% head(10)
    
    # reorder Station based on count value    
        g_stat_seq$Station = factor(g_stat_seq$Station, levels = g_stat_seq$Station[order(g_stat_seq$swipe_count)])
        g_stat_seq
    
    })
    
    ##########----------------
    g_type_seq = reactive({ 
        g_type_seq = df %>% 
            filter(year != 2017 & year != 2010) %>% 
            group_by(fare_type) %>% 
            summarise(swipe_count = sum(fare_swipe)/1e9) %>% 
            arrange(desc(swipe_count)) %>% head(10)
        
        # reorder Station based on count value    
        g_type_seq$fare_type = factor(g_type_seq$fare_type, levels = g_type_seq$fare_type[order(g_type_seq$swipe_count)])
        g_type_seq
        
    })
    
    
 
    ###----------- ggv1 plots 
    
    # show timeline of fare count by period group by period, sum or mean of fare swipe count !!!!!!!!! done
    # total count by year
    output$ggv_sum = renderGvis({
        gvisColumnChart(g_sum(), 
                        xvar = 'year', 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title="Total MTA Fare Card Swipe Number", 
                            vAxis="{title:'Count (Million)'}",
                            hAxis="{title:'Year'}"
                            ))
        
    }) 
    
    # total count by month
    output$ggv_mean = renderGvis({
        gvisColumnChart(g_mean(), 
                        xvar = 'month', 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title="Avearage MTA Fare Card Swipe Number", 
                            vAxis="{title:'Count'}",
                            hAxis="{title:'Month'}"
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
                            hAxis="{title:'Count (Million)'}" #,
                            #vAxis="{title:'Station'}"
                        ))
    })

    #####
    output$ggv_type_seq = renderGvis({
        gvisBarChart(g_type_seq(),
                     xvar = 'fare_type',
                     yvar = 'swipe_count',
                     options=list(
                         height= 300,
                         #weight = 6,
                         legend='none',
                         title="Most Commen MTA Subway Fare Type",
                         hAxis="{title:'Count (Billion)'}",
                         vAxis="{title:'Fare Type'}"
                     ))
    })
    
    
    
    ### -----------------valueBox -Tab ggv1-----------------------
    
    output$period_max <- renderValueBox({
        valueBox(
            value = max(g_sum()$swipe_count),
            subtitle = "Max counts (Million)",
            icon = icon("star")
        )
    })   
    
    output$period_mean <- renderValueBox({
        valueBox(
            value = round(mean(g_sum()$swipe_count),0),
            subtitle = "Average counts",
            icon = icon("star")
        )
    }) 
    
    
    output$period_rank_1 <- renderValueBox({
        valueBox(
            value = arrange(g_mean(), desc(swipe_count))[1, 'month'],
            subtitle = "Rank #1",
            icon = icon("star")
        )
    }) 
    
    
    

       
 #####################-------ggv2: fare type  ------------------   
    #      Now let's look at different fare type ----------------
    #      only count the sum of fare swipe  ---------------
    g_sum_type = reactive({
        df %>%
            filter(!year %in% c(2010, 2017)) %>%
            filter(fare_type == input$fare_type) %>%
            group_by_(input$period) %>%
            dplyr::summarise(swipe_count = sum(fare_swipe)/1e6)

    })
    
    g_sum_station = reactive({   
        df %>% 
            filter(!year %in% c(2010, 2017)) %>% 
            filter(Station == input$station) %>%
            group_by_(input$period) %>% 
            dplyr::summarise(swipe_count = sum(fare_swipe)/1e6)
        
    })
    
    
    
    # g_sum_station_top = reactive({   
    #     df %>% group_by( year, month, Station) %>% 
    #         summarise(Total_swipe = sum(fare_swipe)) %>% 
    #         arrange(desc(Total_swipe)) %>% head(1)
    # })
    
    
#---------- ggv2
    
    output$ggv_sum_station = renderGvis({
        gvisColumnChart(g_sum_station(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title= input$station , 
                            vAxis="{title:'Count (Million)'}" ) ) # ,
                            #hAxis="{title:'height (in)'}"))
    
    }) 
    
    output$ggv_sum_type = renderGvis({
        gvisColumnChart(g_sum_station(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title= input$fare_type, 
                            vAxis="{title:'Count (Million)'}" ) ) # ,
                            #hAxis="{title:'height (in)'}"))
        
    }) 
    
    
    
    
#####################-------ggv3: station  ------------------   
    #      Now let's look at different station ----------------
    #      only count the sum of fare swipe  ---------------
    
    g_sum_type_station = reactive({  
        df %>% filter(!year %in% c(2010, 2017)) %>% 
            filter(fare_type == input$fare_type) %>%
            filter(Station == input$station) %>% 
            group_by_(input$period) %>%
            dplyr::summarise(swipe_count = sum(fare_swipe)/1e6)
 
        
    })
    
    
#---------- ggv3   
    
    
    output$ggv_sum_type_station = renderGvis({
        gvisColumnChart(g_sum_type_station(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 300,
                            legend='none',
                            title="Total Swipe Count By Each Fare Type in Each Station", 
                            vAxis="{title:'Count (Million)'}" ) ) # ,
        #hAxis="{title:'height (in)'}"))
        
    }) 
    
    
### -----------------valueBox -Tab ggv2-----------------------
    
    output$top_1_station <- renderValueBox({
        valueBox(
            value = max(g_sum_station_top()$Station),
            subtitle = "Busiest Station",
            icon = icon("star")
        )
    })   
    

    output$top_1_year <- renderValueBox({
        valueBox(
            value = g_sum_station_top()$year,
            subtitle = "Busiest Year",
            icon = icon("star")
        )
    })   
    
    output$top_1_month <- renderValueBox({
        valueBox(
            value =   g_sum_station_top()$month,
            subtitle = "Busiest Month",
            icon = icon("star")
        )
    }) 
    
    
    output$top_1_count <- renderValueBox({
        valueBox(
            value = g_sum_station_top()$Total_swipe,
            subtitle = "Total swipe",
            icon = icon("star")
        )
    }) 
    
    
    
    
    
#####################-------ggv4: Time Line  ------------------   
    #      Now let's look at time line     --------------
    #                 for different station ---------------
    #      only count the sum of fare swipe  ---------------   
    
    
    g_timeline = reactive({
        df %>%  filter(fare_type == input$fare_type) %>%
            filter(Station == input$station) %>%
            group_by(To.Date) %>% 
            dplyr::summarise(swipe_count = sum(fare_swipe))
        
    })
    
    
#---------ggvs4 ------------time line
    
    output$ggv_timeline = renderGvis({
        gvisAnnotationChart(g_timeline(),
                            datevar="To.Date",
                            numvar="swipe_count",
                            
                            options=list(
                                #width = 10,
                                height= 300,
                                width='90%')
                            )
        
    }) 
    
    
 



} #1