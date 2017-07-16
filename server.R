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
            group_by_(input$period) %>% 
            dplyr::summarise(swipe_count = sum(fare_swipe))
        
        
    })
    
    ################--------------ggv1_mean : average swipe count in each time period 
    g_mean = reactive({   
        df %>% #filter(!year %in% c(2010, 2017)) %>% 
            group_by_(input$period) %>% 
            dplyr::summarise(swipe_count = mean(fare_swipe))
        
        
    })

 
    ###----------- ggv1 plots 
    
    # show timeline of fare count by period group by period, sum or mean of fare swipe count !!!!!!!!! done
    output$ggv_sum = renderGvis({
        gvisColumnChart(g_sum(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 350,
                            legend='none',
                            title="Total MTA Fare Card Swipe Number", 
                            vAxis="{title:'Count'}"
                            #hAxis="{title:'height (in)'}"
                            ))
        
    }) 
    
    
    output$ggv_mean = renderGvis({
        gvisColumnChart(g_mean(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 350,
                            legend='none',
                            title="Avearage MTA Fare Card Swipe Number", 
                            vAxis="{title:'Count'}"
                            #hAxis="{title:'height (in)'}"
                        ))
    }) 
    
    
    ### -----------------valueBox -Tab ggv1-----------------------
    
    output$period_max <- renderValueBox({
        valueBox(
            value = max(g_sum()$swipe_count),
            subtitle = "Max counts",
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
            value = arrange(g_sum(), desc(swipe_count))[1, input$period],
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
            dplyr::summarise(swipe_count = sum(fare_swipe))
        
    })
    
    
    g_sum_station_top = reactive({   
        df %>% group_by( year, month, Station) %>% 
            summarise(Total_swipe = sum(fare_swipe)) %>% 
            arrange(desc(Total_swipe)) %>% head(1)
    })
    
    
    
    
#---------- ggv2
    
    output$ggv_sum_type = renderGvis({
        gvisColumnChart(g_sum_type(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 350,
                            legend='none',
                            title="Total Swipe Number", 
                            vAxis="{title:'Count'}" ) ) # ,
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
            dplyr::summarise(swipe_count = sum(fare_swipe))
 
        
    })
    
    
#---------- ggv3   
    
    
    output$ggv_sum_type_station = renderGvis({
        gvisColumnChart(g_sum_type_station(), 
                        xvar = input$period, 
                        yvar = 'swipe_count', 
                        options=list(
                            height= 350,
                            legend='none',
                            title="Total Swipe Number", 
                            vAxis="{title:'Count'}" ) ) # ,
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
        df %>%  
            filter(fare_type == input$fare_type) %>%
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
                                #height= 500,
                                width='95%', height=500)
                            )
        
    }) 
    
    
    
    
#####================================================================ old code
#   
#     output$ggv2 = renderGvis({
#         gvisColumnChart(g2(), 
#                         xvar = input$period, 
#                         yvar = 'totalfullfare_ym', 
#                         options=list(
#                             
#                             lineWidth=2, pointSize=0,
#                             title="Women", 
#                             vAxis="{title:'weight (lbs)'}",
#                             hAxis="{title:'height (in)'}"))
#         
#     }) 
#     
# # show histogram using googleVis: ggv1
#     
#     output$ggv1 = renderGvis({
#         gvisHistogram(g3(), 
#                       options=list(
#                           title="Histogram of total swipe in each station", 
#                           vAxis="{title:'weight (lbs)'}",
#                           # hAxis="{title:'height (in)'}", 
#                           width='95%', height=500))
# 
#     })



########################### ggplot2 #########################
    
# attach output object (id is TripDuration) to output
    # output$totalswipe = renderPlot({
    #    g1() %>% group_by_(input$period) %>% 
    #         dplyr::summarise(totalfullfare_ym = sum(fare_swipe)) %>%
    #         ggplot(aes_string(x = input$period, y = 'totalfullfare_ym')) + 
    #         geom_bar( aes_string(fill = input$period), stat='identity', show.legend = F) +
    #         ggtitle('Total Swipe in MTA By ', input$period) +
    #         ylab('Total Number of Swipe') + 
    #         theme(text = element_text(size=16))
    # })
    
    
#output$TakeTrip = renderPlot({ })
    
    # output$avgswipe = renderPlot({
    #     g2() %>% group_by_(input$period) %>% 
    #         dplyr::summarise(totalfullfare_ym = mean(fare_swipe)) %>%
    #         ggplot(aes_string(x = input$period, y = 'totalfullfare_ym')) + 
    #         geom_bar( aes_string(fill = input$period), stat='identity', show.legend = F) +
    #         ggtitle('Average Swipe in MTA By ', input$period) +
    #         theme(text = element_text(size=10))
    # })
    # 
##############################################################    

} #1