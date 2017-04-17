library(shiny)
library(plotly)
library(leaflet)
source('helpers.R')

# Define server logic
shinyServer(function(input, output, session) {
    
    # Update pollutant selection box
    updateSelectInput(session, 'select_pollutant', 
                      choices = criteriaList)
    
    #########################
    # Reactive Functions
    #########################
    # Extract selected pollutant data.
    data_pollutant <- reactive({
        data <- data_criteria[data_criteria$Parameter.Code == input$select_pollutant, ]
    })
    
    # Extract selected pollutant with selected metric
    data_pollutant_metric <- reactive({
        data <- data_criteria[(data_criteria$Parameter.Code == input$select_pollutant) &
                                  data_criteria$Metric.Used == input$select_metric, ] 
    })
    # Build metric list for metric selection.
    metric_list <- reactive({
        data <- data_pollutant()
        names <- unique(data$Metric.Used)
        return(setNames(as.list(names), names))
    })
    
    
    # Compute state aggregated means.
    state_aggregate <- reactive({
        data <- data_pollutant_metric()
        
        
        # Compute aggregated means
        data_aggregate <- aggregate(list(Arithmetic.Mean = data$Arithmetic.Mean),
                                    by = list(State.Code = data$State.Code,
                                              Metric.Used = data$Metric.Used), 
                                    FUN = mean)
        
        
        data_aggregate$Rank <- rank(1 - data_aggregate$Arithmetic.Mean)
        data_aggregate <- merge(data_aggregate, data_states, 
                                by = c('State.Code'), 
                                all = TRUE)
        data_aggregate$hover <- with(data_aggregate, paste(State.Name, '<br>', 'Rank:', Rank))
        
        
        return(data_aggregate)
    })
    
    #########################
    # End of Reactive Functions
    #########################
    
    # Update metric selection box if user selects another pollutant
    observe({
        updateSelectInput(session, 'select_metric',
                          choices = metric_list())
    })
    
    # Leaflet map to show monitor sites.
    output$site <- renderLeaflet({
        # Check required input first
        validate(
            need(input$select_pollutant, 'Please select pollutant.'),
            need(input$select_metric, 'Please select metric')
        )
        
        data <- data_pollutant_metric()
        
        if (input$top_ten) {
            data <- data[order(data$Arithmetic.Mean, decreasing = TRUE),][1:10,]
        }
        
        data %>% 
            leaflet() %>%
            addTiles() %>% 
            addMarkers(clusterOptions = TRUE, lng = ~Longitude, lat = ~Latitude, 
                       label = paste('Rank', as.character(1:10)),
                       popup = paste('PM2.5 Mean:', 
                                     as.character(round(data$Arithmetic.Mean, 3)), 
                                     'um'))
    })
    
    # Update leaflet map (to avoid resetting zoom levels)
    observe({
        data <- data_pollutant_metric()
        
        if (input$top_ten) {
            data <- data[order(data$Arithmetic.Mean, decreasing = TRUE),][1:10,]
        }
        
        # Compute circle radius 10000*(normalized arithmetic mean Z score + 2)
        radii <- 1e4*((data$Arithmetic.Mean - mean(data$Arithmetic.Mean))/sd(data$Arithmetic.Mean) + 2)
        proxy <- leafletProxy('site', data = data)
        
        if (input$circle) {
            proxy %>%
                addCircles(lng = ~Longitude, lat = ~Latitude,
                           radius = radii, weight = 0, color = 'red')
        } else {
            proxy %>% 
                clearShapes()
        }
        
        
    })
    
    # Plotly Choropleth Map for State Aggregate
    output$state <- renderPlotly({
        # Check required input first
        validate(
            need(input$select_pollutant, 'Please select pollutant.'),
            need(input$select_metric, 'Please select metric')
        )
        
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        p <- plot_geo(state_aggregate(), 
                      locationmode = 'USA-states') %>%
            add_trace(
                z = ~Arithmetic.Mean, text = ~hover, locations = ~State.Abb,
                color = ~Arithmetic.Mean, colors = 'Reds'
            ) %>%
            colorbar(title = "Average Concentration") %>%
            layout(
                title = 'Average Concentration by State<br>(Hover for details)',
                geo = g
            )
        p
        
        
    })
})