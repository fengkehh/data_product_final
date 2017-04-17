library(shiny)
library(plotly)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Air Pollution Visualizer"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            
            selectInput('select_pollutant', label = h3('Select Pollutant'), 
                        choices = list()),
            
            selectInput('select_metric', label = h3('Select Metric'),
                        choices = list()),
            
            br(),br(),br(),br(),br(),
            a(href = '', 'User Manual'),
            br(), br(),
            strong('Created by Keh-Harng Feng')
            
        ),
        
        
        mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            tabsetPanel(
                tabPanel('Monitor Sites', 
                         checkboxInput('top_ten', label = 'Show Top 10', value = FALSE),
                         checkboxInput('circle', label = 'Visualize Concentration', value = TRUE),
                         leafletOutput('site'),
                         p(),
                         strong('Tips:'),
                         div('Hover over a marker to see the rank of the site concentration compared to all other displayed markers.',
                             br(),br(),
                             'Click on a marker to show concentration level.',
                             br(),br(),
                             'Show Top 10 shows the top 10 sites with highest measured concentrations.',
                             br(),br(),
                             'Visualize Concentration controls the display of concentration circles around each site.')
                         ),
                
                tabPanel('State Average', plotlyOutput('state'),
                         p(),
                         strong('Tips:'),
                         div('Hover over each state to see corresponding measured values.',
                         br(),br(),
                         'Missing popup label means no data is available for the state!')
                         )
                
            )
        )
    )
))