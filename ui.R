library(shiny)
library(shinyBS)
library(plotly)
library(leaflet)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("2016 U.S. Air Pollution Visualizer"),
  
  # Sidebar for pollutant & metric selection
  sidebarLayout(
    sidebarPanel(
      selectInput('select_pollutant', 'Select Pollutant', choices = list()),
      
      uiOutput('selectUI'),
      
      br(),br(),
      a(href = '', 'More Info'),
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
                 leafletOutput('site'),
                 checkboxInput('top_ten', label = 'Show Top 10', value = FALSE),
                 bsTooltip('top_ten', 
                           'Show only the top 10 sites with highest measured concentrations',
                           placement = 'bottom',
                           options = list(container = 'body')),
                 checkboxInput('circle', label = 'Visualize Concentration', value = TRUE),
                 bsTooltip('circle', 
                           'Toggle visual display of concentration level',
                           placement = 'bottom',
                           options = list(container = 'body')),
                 actionLink('site_tips', label = 'Tips'),
                 bsPopover('site_tips', 
                           'Hover over individual markers to display ranks compared to other markers.<br>Click on markers to zoom (cluster) or display mean (individual)',
                           trigger = 'click',
                           placement = 'top')
                     
        ),
        
        tabPanel('State Average', plotlyOutput('state'),
                 actionLink('state_tips', label = 'Tips'),
                 bsPopover('state_tips', 
                           'Hover over each state to display detailed info about each state.<br>No label means no data available.',
                           trigger = 'click',
                           placement = 'top')
        )
        
      )
    )
  )
))