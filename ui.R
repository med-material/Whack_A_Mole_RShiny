library(shiny)
library(plotly)

# Define UI for application that plots features of movies
shinyUI(navbarPage(title = "Whack A Mole VR",
                   tabPanel(
                     "Tests results",
                     
                     # Sidebar layout with a input and output definitions
                     sidebarLayout(
                       # Inputs
                       sidebarPanel(
                         uiOutput("Date"),
                         uiOutput("Participants"),
                         uiOutput("Difficulty"),
                         uiOutput("TestId")
                       ),
                       
                       # Outputs
                       mainPanel(tabsetPanel(
                         tabPanel("Precision",
                                  plotlyOutput("plot")
                         ),
                         tabPanel("Hit location",
                                  selectInput(
                                    "HeatMapDisplay",
                                    label = "",
                                    choices = list("Hits - Misses" = "-1", "Hits" = "1", "Misses" = "2"),
                                    selected = -1
                                  ),
                                  plotlyOutput("PrecisionHeatMap")
                         )
                       ))
                     )
                   )))
