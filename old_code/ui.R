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
                         tabPanel("Hit location",
                                  selectInput(
                                    "HeatMapDisplay",
                                    label = "",
                                    choices = list("Hits - Misses" = "-1", "Hits" = "1", "Misses" = "2"),
                                    selected = -1
                                  ),
                                  plotlyOutput("PrecisionHeatMap"),
                                  uiOutput("HeadRotation")
                         ),
                         tabPanel("Precision",
                                  plotlyOutput("plot")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(6,
                                textOutput("ParticipantTextOutput"),
                                textOutput("TestTextOutput"),
                                textOutput("DurationTextOutput"),
                                textOutput("SpeedTextOutput")
                                ),
                         column(6,
                                textOutput("MirrorEffectTextOutput"),
                                textOutput("DualLaserTextOutput"),
                                textOutput("EyePatchTextOutput"),
                                textOutput("PrismEffectTextOutput")
                                )
                       )
                       )
                     )
                   )))
