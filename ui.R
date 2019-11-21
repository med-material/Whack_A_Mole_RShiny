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
                         selectInput(
                           "Participants",
                           label = h3("Participant"),
                           choices = GenerateSelectChoices("All participants", "Participant", "ParticipantId"),
                           selected = -1
                         ),
                         selectInput(
                           "Difficulty",
                           label = h3("Difficulty"),
                           choices = GenerateSelectChoices("All difficulties", "Difficulty", "GameSpeed"),
                           selected = -1
                         ),
                         conditionalPanel(
                           "input.Participants != -1",
                           selectInput(
                             "Test",
                             label = h3("Test"),
                             choices = list("1" = "-1"),
                             selected = -1
                           )
                         ),
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
