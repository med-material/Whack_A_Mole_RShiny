library(shiny)
library(plotly)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  
  includeCSS("custom.css"),
  useShinyjs(),
  fluidRow(
    column(8, titlePanel("Tunnel Goal Fitts Tests (Cohort 2020)")),
    column(4,
           column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib="font-awesome")),
           column(11,style = "margin-top : 20px; text-align: center;",
            selectInput("emailSelect", NULL, choices=c("Loading.." = -1))
           )
    )
  ),
  fluidRow(
    column(12, checkboxGroupInput("pidChooser", label = "Loading...", choices = NULL, inline = TRUE))
  ),
#  subjectChooser ----------------
  tabsetPanel(id = "subjectChooser", type = "tabs",
          
    tabPanel(value  = "trainingperformance", id = "Goal", strong("Goal Test"),
        navlistPanel(
          widths = c(4, 8),
          "Choose Visualization:",
          tabPanel("Overall Speed",
              plotlyOutput("speedOverTime"),
              #tags$div(class = "vizcontrols-explainer"),
              #textOutput("averagePerf"),
          ),
          tabPanel("Mirror Condition Comparison",
                   #plotOutput('goalLRPlot')
                   #plotlyOutput("fittsRegPlot"),
                   #tags$div(class = "vizcontrols-explainer"),
          )
          )
        
    ),
  
  # Rest of Page ---------------------------------------------------------------
  
  tags$footer()
)
)