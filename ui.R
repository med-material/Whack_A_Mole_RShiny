#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    includeCSS("custom.css"),
    # Input ----------------
    fluidRow(
        column(4, titlePanel("Game Timeline"),),
    ),
    fluidRow(
        column(2, data_selection_summary_UI("input_info")),
        column(3, actionButton("DbButton", "Change Data"),
                  actionButton("CsvButton","Manual Upload"))
    ),
    #  Output ----------------
    tabsetPanel(id = "analysisChooser", type = "tabs",
        tabPanel(value  = "Time (X-axis)", id = "Timeline", strong("Timeline"),
                 sidebarPanel(width = 2,
                      selectizeInput("timestampInput", "Timestamp", choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
                      selectizeInput("eventInput", "Event Data", choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
                      selectizeInput("eventTypeInput", "EventType Data", choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
                      selectizeInput("contInput", "Continuous Data", choices = NULL, selected = NULL, multiple = TRUE, options = NULL),
                      selectizeInput("ignoreEventInput", "Ignore Events", choices = NULL, selected = NULL, multiple = TRUE, options = NULL)
                 ),
                 mainPanel(width = 10,
                           fluidRow(
                               column(8, plotlyOutput("timelinePlot"),tags$div(class = "vizcontrols-explainer")),
                               column(4, plotlyOutput("gridPlot"))
                           ),
                           fluidRow(
                               column(2, tableOutput("moleTable")),
                               column(2, tableOutput("directionTable")),
                               column(4, plotlyOutput("motorPlot")),
                               column(4, plotlyOutput("eyePlot"))
                           )
                 ),
        ),
        # Rest of Page ---------------------------------------------------------------
        tags$footer()
    )
))