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
    useShinyjs(),
    tags$header(
        # Input ----------------
        img(src='whack_icon.svg', id="whack-logo"),
        actionButton("DbButton", "Change Data"),
        actionButton("CsvButton","Manual Upload"),
        tags$div(class = "d-inline-block separated", data_selection_summary_UI("input_info"))
    ),
    #  Output ----------------
    tabsetPanel(id = "analysisChooser", type = "tabs",
        tabPanel(value  = "Time (X-axis)", id = "Timeline", strong("Timeline"), icon = icon('chart-bar'),
                 game_timeline_UI("timeline_panel")
        ),
        tabPanel(value = "Player Overview", id = "PlayerOverview", strong("Player Overview"), icon=icon('user'),
                 mainPanel(width = 12,
                           player_overview_UI("overview_panel")
                 )
                 ),
        # Rest of Page ---------------------------------------------------------------
        tags$footer()
    )
))