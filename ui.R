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
    tags$header(fluidRow(
        # Input ----------------
        column(1,div(class="text-center",img(src='whack_icon.svg', id="whack-logo"))),
        column(11,actionButton("DbButton", "Change Data"),
               actionButton("CsvButton","Manual Upload"),
               tags$div(class = "d-inline-block separated", data_selection_summary_UI("input_info"))
        )
    )),
    navlistPanel(fluid= FALSE, widths=c(1,11), well = FALSE, id = "overall-nav",
        tabPanel(title = div(class="text-center", img(src='nav_individual.svg', style="max-width:100%;"),tags$br(),"Individual"),
            #  Output ----------------
            tabsetPanel(id = "analysisChooser", type = "tabs",
                tabPanel(value = "Player Overview", id = "PlayerOverview", strong("Player Overview"), icon=icon('user'),
                    div(class="main-content", player_overview_UI("overview_panel"))
                ),
                tabPanel(value  = "Time (X-axis)", id = "Timeline", strong("Timeline"), icon = icon('chart-bar'),
                    div(class="main-content", game_timeline_UI("timeline_panel"))
                ),
                # Rest of Page ---------------------------------------------------------------
                tags$footer()
            )
        ),
        tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Trends"),
                 tabPanel(value = "Under Construction", id = "TrendsOverview", strong("Player Overview"), icon=icon('user'),
                          div(class="main-content", tags$p("Under Construction.."))
                 ),
        ),
        tabPanel(title = div(class="text-center", img(src='nav_plan.svg', style="max-width:100%;"),tags$br(),"Plan"),
                 tabPanel(value = "Under Construction", id = "PlanOverview", strong("Player Overview"), icon=icon('user'),
                          div(class="main-content", tags$p("Under Construction.."))
                 ),
        )
    )
))
