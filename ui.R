#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    includeCSS("custom.css"),
    useShinyjs(),
    tags$header(fluidRow(
        # Input ----------------
        column(1,div(class="text-center",img(src='whack_icon.svg', id="whack-logo"))),
        column(11,
               actionButton("DbButton", "Change Data"),
               actionButton("CsvButton","Manual Upload"),
               tags$div(class = "d-inline-block separated", data_selection_summary_UI("input_info"),),
               tags$div(class = "d-inline-block separated", data_selection_filter_UI("input_filter"),)
        )
    )),
    navlistPanel(fluid= FALSE, widths=c(1,11), well = FALSE, id = "overall-nav",
        tabPanel(title = div(class="text-center", img(src='nav_individual.svg', style="max-width:100%;"),tags$br(),"Individual"),
                 navlistPanel(id = "analysisChooser", well= FALSE, widths=c(2,10), fluid = FALSE,
                   tabPanel(value = "Player Overview", id = "PlayerOverview", HTML("Player Overview<br><small>See overview information.</small>"),
                            div(class="main-content", player_overview_UI("overview_panel"))
                   ),
                   tabPanel(value  = "Game Performance", id = "GamePerf", HTML("Game Performance<br><small>Successful hits, misses and speed.</small>"),
                            div(class="main-content", individual_game_performance_UI("individual_game_performance"))
                   ),
                   tabPanel(value  = "Action Performance", id = "ActionPerf", HTML("Action Performance<br><small>Coarse and fine submovement.</small>"),
                            div(class="main-content", individual_action_performance_UI("individual_action_performance"))
                   ),
                   tabPanel(value  = "Head Movement", id = "HeadMove", HTML("Head Movement<br><small>Position and orientation of the head.</small>"),
                            div(class="main-content", individual_head_movement_UI("individual_head_movement"))
                   ),
                   tabPanel(value  = "Eye Movement", id = "EyeMove", HTML("Eye Movement<br><small>Hemispheric position and 3D gaze activity.</small>"),
                            div(class="main-content", individual_gaze_movement_UI("individual_gaze_movement"))
                   ),
                   tabPanel(value  = "Controller Movement", id = "ControllerMove", HTML("Controller Movement<br><small>Left and right controller activity.</small>"),
                            div(class="main-content", individual_controller_movement_UI("individual_controller_movement"))
                   ),
                   tabPanel(value  = "Time (X-axis)", id = "Timeline", HTML("Timeline<br><small>Replay the game from start to end.</small>"),
                            div(class="main-content", game_timeline_UI("timeline_panel"))
                   )
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
                 )
        )
    ),
    # Rest of Page ---------------------------------------------------------------
    tags$footer()
))
