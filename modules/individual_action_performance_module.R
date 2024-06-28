individual_action_performance_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
      tags$h3("Action Performance Summary", class="page-header"),
      plot_action_summary_UI(ns("action_summary"))
    ),
    fluidRow(
      tags$h3("Spatial Overview of Actions", class="page-header"),
      tags$p("Provides an overview of players' actions."),
      plot_action_grid_performance_UI(ns("action_grid"))
    ),
  )
}

individual_action_performance <- function(input, output, session, df, meta) {
  ns <- session$ns
  observe({
    req(!is.na(df()))
    req(!is.null(df()))
    callModule(plot_action_summary, "action_summary", df)
    callModule(plot_action_grid_performance, "action_grid", df)
  })
}