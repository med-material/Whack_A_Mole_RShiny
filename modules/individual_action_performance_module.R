individual_action_performance_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
      tags$h3("Action Performance Summary", class="page-header"),
      plot_action_summary_UI(ns("action_summary"))
    ),
  )
}

individual_action_performance <- function(input, output, session, df, meta) {
  ns <- session$ns
  observe({
    req(!is.na(df()))
    req(!is.null(df()))
    callModule(plot_action_summary, "action_summary", df)
  })
}