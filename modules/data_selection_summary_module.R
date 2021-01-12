library(lubridate)
library(shinyjs)
data_selection_summary_UI <- function(id) {
  ns = NS(id)
  uiOutput(ns("data_selection_text"))
}

data_selection_summary <- function(input, output, session, df) {
  ns <- session$ns

  output$data_selection_text <- renderUI({
    HTML(paste(
      "Data from ",
      wday(min(as.character(df()$MetaTimestamp), na.rm=T), abbr = F, label=T),
      format(date(min(as.character(df()$MetaTimestamp), na.rm=T)), "%d %b %Y"),
      sprintf("%02d:%02d", hour(min(as.character(df()$MetaTimestamp), na.rm=T)), minute(min(as.character(df()$MetaTimestamp), na.rm=T))),
      "<br><small>",
      as.character(min(as.character(df()$MetaEmail), na.rm=T)),
      "(",
      str_sub(max(as.character(df()$SessionID), na.rm=T),-6,-1),
      ")",
      "</small>"
    ))
  })  
}