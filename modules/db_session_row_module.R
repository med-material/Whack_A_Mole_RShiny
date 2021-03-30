library(lubridate)
library(shinyjs)
db_session_row_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(id=ns("row"),
      column(8, uiOutput(ns("sessionText"))),
      column(2, actionButton(ns("actionDelete"), "Delete")),
      column(2, actionButton(ns("actionChoose"), "Choose"))
    )
  )
}

db_session_row <- function(input, output, session, sesid, email, timestamp, name, program, duration) {
  ns <- session$ns
  
  toReturn <- reactiveValues(active = F, sesid = sesid, trigger = 0)
  
  db_session_row_UpdateText(input, output, session, sesid, email, timestamp, name, program, duration)
  
  observeEvent(input$actionDelete, {
    db_session_row_UpdateText(input, output, session, sesid, email, timestamp, name, program, duration, markForDeletion=TRUE)
    MarkDataForDeletion("Meta","SessionID",sesid)
  })
  
  observeEvent(input$actionChoose, {
    SetSessionID(sesid)
    toReturn$active = T
    toReturn$trigger = toReturn$trigger + 1
    removeModal()
  })
  
  return(toReturn)
}

db_session_row_UpdateText <- function(input, output, session, sesid, email, timestamp, name, program, duration, markForDeletion=F) {
  time <- sprintf("%02d:%02d", hour(timestamp), minute(timestamp))
  weekday <- wday(timestamp, abbr = F, label=T)
  thedate <- format(date(timestamp), "%d %b %Y")
  timestring <- paste(weekday, thedate,time)
  theid <- str_sub(sesid,-6,-1)
  style = ""
  styletext = ""
  if (markForDeletion) {
    style = "class='bg-danger'"
    styletext = " <strong>(Marked For Deletion)</strong>"
  }
  output$sessionText <- renderUI({
    HTML(paste0("<p ",style,">",trimws(name),", ",program," (",duration," sec.)",styletext,"<br><small>",timestring," (",theid,")</small></p>"))
  })
}