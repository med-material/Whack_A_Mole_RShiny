db_select_UI <- function(id) {
  ns = NS(id)
  list(
    fluidPage(
    HTML("<h3>Select and Edit Data</h3>",
         "<p>Here you can switch which data record is being used,
         remove records and upload new records.</p>"),
    textOutput(ns("statusText")),
    uiOutput(ns("sessionList"))
    )
  )
}

db_select <- function(input, output, session, connected) {
  ns <- session$ns
  
  meta = NULL
  active_session = ""
  current_trigger = 0
  active_df = NULL
  
  if (connected) {
    meta <- unique(RetreiveAllData("Meta"))
    active_session = GetSessionID()
    if (active_session == "NA") {
      latest_session = nrow(meta)
      active_session <- as.character(meta$SessionID[latest_session])
      SetSessionID(active_session)
    }
    active_df = RetreiveCurrentData()
    current_trigger = current_trigger + 1
  }

  toReturn <- reactiveValues(
    df = active_df,
    df_meta = meta,
    session = active_session,
    trigger = current_trigger
  )
  
  observeEvent(toReturn$df_meta, {
    req(!is.null(toReturn$df_meta))
    ids <- toReturn$df_meta$SessionID
    timestamps <- toReturn$df_meta$Timestamp
    emails <- toReturn$df_meta$Email
    output$statusText <- renderText({ paste(length(ids), "sessions available.") })
    output$sessionList <- renderUI({
      lapply(1:length(ids), function(i) {
        if (ids[i] == toReturn$session) {
          tags$div(class = "font-weight-bold text-primary",
             db_session_row_UI(ns(i))
          )
        } else {
          db_session_row_UI(ns(i))
        }
      })
    })
    
    chosen_row <- lapply(1:length(ids), function(i) {
      callModule(db_session_row, i, meta$SessionID[i],meta$Email[i],meta$Timestamp[i],
                 meta$ProfileName[i], meta$SessionProgram[i], meta$SessionDuration[i])
    })
    
    lapply(chosen_row, function(row) {
      observeEvent(row$active, {
        req(row$trigger > 0)
        SetSessionID(row$sesid)
        toReturn$df <<- RetreiveCurrentData()
        toReturn$session <<- row$sesid
        toReturn$trigger <<- toReturn$trigger + 1
        
      })
    })
    
  })
  
  return(toReturn)
}