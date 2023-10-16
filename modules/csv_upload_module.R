#csv_upload_df = NULL
csv_upload_UI <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>CSV File Upload</h3>",
         "<p>Please add three files in here, which represent the
         <strong>Meta</strong>, <strong>Event</strong> or <strong>Sample</strong>
         CSV files.</p>"),
    
    fileInput(
      ns("fileMeta"),
      "Choose Meta CSV File", placeholder = "Select Meta CSV file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    fileInput(
      ns("fileEvent"),
      "Choose Event CSV File", placeholder = "Select Event CSV file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    fileInput(
      ns("fileSample"),
      "Choose Sample CSV File", placeholder = "Select Sample CSV file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    actionButton(ns("actionSubmit"), "Submit"),
    textOutput(ns("statusText"))
  )
}

csv_upload <- function(input, output, session) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  observeEvent(input$actionSubmit, {
    load_files <- !is.null(input$fileMeta) && !is.null(input$fileEvent) && !is.null(input$fileSample)
    if (load_files) {
      df <- LoadFromFilePaths(input$fileMeta$datapath, input$fileEvent$datapath, input$fileSample$datapath)
      meta <- LoadSingleFile(input$fileMeta$datapath)
      toReturn$df <- PreprocessGlobalData(df)
      toReturn$df_meta <- meta
      toReturn$trigger <- toReturn$trigger + 1
    }
  })
  
  observeEvent(toReturn$df, {
    req(!is.null(toReturn$df))
    output$statusText <- renderText({ " Data Received Successfully!" })
    insertUI(selector = paste0("#", ns("statusText")), where="afterBegin",
            ui = icon("check", class = "fa-1x", lib="font-awesome"))
  })
  
  return(toReturn)
}