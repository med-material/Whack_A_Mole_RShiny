#csv_upload_df = NULL
csv_upload_UI <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>CSV File Upload</h3>",
         "<p>Please add three files in here, which represent the
         <strong>Event</strong>, <strong>Meta</strong> or <strong>Sample</strong>
         CSV files.</p>"),
    fileInput(
      ns("fileMeta"),
      "Choose Meta CSV File", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    fileInput(
      ns("fileEvent"),
      "Choose Event CSV File", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    fileInput(
      ns("fileSample"),
      "Choose Sample CSV File", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
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
      toReturn$df <-  LoadFromFilePaths(input$fileMeta$datapath, input$fileEvent$datapath, input$fileSample$datapath)
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