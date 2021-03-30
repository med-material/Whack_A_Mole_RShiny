#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.maxRequestSize=50*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
  r <- reactiveValues(df = NULL, meta = NULL, source = NULL)

  callModule(data_selection_summary,"input_info", reactive(r$df))
  callModule(player_overview,"overview_panel", reactive(r$df))

  auth = read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character"))
  connected = ConnectToServer(auth)
  if (!connected) {
    r$df <- NA
    shinyjs::disable("DbButton")
  }
  
  db_data <- callModule(db_select, "selectData", connected)
  csv_data <- callModule(csv_upload, "uploadData")

  
  observeEvent(csv_data$trigger, {
    req(csv_data$trigger > 0)
    r$df <- csv_data$df
    r$source <- 'csv data'
  })
  observeEvent(db_data$trigger, {
    req(db_data$trigger > 0)
    r$df <- db_data$df
    r$source <- db_data$session
  })
  
  observeEvent(input$CsvButton, {
    insertUI(selector = "#CsvButton", where = "afterEnd",
             ui = showModal(modalDialog(csv_upload_UI("uploadData"), easyClose = TRUE)))
  })
  observeEvent(input$DbButton, {
    insertUI(selector = "#DbButton", where = "afterEnd",
             ui = showModal(modalDialog(db_select_UI("selectData"), easyClose = TRUE)))
  })
    
  observeEvent(r$df, {
    req(!is.na(r$df))
        updateSelectizeInput(session,"timestampInput", choices = names(r$df), selected = "Timestamp.Event", server = FALSE)
        updateSelectizeInput(session,"eventInput", choices = names(r$df), selected = "Event", server = FALSE)
        updateSelectizeInput(session,"eventTypeInput", choices = names(r$df), selected = "EventType", server = FALSE)
        updateSelectizeInput(session,"contInput", choices = names(r$df), selected = c("HeadCameraRotEulerX", "HeadCameraRotEulerY", "HeadCameraRotEulerZ", "HeadCameraPosWorldX","HeadCameraPosWorldY","HeadCameraPosWorldZ", "WorldGazeHitPositionX","WorldGazeHitPositionY","WorldGazeHitPositionZ","MolePositionWorldX","MolePositionWorldY","MolePositionWorldZ"), server = FALSE)
        updateSelectizeInput(session,"ignoreEventInput", choices = unique(r$df$Event), selected = c("NoData", "Sample"), server = FALSE)
  })
    
    output$timelinePlot <- renderPlotly({
        validate(need(r$df, "Loading.."), errorClass = "vis")
        validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
        vis_timeline_whack(r$df, input$timestampInput, input$eventInput, input$eventTypeInput,
                     input$contInput, input$ignoreEventInput)
    })
    output$gridPlot <- renderPlotly({
        validate(need(r$df, ""), errorClass = "vis")
        validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
        select.data <- event_data(event = "plotly_selected")
        vis_whackgrid(r$df, select.data, input$timestampInput, input$contInput)
    })
    output$moleTable <- renderTable({
        req(r$df)
        vis_moleTable(r$df)
    })
    output$directionTable <- renderTable({
      req(r$df)
      vis_directionTable(r$df, input$timestampInput)
    })
    output$eyePlot <- renderPlotly({
        validate(need(r$df, ""), errorClass = "vis")
        validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
        select.data <- event_data(event = "plotly_selected")
        vis_eyePlot(r$df, select.data, input$timestampInput)
    })
    output$motorPlot <- renderPlotly({
        validate(need(r$df, ""), errorClass = "vis")
        validate(need(r$df$MotorSpaceName, "Motorspace Visualization Not Supported."), errorClass = "vis")
        validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
        select.data <- event_data(event = "plotly_selected")
        vis_motorspace(r$df, select.data, input$timestampInput)
    })
})
