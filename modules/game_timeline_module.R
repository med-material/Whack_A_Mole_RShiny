game_timeline_UI <- function(id) {
  ns = NS(id)
  list(
  sidebarPanel(width = 2, 
               selectizeInput(ns("timestampInput"), "Timestamp", choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
               selectizeInput(ns("eventInput"), "Event Data", choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
               selectizeInput(ns("eventTypeInput"), "EventType Data", choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
               selectizeInput(ns("contInput"), "Continuous Data", choices = NULL, selected = NULL, multiple = TRUE, options = NULL),
               selectizeInput(ns("ignoreEventInput"), "Ignore Events", choices = NULL, selected = NULL, multiple = TRUE, options = NULL)
  ),
  mainPanel(width = 10,
            fluidRow(
              column(8, plotlyOutput(ns("timelinePlot")),tags$div(class = "vizcontrols-explainer")),
              column(4, plotlyOutput(ns("gridPlot")))
            ),
            fluidRow(
              column(2, tableOutput(ns("moleTable"))),
              column(2, tableOutput(ns("directionTable"))),
              column(4, plotlyOutput(ns("motorPlot"))),
              column(4, plotlyOutput(ns("eyePlot")))
            )
  )
  )
}

game_timeline <- function(input, output, session, df) {
  ns <- session$ns
  
  
  observeEvent(df(), {
    req(!is.na(df()))
    print(nrow(df()))
    updateSelectizeInput(session,"timestampInput", choices = names(df()), selected = "Timestamp.Event", server = FALSE)
    updateSelectizeInput(session,"eventInput", choices = names(df()), selected = "Event", server = FALSE)
    updateSelectizeInput(session,"eventTypeInput", choices = names(df()), selected = "EventType", server = FALSE)
    updateSelectizeInput(session,"contInput", choices = names(df()), selected = c("HeadCameraRotEulerX", "HeadCameraRotEulerY", "HeadCameraRotEulerZ", "HeadCameraPosWorldX","HeadCameraPosWorldY","HeadCameraPosWorldZ", "WorldGazeHitPositionX","WorldGazeHitPositionY","WorldGazeHitPositionZ","MolePositionWorldX","MolePositionWorldY","MolePositionWorldZ"), server = FALSE)
    updateSelectizeInput(session,"ignoreEventInput", choices = unique(df()$Event), selected = c("NoData", "Sample"), server = FALSE)
  })
  
  output$timelinePlot <- renderPlotly({
    validate(need(df(), "Loading.."), errorClass = "vis")
    validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
    vis_timeline_whack(df(), input$timestampInput, input$eventInput, input$eventTypeInput,
                       input$contInput, input$ignoreEventInput)
  })
  output$gridPlot <- renderPlotly({
    validate(need(df(), ""), errorClass = "vis")
    validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
    select.data <- event_data(event = "plotly_selected")
    vis_whackgrid(df(), select.data, input$timestampInput, input$contInput)
  })
  output$moleTable <- renderTable({
    req(df())
    vis_moleTable(df())
  })
  output$directionTable <- renderTable({
    req(df())
    vis_directionTable(df(), input$timestampInput)
  })
  output$eyePlot <- renderPlotly({
    validate(need(df(), ""), errorClass = "vis")
    validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
    select.data <- event_data(event = "plotly_selected")
    vis_eyePlot(df(), select.data, input$timestampInput)
  })
  output$motorPlot <- renderPlotly({
    validate(need(df(), ""), errorClass = "vis")
    validate(need(df()$MotorSpaceName, "Motorspace Visualization Not Supported."), errorClass = "vis")
    validate(need(input$timestampInput, "Loading.."), errorClass = "vis")
    select.data <- event_data(event = "plotly_selected")
    vis_motorspace(df(), select.data, input$timestampInput)
  })
  
}