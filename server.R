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
    
    df <- reactive ({
        load_files <- !is.null(input$fileMeta) && !is.null(input$fileEvent) &&
                      !is.null(input$fileSample)
        if (load_files) {
            LoadFromFilePaths(input$fileMeta$datapath, input$fileEvent$datapath, input$fileSample$datapath)
        } else {
            NULL
        }
    })
    
    observeEvent(input$visButton, {
        updateSelectizeInput(session,"timestampInput", choices = names(df()), selected = "Timestamp", server = FALSE)
        updateSelectizeInput(session,"eventInput", choices = names(df()), selected = "Event", server = FALSE)
        updateSelectizeInput(session,"eventTypeInput", choices = names(df()), selected = "EventType", server = FALSE)
        updateSelectizeInput(session,"contInput", choices = names(df()), selected = c("HeadCameraRotEulerX", "HeadCameraRotEulerY", "HeadCameraRotEulerZ", "HeadCameraPosWorldX","HeadCameraPosWorldY","HeadCameraPosWorldZ", "WorldGazeHitPositionX","WorldGazeHitPositionY","WorldGazeHitPositionZ","MolePositionWorldX","MolePositionWorldY","MolePositionWorldZ"), server = FALSE)
        updateSelectizeInput(session,"ignoreEventInput", choices = unique(df()$Event), selected = c("NoData", "Sample"), server = FALSE)
    })
    
    output$timelinePlot <- renderPlotly({
        validate(need(df(), Msg_nodata()))
        validate(need(input$timestampInput, "Loading.."))
        vis_timeline_whack(df(), input$timestampInput, input$eventInput, input$eventTypeInput,
                     input$contInput, input$ignoreEventInput)
    })
    output$gridPlot <- renderPlotly({
        validate(need(df(), ""))
        validate(need(input$timestampInput, "Loading.."))
        select.data <- event_data(event = "plotly_selected")
        vis_whackgrid(df(), select.data, input$timestampInput, input$contInput)
    })
    output$moleTable <- renderTable({
        req(df())
        vis_moleTable(df())
    })
    output$eyePlot <- renderPlotly({
        validate(need(df(), ""))
        validate(need(input$timestampInput, "Loading.."))
        select.data <- event_data(event = "plotly_selected")
        vis_eyePlot(df(), select.data, input$timestampInput)
    })
    output$motorPlot <- renderPlotly({
        validate(need(df(), ""))
        validate(need(df()$MotorSpaceName, "Not Supported."))
        validate(need(input$timestampInput, "Loading.."))
        select.data <- event_data(event = "plotly_selected")
        vis_motorspace(df(), select.data, input$timestampInput)
    })
})
