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
        validate(need(!is.null(input$fileMeta) && !is.null(input$fileEvent) &&
                      !is.null(input$fileSample), Msg_nodata()),
                 need(input$visButton, "Press the Upload Button."))
        LoadFromFilePaths(input$fileMeta$datapath, input$fileEvent$datapath, input$fileSample$datapath)
    })
    
    observeEvent(input$visButton, {
        updateSelectizeInput(session,"timestampInput", choices = names(df()), selected = "Timestamp", server = FALSE)
        updateSelectizeInput(session,"eventInput", choices = names(df()), selected = "Event", server = FALSE)
        updateSelectizeInput(session,"eventTypeInput", choices = names(df()), selected = "EventType", server = FALSE)
        updateSelectizeInput(session,"contInput", choices = names(df()), server = FALSE)
        updateSelectizeInput(session,"ignoreEventInput", choices = unique(df()$Event), selected = c("NoData", "Sample"), server = FALSE)
    })
    
    output$timelinePlot <- renderPlotly({
        validate(need(df(), Msg_nodata()))
        vis_timeline(df(), input$timestampInput, input$eventInput, input$eventTypeInput,
                     input$contInput, input$ignoreEventInput)
    })
})
