library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
  observe({
    
    if (input$Participants == -1)
      return()
    
    updateSelectInput(session, "Test",
                      choices = GenerateSelectChoices("All tests", "Test", "TestId", list(list(paste("ParticipantId = ", input$Participants, sep = ""))))
    )})
  
  observe({
    conditionsList <- list(list("Event = 'Mole Expired'", "Event = 'Mole Hit'"))
    
    i = 2
    if (input$Participants != -1) 
    {
      conditionsList[[i]] <- (paste("ParticipantId = ", input$Participants, sep = "")) 
      i = i+1
    }
    
    if (input$Test != -1)
    {
      conditionsList[[i]] <- (paste("TestId = ", input$Test, sep = "")) 
      i = i+1
    }
    
    if (input$Difficulty != -1)
    {
      conditionsList[[i]] <- (paste("GameSpeed = '", input$Difficulty, "'", sep = "")) 
      i = i+1
    }
    
    output$PrecisionHeatMap <- renderPlotly(plot_ly(z = GenerateMatrix(xParam = "MoleIndexX", yParam = "MoleIndexY", xLength = 9, yLength = 7, valueParam = "Event", conditions = conditionsList, scoringFunction = function(matrixList, x, y)
    {
      minusScore <- length(which((matrixList$MoleIndexX == x) & (matrixList$MoleIndexY == y) & (matrixList$Event == "Mole Expired")))
      plusScore <- length(which((matrixList$MoleIndexX == x) & (matrixList$MoleIndexY == y) & (matrixList$Event == "Mole Hit")))
      
      if (input$HeatMapDisplay == 1)
      {
        return(plusScore)
      }
      else if (input$HeatMapDisplay == 2)
      {
        return(-minusScore)
      }
      return(plusScore - minusScore)
    }
    ), type = "heatmap") %>% layout(xaxis = list(title = "Mole X index"), yaxis = list(title = "Mole Y index")))
  })
})

