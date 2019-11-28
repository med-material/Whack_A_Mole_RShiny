library(shiny)

# Global variables
firstGen <- TRUE
initDone <- FALSE

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
  # Init the dropdowns
  output$Participants <- renderUI({selectInput(
    "Participants",
    label = h3("Participant"),
    choices = GenerateSelectChoices(default = "All participants", text = "Participant", fieldName = "ParticipantId"))})
  
  output$Difficulty <- renderUI({selectInput(
    "Difficulty",
    label = h3("Difficulty"),
    choices = GenerateSelectChoices(default = "All difficulties", text = "Difficulty", fieldName = "GameSpeed"))})
  
  output$Date <- renderUI({selectInput(
    "Date",
    label = h3("Date"),
    choices = GenerateSelectChoices(default = "All dates", text = "", fieldName = "Date"))})
  
  output$TestId <- renderUI({selectInput(
    "TestId",
    label = h3("Test"),
    choices = GenerateSelectChoices(default = "All tests", text = "Test", fieldName = "TestId"))})
  
  # Init the help texts
  
  output$ParticipantTextOutput <- renderText(paste("Participant ID:", CheckPropertyValue("ParticipantId", GenerateFilters()), sep = " "))
  output$TestTextOutput <- renderText(paste("Test ID:", CheckPropertyValue("TestId", GenerateFilters()), sep = " "))
  output$DurationTextOutput <- renderText(paste("Game duration:", CheckPropertyValue("GameDuration", GenerateFilters()), sep = " "))
  output$SpeedTextOutput <- renderText(paste("Game speed:", CheckPropertyValue("GameSpeed", GenerateFilters()), sep = " "))
  output$MirrorEffectTextOutput <- renderText(paste("Mirror effect:", CheckPropertyValue("MirrorEffect", GenerateFilters()), sep = " "))
  output$DualLaserTextOutput <- renderText(paste("Dual laser:", CheckPropertyValue("DualTask", GenerateFilters()), sep = " "))
  output$EyePatchTextOutput <- renderText(paste("Eye patch: ", CheckPropertyValue("EyePatch", GenerateFilters()), sep = " "))
  output$PrismEffectTextOutput <- renderText(paste("Prism effect:", CheckPropertyValue("PrismEffect", GenerateFilters()), sep = " "))
  
  # On update of one of the dropdown, update the value of the others
  observeEvent({input$Date
    input$Participants
    input$Difficulty
    input$TestId
    1}, {
      tempInitDone = initDone
      UpdateInputs()
      if(tempInitDone)
      {
        if(input$Participants != -1)
        {
          if(input$TestId != -1)
          {
            output$ParticipantTextOutput <- renderText(paste("Participant ID:", CheckPropertyValue("ParticipantId", GenerateFilters()), sep = " "))
            output$TestTextOutput <- renderText(paste("Test ID:", CheckPropertyValue("TestId", GenerateFilters()), sep = " "))
            output$DurationTextOutput <- renderText(paste("Game duration:", CheckPropertyValue("GameDuration", GenerateFilters()), sep = " "))
            output$SpeedTextOutput <- renderText(paste("Game speed:", CheckPropertyValue("GameSpeed", GenerateFilters()), sep = " "))
            output$MirrorEffectTextOutput <- renderText(paste("Mirror effect:", CheckPropertyValue("MirrorEffect", GenerateFilters()), sep = " "))
            output$DualLaserTextOutput <- renderText(paste("Dual laser:", CheckPropertyValue("DualTask", GenerateFilters()), sep = " "))
            output$EyePatchTextOutput <- renderText(paste("Eye patch: ", CheckPropertyValue("EyePatch", GenerateFilters()), sep = " "))
            output$PrismEffectTextOutput <- renderText(paste("Prism effect:", CheckPropertyValue("PrismEffect", GenerateFilters()), sep = " "))
          }
        }
      }
    })
  
  # Updates the Hit heatmap
  observeEvent({input$Date
    input$Participants
    input$Difficulty
    input$TestId
    1}, 
    {
      if(initDone)
      {
        conditionsList <- GenerateFilters(filters = list(list("Event = 'Mole Expired'", "Event = 'Mole Hit'")))
        
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
      }
    })
  
  
  # Function updating the value of the dropdowns
  UpdateInputs <- function()
  {
    if(firstGen)
    {
      firstGen <<- FALSE
      return()
    }
    
    output$Participants <- renderUI({selectInput(
      "Participants",
      label = h3("Participant"),
      choices = GenerateSelectChoices(default = "All participants", text = "Participant", fieldName = "ParticipantId", conditions = GenerateFilters(toIgnore = "Participants", ignoreTest = TRUE)), selected = input$Participants)})
    
    if(input$Participants != -1)
    {
      output$TestId <- renderUI({selectInput(
        "TestId",
        label = h3("Test"),
        choices = GenerateSelectChoices(default = "All tests", text = "Test", fieldName = "TestId", conditions = GenerateFilters(toIgnore = "TestId", ignoreTest = TRUE), extraInfo = list("Time")), selected = input$TestId)})
    }
    else
    {
      output$TestId <- NULL
    }
    
    output$Difficulty <- renderUI({selectInput(
      "Difficulty",
      label = h3("Difficulty"),
      choices = GenerateSelectChoices(default = "All difficulties", text = "Difficulty", fieldName = "GameSpeed", conditions = GenerateFilters(toIgnore = "Difficulty", ignoreTest = TRUE)), selected = input$Difficulty)})
    
    output$Date <- renderUI({selectInput(
      "Date",
      label = h3("Date"),
      choices = GenerateSelectChoices(default = "All dates", text = "", fieldName = "Date", conditions = GenerateFilters(toIgnore = "Date", ignoreTest = TRUE)), selected = input$Date)})
    
    if(!initDone)
      initDone <<- TRUE
  }
  
  # Function generating the filters for the SQL request
  GenerateFilters <- function(toIgnore = "", filters = list(), ignoreTest = FALSE)
  {
    i = length(filters) + 1
    
    if(toIgnore != "Participants")
    {
      if (input$Participants != -1)
      {
        filters[[i]] <- list(paste("ParticipantId = ", input$Participants, sep = ""))
        i = i+1
      }
    }
    
    if(!ignoreTest)
    {
      if(toIgnore != "TestId")
      {
        if (input$Participants != -1)
        {
          if (input$TestId != -1)
          {
            filters[[i]] <- list(paste("TestId = ", input$TestId, sep = ""))
            i = i+1
          }
        }
      }
    }
    
    if(toIgnore != "Difficulty")
    {
      if (input$Difficulty != -1)
      {
        filters[[i]] <- list(paste("GameSpeed = '", input$Difficulty, "'", sep = "")) 
        i = i+1
      }
    }
    
    if(toIgnore != "Date")
    {
      if (input$Date != -1)
      {
        filters[[i]] <- list(paste("Date = '", input$Date, "'", sep = "")) 
        i = i+1
      }
    }
    
    if (i == 1)
      return (list())
    else
      return(filters)
  }
})

