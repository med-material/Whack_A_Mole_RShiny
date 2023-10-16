library(lubridate)
library(shinyjs)

data_selection_filter_UI <- function(id) {
  ns = NS(id)
  column(12,
         tags$div(class=c('d-inline-block separated'), 
                  uiOutput(ns("data_selection_filter"))),
         tags$div(class=c('contextual-toolbar d-inline-block separated'), 
                  selectizeInput(ns("segmentFilter"), label = "Filter data..",
                                     choices = c("Loading.." = "All"),
                                     selected = c("All"), multiple = TRUE, width='400px',
                                     options = list(placeholder = "Filter data.."))),
  )
}

data_selection_filter <- function(input, output, session, df) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0,
    ndf = NA
  )

  # This observer looks for changes to df and updates the checkboxgroups.
  observe({
    validate(need(!is.na(df()), "Database not available."))
    validate(need(!is.null(df()), "Loading dataset.."))
    df_data <- df()
    
    df_c <- df()
    
    counts = df_c %>% distinct(PatternSegmentLabel, PatternSegmentID) %>% drop_na()
  
    labels = counts$PatternSegmentLabel
    values = paste0(counts$PatternSegmentLabel,counts$PatternSegmentID)
    
    newChoiceNames = labels
    newChoiceValues = values
    names(newChoiceValues) = newChoiceNames
    
    nCounts = nrow(counts)
    nDf = nrow(df())
    shinyjs::show("segmentFilter")
    
    if (length(labels) == 0) {
      newChoiceNames = "No data segments available"
      newChoiceValues = "All"
      nCounts = "No"
      shinyjs::hide("segmentFilter")
    }

    updateSelectizeInput(session, label = "Filter data..", inputId = "segmentFilter", 
                             choices = newChoiceValues, 
                             selected = NA, options = list(
                               placeholder = "Filter data.."))
  })  
  
  # Keep track of how much data is displayed and no. of segments
  output$data_selection_filter <- renderUI({
    validate(need(!is.na(df()), "Database not available."))
    validate(need(!is.null(df()), "Loading dataset.."))
    df_data <- df()
    
    df_c <- df()
    
    counts = df_c %>% distinct(PatternSegmentLabel, PatternSegmentID) %>% drop_na()
    
    nCounts = nrow(counts)
    nDf = nrow(df())
    
    if (length(labels) == 0) {
        nCounts = "No"
    }
    
    # Add filtering stats
    nDfFilter = toReturn$ndf
    if (!is.na(nDfFilter)) {
      nDf = paste(nDfFilter,"/",nDf)
    }
    
    ui <- HTML(paste(
      "<small>",
      nDf,
      "data points",
      "</small><br><small>",
      nCounts,
      "data segments",
      "</small>" 
    ))
  })  
  
  observeEvent(input$segmentFilter, {
    validate(need(!is.na(df()), "Database not available."))
    validate(need(!is.null(df()), "Loading dataset.."))
    
    if (is.null(input$segmentFilter)) {
      df_data = df() 
      toReturn$df <- df_data
      toReturn$trigger <- toReturn$trigger + 1
      toReturn$ndf <- NA
      return()
    }
    
    df_data = df()
    
    toReturn$filter <- input$segmentFilter
    
    segments = df_data %>% distinct(PatternSegmentLabel, PatternSegmentID) %>%
      mutate(labelID = paste0(PatternSegmentLabel,PatternSegmentID))
    
    segmentLabel = segments %>% filter(labelID %in% input$segmentFilter) %>% select(PatternSegmentLabel)
    segmentID = segments %>% filter(labelID %in% input$segmentFilter) %>% select(PatternSegmentID)
    
    df_data = df_data %>% fill(PatternSegmentID, .direction="down") %>%
                          fill(PatternSegmentLabel, .direction="down")
    
    df_data = df_data %>% filter(PatternSegmentLabel %in% unlist(segmentLabel),
                                 PatternSegmentID %in% unlist(segmentID))
    
    toReturn$df <- df_data
    toReturn$trigger <- toReturn$trigger + 1
    toReturn$ndf <- nrow(df_data)
    
  }, ignoreNULL=FALSE)
  
  return(toReturn)
}