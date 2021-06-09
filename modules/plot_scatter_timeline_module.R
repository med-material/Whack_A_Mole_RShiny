library(lubridate)
library(shinyjs)
library(plotly)

options("digits.secs"=6)

plot_scatter_timeline_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
        actionButton(ns("resetPlot"), "Reset Plot"),
        checkboxGroupInput(ns("moleFilter"), label = " ",
                                     choices = c("Hits" = "Mole Hit", "Misses" = "Mole Missed","Distractors" = "Fake Mole Hit"),
                                     selected = c("Mole Hit"), inline = TRUE),
        checkboxGroupInput(ns("leftRightFilter"), label = " ",
                                     choices = c("Left", "Right", "Center"),
                                     selected = c("Left", "Right", "Center"), inline = TRUE),
      )
    ),
    fluidRow(
      plotlyOutput(ns("timelinePlot")),
    ),
    fluidRow(
      tags$div(class = "vizcontrols-explainer")
    )
  )
}

plot_scatter_timeline <- function(input, output, session, df) {
  ns <- session$ns
  
  r <- reactiveValues(filter = c("Mole Hit"), lrfilter = c("Left", "Right", "Center"), reset = 0)
  
  observeEvent(input$moleFilter, {
    r$filter <- input$moleFilter
    
  })
  
  observeEvent(input$leftRightFilter, {
    r$lrfilter <- input$leftRightFilter
  })
  
  observeEvent(input$resetPlot, {
    r$reset = r$reset + 1
  })
  
  # Add counts to our checkbox groups.
  observeEvent(df, {
    df_c <- df()
    counts = df_c %>% filter(Event %in% c("Mole Hit", "Mole Missed", "Fake Mole Hit")) %>%
      group_by(Event) %>%
      dplyr::summarize(Count = n()) %>%
      pivot_wider(names_from = "Event", values_from="Count")
    
    hitlabel = paste0("Hits (",counts[["Mole Hit"]]  %>% replace(is.null(.), "0"), ")")
    misslabel = paste0("Misses (",counts[["Mole Missed"]]  %>% replace(is.null(.), "0"), ")")
    distlabel = paste0("Distractors (",counts[["Fake Mole Hit"]]  %>% replace(is.null(.), "0"), ")")
    
    new_choices = c(hitlabel, misslabel,distlabel)
    new_choiceValues = c("Mole Hit", "Mole Missed", "Fake Mole Hit")
    
    updateCheckboxGroupInput(session, label = NULL, inputId = "moleFilter", 
                             choiceNames = new_choices, 
                             choiceValues = new_choiceValues, 
                             selected = "Mole Hit", inline = TRUE)
    
    count_lr <- df_c %>% filter(Event %in% c("Mole Hit")) %>%
                         dplyr::mutate(mole_lr = "Center", 
                                       mole_lr = ifelse(MolePositionWorldX > 0, "Right", mole_lr),
                                       mole_lr = ifelse(MolePositionWorldX < 0, "Left", mole_lr)) %>%
                                group_by(mole_lr) %>%
                                dplyr::summarize(Count = n()) %>%
                                pivot_wider(names_from = "mole_lr", values_from="Count")
                         
    leftlabel = paste0("Left (",count_lr[["Left"]]  %>% replace(is.null(.), "0"), ")")
    centerlabel = paste0("Center (",count_lr[["Center"]]  %>% replace(is.null(.), "0"), ")")
    rightlabel = paste0("Right (",count_lr[["Right"]]  %>% replace(is.null(.), "0"), ")")
    
    new_choices = c(leftlabel, centerlabel,rightlabel)
    new_choiceValues = c("Left", "Center", "Right")
    
    updateCheckboxGroupInput(session, label = NULL, inputId = "leftRightFilter", 
                             choiceNames = new_choices, 
                             choiceValues = new_choiceValues, 
                             selected = new_choiceValues, inline = TRUE)
    
  })
  
  output$timelinePlot <- renderPlotly({
    validate(need(df(), "Loading.."), errorClass = "vis")
    # Triggers plot reset when pressing button.
    if (r$reset > 0 ) {
      print(r$reset)
    }
    
    df_vis <- df()
    df_vis <- df_vis %>% dplyr::mutate(mole_lr = "Center", 
                                       mole_lr = ifelse(MolePositionWorldX > 0, "Right", mole_lr),
                                       mole_lr = ifelse(MolePositionWorldX < 0, "Left", mole_lr))
    moles_hit = df_vis %>% filter(Event %in% r$filter) %>% filter(mole_lr %in% r$lrfilter)
    timetemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", xaxis = list(tickformat="ms"))
    fig <- timetemplate %>%
            add_trace(data=moles_hit, x =~GameTimeSpent, y =~MoleActivatedDuration,
            type='scattergl',mode='markers') %>%
            layout(
              yaxis=list(range=c(0,5), title="Hitting Speed"),
              xaxis=list(range=c(0,max(df_vis$GameTimeSpent, na.rm=T) + 10), title="Game Time (seconds)")
            )
    
    return(fig)
  })
  
  
  
}