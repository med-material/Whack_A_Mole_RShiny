library(lubridate)
library(shinyjs)
library(plotly)

options("digits.secs"=6)

plot_grid_performance_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
        #actionButton(ns("resetPlot"), "Reset Plot"),
        checkboxGroupInput(ns("moleFilter"), label = " ",
                                     choices = c("Hits" = "Mole Hit", "Misses" = "Mole Expired","Distractors" = "Fake Mole Hit"),
                                     selected = c("Mole Hit"), inline = TRUE),
        #checkboxGroupInput(ns("leftRightFilter"), label = " ",
        #                             choices = c("Left", "Right", "Center"),
        #                             selected = c("Left", "Right", "Center"), inline = TRUE),
      )
    ),
    fluidRow(class="vis-plot",
      plotlyOutput(ns("gridPlot")),
    ),
    fluidRow(
      tags$div(class = "vizcontrols-explainer")
    )
  )
}

plot_grid_performance <- function(input, output, session, df) {
  ns <- session$ns
  
  r <- reactiveValues(filter = c("Mole Hit"), reset = 0)
  
  observeEvent(input$moleFilter, {
    r$filter <- input$moleFilter
    
  })
  
  observeEvent(input$resetPlot, {
    r$reset = r$reset + 1
  })
  
  # Add counts to our checkbox groups.
  observeEvent(df, {
    df_c <- df()
    counts = df_c %>% filter(Event %in% c("Mole Hit", "Mole Expired", "Fake Mole Hit")) %>%
      group_by(Event) %>%
      dplyr::summarize(Count = n()) %>%
      pivot_wider(names_from = "Event", values_from="Count")
    
    hitlabel = paste0("Hits (",counts[["Mole Hit"]]  %>% replace(is.null(.), "0"), ")")
    misslabel = paste0("Misses (",counts[["Mole Expired"]]  %>% replace(is.null(.), "0"), ")")
    distlabel = paste0("Distractors (",counts[["Fake Mole Hit"]]  %>% replace(is.null(.), "0"), ")")
    
    new_choices = c(hitlabel, misslabel,distlabel)
    new_choiceValues = c("Mole Hit", "Mole Expired", "Fake Mole Hit")
    
    updateCheckboxGroupInput(session, label = NULL, inputId = "moleFilter", 
                             choiceNames = new_choices, 
                             choiceValues = new_choiceValues, 
                             selected = "Mole Hit", inline = TRUE)
  })
  
  output$gridPlot <- renderPlotly({
    validate(need(df(), "Loading.."), errorClass = "vis")
    # Triggers plot reset when pressing button.
    if (r$reset > 0 ) {
      print(r$reset)
    }
    
    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    df_vis <- df()

    # Create wall background
    col_count = df_vis %>% filter(!is.na(WallColumnCount)) %>% select(WallColumnCount)
    row_count = df_vis %>% filter(!is.na(WallRowCount)) %>% select(WallRowCount)
    Wall_moles <- expand.grid(1:tail(col_count, n=1)[,1], 1:tail(row_count, n=1)[,1]) %>%
      dplyr::rename(x = Var1, y = Var2)
    
    fig <- vistemplate %>%
      add_trace(name="Spawn Points", data=Wall_moles,
                x=~x, y=~y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none')

    
    # Create filtered dataset based on user selection
    df_moles = df_vis %>% filter(Event %in% r$filter)
    
    wall_perf = df_moles %>%
      group_by(MoleId) %>% dplyr::summarise(MoleIndexX = first(MoleIndexX),
                                            MoleIndexY = first(MoleIndexY),
                                            speed = mean(MoleActivatedDuration, na.rm=T))
    
    # Define custom color scale
    mole_scale = list(c(0, 'rgba (77, 220, 32, 0.4)'), list(0.5,'rgba(242, 152, 11,0.4)'), list(1,'rgba(240, 77, 66,0.4)'))
    
    # Only show data if we have it.  
    if (nrow(wall_perf) > 0) {
      fig <- fig %>%
        add_trace(name="Valid Moles", data=wall_perf,
                x=~MoleIndexX, y=~MoleIndexY, type='scatter', mode='markers',
                marker=list(size=32, color=~speed,colorscale=mole_scale)) %>%
        add_trace(name="Valid Moles", data=wall_perf,
                  x=~MoleIndexX, y=~MoleIndexY, type='scatter', mode='text',
                  text=~sprintf("%.2f",speed), textfont = list(color = '#000000', size = 8))
    }
    
    fig <- fig %>%
      layout(yaxis=list(titlefont = list(size=0), title=" "), xaxis=list(titlefont = list(size=0), title=" "))
    
    return(fig)
  })
  
  
  
}