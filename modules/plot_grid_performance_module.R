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
        radioButtons(ns("controllerFilter"), label = "Choose Controller",
                     choices = c("Left Hand" = "Left","Right Hand" = "Right"),
                     selected = c("Right"), inline = TRUE),
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
  observeEvent(input$controllerFilter, {
    r$controller <- input$controllerFilter
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
    controllers = na.omit(unique(df_c$ControllerNameHit))
    if (length(controllers) > 1) {
      shinyjs::show("controllerFilter")
      updateCheckboxGroupInput(session, label = NULL, inputId = "controllerFilter", 
                               selected = "Right", inline = TRUE)
    } else {
      shinyjs::hide("controllerFilter")
      updateCheckboxGroupInput(session, label = NULL, inputId = "controllerFilter", 
                               selected = controllers, inline = TRUE)
    }
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
    
    df_vis = df_vis %>% filter(ControllerNameHit %in% r$controller)
    WallMoles = df_vis %>% ungroup() %>% filter(Event %in% c("Mole Created","Mole Spawned")) %>% dplyr::summarise(
      id = MoleId,
      x = MolePositionWorldX,
      y = MolePositionWorldY,
    ) %>% dplyr::distinct() %>% na.omit(.)
    
    # Create wall background
    #col_count = df_vis %>% filter(!is.na(WallColumnCount)) %>% select(WallColumnCount)
    #row_count = df_vis %>% filter(!is.na(WallRowCount)) %>% select(WallRowCount)
    #Wall_moles <- expand.grid(1:tail(col_count, n=1)[,1], 1:tail(row_count, n=1)[,1]) %>%
    #  dplyr::rename(x = Var1, y = Var2)
    
    WS = df_vis %>% filter(HitOrder == min(HitOrder,na.rm=T), Event=="Hit Begin") %>%
      dplyr::summarize(x0 = last(WallBoundsXMin),
                       y0 = last(WallBoundsYMin),
                       x1 = last(WallBoundsXMax),
                       y1 = last(WallBoundsXMax),
                       width = last(WallBoundsXMax) -last(WallBoundsXMin),
                       height = last(WallBoundsYMax) -last(WallBoundsYMin))
    
    fig <- vistemplate %>%
      add_trace(name="Spawn Points", data=WallMoles,
                x=~c(x), y=~y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32, color="#8d9096ff"),hoverinfo='none') %>%
      add_trace(name="Wall Boundary", data=WS,
                x=~c(x0,x0,x1,x1,x0), y=~c(y0,y1,y1,y0,y0), type='scatter',mode='lines',line=list(width=1,color="#8d9096ff"),hoverinfo='none')

    
    
    # Create filtered dataset based on user selection
    df_moles = df_vis %>% filter(Event %in% r$filter)
    
    wall_perf = df_moles %>%
      group_by(MoleId) %>% dplyr::summarise(x = first(MolePositionWorldX),
                                            y = first(MolePositionWorldY),
                                            speed = mean(MoleActivatedDuration, na.rm=T))
    
    # Define custom color scale
    mole_scale = list(c(0, 'rgba (77, 220, 32, 0.4)'), list(0.5,'rgba(242, 152, 11,0.4)'), list(1,'rgba(240, 77, 66,0.4)'))
    
    # Only show data if we have it.  
    if (nrow(wall_perf) > 0) {
      fig <- fig %>%
        add_trace(name="Valid Moles", data=wall_perf,
                x=~x, y=~y, type='scatter', mode='markers',
                marker=list(size=32, color=~speed,colorscale=mole_scale)) %>%
        add_trace(name="Valid Moles", data=wall_perf,
                  x=~x, y=~y, type='scatter', mode='text',
                  text=~sprintf("%.2f",speed), textfont = list(color = '#000000', size = 8))
    }
    
    fig <- fig %>%
      layout(yaxis=list(zeroline=F,titlefont = list(size=0), title=" "), xaxis=list(zeroline=F,titlefont = list(size=0), title=" "))
    
    return(fig)
  })
  
  
  
}