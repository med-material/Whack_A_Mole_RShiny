library(lubridate)
library(shinyjs)
library(plotly)

options("digits.secs"=6)

plot_action_grid_performance_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
         checkboxGroupInput(ns("directionFilter"), label = "Travel Directions",
                            choices = c("\u2190" = "Left","\u2192" = "Right","\u2191" = "Up","\u2193" = "Down", ""),
                            selected = c("Left", "Up"), inline = TRUE),
         radioButtons(ns("viewFilter"), label = " ",
                      choices = c("Lines", "Trajectories"),
                      selected = c("Lines"), inline = TRUE),
         radioButtons(ns("controllerFilter"), label = "Choose Controller",
                      choices = c("Left Hand" = "Left","Right Hand" = "Right"),
                      selected = c("Right"), inline = TRUE),
         #sliderInput("range", "Time ", min = 0, max = 50, value = 0, step = 10),
         # todo: use a slider to control which action we are currently seeing - limit the number of actions shown at a time to 6-7 fx.
         # include also an option to show all actions at once (off by default)
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

plot_action_grid_performance <- function(input, output, session, df) {
  ns <- session$ns
  
  r <- reactiveValues(filter = c("Left","Up"), reset = 0,
                      view = c("Combined"))
  
  observeEvent(input$directionFilter, {
    r$filter <- input$directionFilter
  })
  
  observeEvent(input$viewFilter, {
    r$view <- input$viewFilter
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
    countsH = df_c %>% group_by(HitHDirection) %>%
      dplyr::summarize(Count = length(unique(HitOrder,na.rm=T))) %>%
      pivot_wider(names_from = "HitHDirection", values_from="Count")
    countsV = df_c %>% group_by(HitVDirection) %>%
      dplyr::summarize(Count = length(unique(HitOrder,na.rm=T))) %>%
      pivot_wider(names_from = "HitVDirection", values_from="Count")
    
    leftlabel = paste0("\u2190 (",countsH[["Left"]]  %>% replace(is.null(.), "0"), ")")
    rightlabel = paste0("\u2192 (",countsH[["Right"]]  %>% replace(is.null(.), "0"), ")")
    uplabel = paste0("\u2191 (",countsV[["Up"]]  %>% replace(is.null(.), "0"), ")")
    downlabel = paste0("\u2193 (",countsV[["Down"]]  %>% replace(is.null(.), "0"), ")")
    
    new_choices = c(leftlabel,rightlabel,uplabel,downlabel)
    new_choiceValues = c("Left", "Right","Up","Down")
    
    updateCheckboxGroupInput(session, label = NULL, inputId = "directionFilter", 
                             choiceNames = new_choices, 
                             choiceValues = new_choiceValues, 
                             selected = c("Left","Up"), inline = TRUE)
    
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
    

    D <- df()
    
    WS = D %>% filter(HitOrder == min(HitOrder,na.rm=T), Event=="Hit Begin") %>%
      dplyr::summarize(x0 = last(WallBoundsXMin),
                       y0 = last(WallBoundsYMin),
                       x1 = last(WallBoundsXMax),
                       y1 = last(WallBoundsXMax),
                       width = last(WallBoundsXMax) -last(WallBoundsXMin),
                       height = last(WallBoundsYMax) -last(WallBoundsYMin))
    
    f = r$filter
    f = c(f,ifelse(any(f == "Left"), "Horisontal",""))
    f = c(f,ifelse(any(f == "Right"), "Horisontal",""))
    f = c(f,ifelse(any(f == "Up"), "Vertical",""))
    f = c(f,ifelse(any(f == "Down"), "Vertical",""))
    
    
    # Filter after we have established in the wall, so the whole wall is represented.
    D = D %>% filter(HitHDirection %in% f, HitVDirection %in% f)
    D = D %>% filter(ControllerNameHit %in% r$controller)
    
    WallMoles = D %>% ungroup() %>% filter(Event %in% c("Mole Created","Mole Spawned")) %>% dplyr::summarise(
      id = MoleId,
      x = MolePositionWorldX,
      y = MolePositionWorldY,
    ) %>% dplyr::distinct() %>% na.omit(.)

    fig_w <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    fig_w <- fig_w %>%
      add_trace(name="Spawn Points", data=WallMoles,
                x=~c(x), y=~y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32, color="#8d9096ff"),hoverinfo='none') %>%
      add_trace(name="Wall Boundary", data=WS,
                x=~c(x0,x0,x1,x1,x0), y=~c(y0,y1,y1,y0,y0), type='scatter',mode='lines',line=list(width=1,color="#8d9096ff"),hoverinfo='none')
        
    Moles = D %>% group_by(HitOrder) %>% filter(Event %in% c("Hit End")) %>% dplyr::summarise(
      start_x = MoleStartPositionX,
      start_y = MoleStartPositionY,
      end_x = MoleEndPositionX,
      end_y = MoleEndPositionY
    ) %>% na.omit(.)
    
    MoleLinesTraj = D %>% group_by(HitOrder) %>% filter(Event %in% c("Sample")) %>%
     mutate(
     order = 1,
     x = ControllerLaserPosWorldX,
     y = ControllerLaserPosWorldY,
    ) %>% select(HitOrder,order,x,y) %>% na.omit(.)
    
    # Ensure no coordinates go beyond wall boundaries. When they do it is most likely due to glitching.
    MoleLinesTraj = MoleLinesTraj %>% filter(x < WS$x1, x > WS$x0, y < WS$y1, y > WS$y0)
    
    MoleLinesTraj = D %>% group_by(HitOrder) %>% dplyr::summarise(
      order = 2,
      x = NA,
      y = NA,
    ) %>% bind_rows(MoleLinesTraj)
    
    MoleLinesTraj = MoleLinesTraj %>% arrange(HitOrder,order)
    
    linecolor = '#1e7bb7ff'
    overlaycolor = '#06477635'
    
    MoleLinesEUC = tibble(
      ax = Moles$start_x,
      ay = Moles$start_y,
      x = Moles$end_x,
      y = Moles$end_y,
      text = as.character(Moles$HitOrder),
      showarrow = TRUE,
      arrowhead = 2,
      arrowsize = 2,
      arrowwidth = 3,
      arrowcolor = ifelse(r$view =="Trajectories", '#06477635','#1e7bb7b5'),
      xref = 'x', yref = 'y',
      axref = 'x', ayref = 'y',
    )
    
    
    
    # Define custom color scale
    #mole_scale = list(c(0, 'rgba(77, 220, 32, 0.4)'), list(0.5,'rgba(242, 152, 11,0.4)'), list(1,'rgba(240, 77, 66,0.4)'))
    
    # Only show data if we have it.  
    fig_w = fig_w %>%
        add_trace(name="End", data=Moles,
                  x=~end_x, y=~end_y, type='scatter', mode='markers',
                  marker=list(size=32,color="rgba(77, 220, 32, 0.4)",line=list(width=1,color="6dbe4cff"))) %>%
        add_trace(name="Start", data=Moles,
              x=~start_x, y=~start_y, type='scatter', mode='markers', 
              marker=list(size=32,color="rgba(255, 255, 255, 0.05)",line=list(width=3,color="6dbe4cff"))) %>%
        layout(annotations = purrr::transpose(MoleLinesEUC))
    
    if (r$view == "Trajectories") {
      fig_w = fig_w %>% add_trace(name="Trajectory", data=MoleLinesTraj,
                x=~x, y=~y, type='scatter', mode='lines', hoverinfo='text',
                hovertext=~paste0('Action: ',round(HitOrder)),
                line=list(width=2.15,color='#1e7bb7ff'))
    }
    
    fig <- fig_w %>%
      layout(yaxis=list(range=c(WS$y0,WS$y1),zeroline=F,titlefont = list(size=0), title=" "), xaxis=list(range=c(WS$x0,WS$x1), zeroline=F,titlefont = list(size=0), title=" "))
    
    return(fig)
  })
  
  
  
}