plot_controller_motorspace_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
               actionButton(ns("resetPlot"), "Reset Plot"),
               checkboxGroupInput(ns("controllerFilter"), label = " ",
                                  choices = c("Left Controller" = "Left", "Right Controller" = "Right"),
                                  selected = c("Right"), inline = TRUE),
      )
    ),
    fluidRow(class="vis-plot",
      plotlyOutput(ns("densityPlot")),
    )
  )
}

plot_controller_motorspace <- function(input, output, session, df) {
  ns <- session$ns

  r <- reactiveValues(filter = c("Right"), reset = 0)
  
  observeEvent(input$controllerFilter, {
    r$filter <- input$controllerFilter
    
  })
  
  observeEvent(input$resetPlot, {
    r$reset = r$reset + 1
  })
  
  observeEvent(df, {
    req(!is.na(df()))
    req(!is.null(df()))
    df_c <- df()

    df_l <- df() %>% drop_na(LeftControllerPosWorldX) %>% filter(LeftControllerPosWorldX != 0)
    df_r <- df() %>% drop_na(RightControllerPosWorldX) %>% filter(RightControllerPosWorldX != 0)
    
    new_choices = c()
    new_choiceValues = c()
    
    if (nrow(df_l) > 0) {
      new_choices = c(new_choices, "Left Controller")
      new_choiceValues = c(new_choiceValues, "Left")
    }
    if (nrow(df_r) > 0) {
      new_choices = c(new_choices, "Right Controller")
      new_choiceValues = c(new_choiceValues, "Right")
    }
    updateCheckboxGroupInput(session, label = NULL, inputId = "controllerFilter", 
                             choiceNames = new_choices, 
                             choiceValues = new_choiceValues, 
                             selected = new_choiceValues, inline = TRUE)
  })
  
  output$densityPlot <- renderPlotly({
    req(!is.na(df()))
    req(!is.null(df()))
    
    if (r$reset > 0 ) {
      print(r$reset)
    }

    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = T)
    
    df <- df()
    
    ms_names <- as.character(unique(df$MotorSpaceName))
    ms_names <- ms_names[!is.na(ms_names)]
    motorspace = data.frame()
    fig <- vistemplate
    for (ms_name in ms_names) {
      motorspace_new = df %>% filter(MotorSpaceName == ms_name, !is.na(MotorSpaceCenterPositionX)) %>% summarise(
        width = tail(MotorSpaceWidth, n=1),
        height = tail(MotorSpaceHeight, n=1),
        multiplier = tail(MotorSpaceMultiplier, n=1),
        center_x = tail(MotorSpaceCenterPositionX, n=1),
        center_y = tail(MotorSpaceCenterPositionY + height, n=1),
        center_z = tail(MotorSpaceCenterPositionZ, n=1),
        left = center_x - (width * multiplier),
        right = center_x + (width * multiplier),
        up = center_y + (height * multiplier),
        down = center_y - (height * multiplier)
        
      )
      motorspace = motorspace %>% bind_rows(motorspace_new)
      new_ms = data.frame(x = c(motorspace_new$left, motorspace_new$left, motorspace_new$right, motorspace_new$right, motorspace_new$left),
                          y=c(motorspace_new$up, motorspace_new$down, motorspace_new$down, motorspace_new$up, motorspace_new$up))
      new_ms$name = ms_name
      fig <- fig %>%
        add_trace(name=~name, data=new_ms,
                  x=~x, y=~y, type='scattergl',mode='lines')
    }

    df_left <- df() %>% drop_na(LeftControllerPosWorldX) %>% filter(LeftControllerPosWorldX != 0)
    df_right <- df() %>% drop_na(RightControllerPosWorldX) %>% filter(RightControllerPosWorldX != 0)
    
    if ('Right' %in% r$filter) {
      fig <-  fig %>% 
        add_trace(name="RightController", data=df_right,
                  x=~RightControllerPosWorldX, y=~RightControllerPosWorldY, type='scattergl', mode='lines+markers')
    }

    if ('Left' %in% r$filter) {
      fig <- fig %>% 
        add_trace(name="LeftController", data=df_left,
                  x=~LeftControllerPosWorldX, y=~LeftControllerPosWorldY, type='scattergl', mode='lines+markers')
    }
    
    axX <- list(titlefont = list(size=1), showticklabels=T, title = NULL, linecolor = toRGB("black"), linewidth = 1, showline = TRUE, mirror = T, range=c(min(motorspace$left)-0.1,max(motorspace$right)+0.1))  
    axY <- list(titlefont = list(size=1), showticklabels=T, title = NULL, linecolor = toRGB("black"), linewidth = 1, showline = TRUE, mirror = T, scaleanchor = "x", scaleratio = 1, range=c(min(motorspace$down)-0.1,max(motorspace$up)+0.1))
    
    fig <- fig %>% layout(
      title = list(text= ' '),
      showlegend=F,
      font = list(size=9),
      xaxis = axX,
      yaxis = axY,
      margin=list(l = 1,r = 1,b = 1,t = 20)
    )
  
    return(fig)
  })  
  
}