plot_controller_density_UI <- function(id) {
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

plot_controller_density <- function(input, output, session, df) {
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
    
    df_l <- df() %>% drop_na(LeftControllerPosWorldX) %>% filter(LeftControllerPosWorldX != 0)
    df_r <- df() %>% drop_na(RightControllerPosWorldX) %>% filter(RightControllerPosWorldX != 0)
    
    fig <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    if ('Left' %in% r$filter) {
      validate(need(nrow(df_l) > 0, "No Data Available."), errorClass = "vis")
      density_l <- density(df_l$LeftControllerPosWorldX, adjust=0.50)
      median_l = tibble(x = c(median(df_l$LeftControllerPosWorldX),median(df_l$LeftControllerPosWorldX)),
                      y = c(0,max(density_l$y)))
      
      fig <- fig %>%
        add_trace(name="Density", x=~density_l$x, y=~density_l$y, type='scatter',mode='lines', color=I('black')) %>%
        add_trace(name="Median", x=~median_l$x, y=~median_l$y, type='scatter', mode='lines', line=list(dash='dot'), color=I('black'))

    }
    
    if ('Right' %in% r$filter) {
      validate(need(nrow(df_r) > 0, "No Data Available."), errorClass = "vis")
      density_r <- density(df_r$RightControllerPosWorldX, adjust=0.50)
      median_r = tibble(x = c(median(df_r$RightControllerPosWorldX),median(df_r$RightControllerPosWorldX)),
                        y = c(0,max(density_r$y)))
      
      fig <- fig %>%
        add_trace(name="Density", x=~density_r$x, y=~density_r$y, type='scatter',mode='lines', color=I('black')) %>%
        add_trace(name="Median", x=~median_r$x, y=~median_r$y, type='scatter', mode='lines', line=list(dash='dot'), color=I('black'))
    }
    
    fig <- fig %>%
      layout(xaxis=list(range=c(-2.5,2.5), tickmode = 'linear', dtick=1, title="Controller Movement", ticksuffix='m'),
             yaxis=list(showticklabels=F, title=''))

    return(fig)
  })  
  
}