plot_gaze_density_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(class="vis-plot",
      plotlyOutput(ns("densityPlot")),
    )
  )
}

plot_gaze_density <- function(input, output, session, df) {
  ns <- session$ns

  output$densityPlot <- renderPlotly({
    req(!is.na(df()))
    req(!is.null(df()))
    
    df_g <- df() %>% drop_na(WorldGazeHitPositionX)
    validate(need(nrow(df_g) > 0, "No Data Available."), errorClass = "vis")
    
    
    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    density <- density(df_g$WorldGazeHitPositionX, adjust=0.50)
    median = tibble(x = c(median(df_g$WorldGazeHitPositionX),median(df_g$WorldGazeHitPositionX)),
                    y = c(0,max(density$y)))
    fig <- vistemplate %>%
      add_trace(name="Density", x=~density$x, y=~density$y, type='scatter',mode='lines', color=I('black')) %>%
      add_trace(name="Median", x=~median$x, y=~median$y, type='scatter', mode='lines', line=list(dash='dot'), color=I('black')) %>%
      layout(xaxis=list(range=c(-10,10), tickmode = 'linear', dtick=1, title="Gaze On Wall Density", ticksuffix='m'),
             yaxis=list(showticklabels=F, title=''))
    return(fig)
  })  
  
}