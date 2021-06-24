plot_head_orientation_density_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(class="vis-plot",
      plotlyOutput(ns("densityPlot")),
    )
  )
}

plot_head_orientation_density <- function(input, output, session, df) {
  ns <- session$ns

  output$densityPlot <- renderPlotly({
    req(!is.na(df()))
    req(!is.null(df()))
    df_h <- df() %>%
      dplyr::mutate(
                    HeadCameraRotEulerY_wrap = ifelse(HeadCameraRotEulerY > 180, HeadCameraRotEulerY - 360, HeadCameraRotEulerY)) %>%
                    drop_na(HeadCameraRotEulerY_wrap)
    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    density <- density(df_h$HeadCameraRotEulerY_wrap, adjust=0.50)
    median = tibble(x = c(median(df_h$HeadCameraRotEulerY_wrap),median(df_h$HeadCameraRotEulerY_wrap)),
                    y = c(0,max(density$y)))
    fig <- vistemplate %>%
      add_trace(name="Density", x=~density$x, y=~density$y, type='scatter',mode='lines', color=I('black')) %>%
      add_trace(name="Median", x=~median$x, y=~median$y, type='scatter', mode='lines', line=list(dash='dot'), color=I('black')) %>%
      layout(xaxis=list(range=c(-90,90), tickmode = 'linear', dtick=10, title="Head Orientation", ticksuffix='&deg;'),
             yaxis=list(showticklabels=F, title=''))
    return(fig)
  })  
  
}