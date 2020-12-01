
vis_eyePlot <- function(df, selection = NULL, col_time) {
  vistemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
    layout(dragmode = "pan", showlegend = T)
  
  df$vis_e_time = df[[col_time]]
  if (!is.numeric(df$vis_e_time)) {
    df <- df %>% mutate(vis_e_time = as.POSIXlt(vis_e_time, format = "%Y-%m-%d %H:%M:%OS"))
    hoursecs = (df$vis_e_time$hour - df$vis_e_time[1]$hour) * 60 * 60
    minsecs = (df$vis_e_time$min - df$vis_e_time[1]$min) * 60
    secs = (df$vis_e_time$sec - df$vis_e_time[1]$sec)
    df$vis_e_time = hoursecs + minsecs + secs
  }
  
  has_selection = !is.null(selection$x)
  if (has_selection) {
    df = df %>% filter(vis_e_time %in% selection$x)
  }
  
  fig <- vistemplate %>%
    add_trace(name="GAZE", data=df,
              x=~LocalGazeDirectionX, y=~LocalGazeDirectionY, type='scattergl',mode='markers') %>%
    add_trace(name="RIGHT", data=df,
              x=~GazeNormal0X, y=~GazeNormal0Y, type='scattergl',mode='markers', visible = "legendonly") %>%
    add_trace(name="LEFT", data=df,
            x=~GazeNormal1X, y=~GazeNormal1Y, type='scattergl',mode='markers', visible = "legendonly")
  
  axopts <- list(titlefont = list(size=1), showticklabels=F, title = NULL, linecolor = toRGB("black"), linewidth = 1, showline = TRUE, mirror = T, range=c(-0.5,0.5))
  
  fig <- fig %>% layout(
    title = list(xanchor = "right", text= 'GAZE DATA'),
    font = list(size=9),
    xaxis = axopts,
    yaxis = axopts,
    margin=list(l = 1,r = 1,b = 1,t = 20),
    height = 200,
    width = 300
    )
  fig
  return(fig)
}