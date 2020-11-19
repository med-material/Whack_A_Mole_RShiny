library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)

# Suppport for Milliseconds
options("digits.secs"=6)

vistemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan")

# The input arguments represent lists of column names.
# col_event: Creates a vertical line with text labels on top for the columns specified.
# col_eventtype: Differentiates the different events.
# col_continuous: this data is plotted between 0 and 1 with connected scatter dots.
# col_period: column to mark periods. NA values differentiate a period start and stop.
vis_timeline <- function(df, col_time, col_event, col_eventtype, col_streams, ignore_event = "") {
  # Debugging
  #df = D
  #ignore_event = c("NoData", "Sample")
  #col_time = "Timestamp"
  #col_event = "Event"
  #col_eventtype = "EventType"
  #col_streams = c("LeftControllerRotEulerZ","RightControllerLaserRotEulerX")
  col_period = "EventColor"
  
  timetemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
    layout(dragmode = "pan", xaxis = list(tickformat="ms"), yaxis = list(range=c(0,1.1)))
  
  if (col_eventtype == "") {
    col_eventtype = col_event
  }
  
  if (ignore_event != "") {
    df[[col_eventtype]] = as.character(df[[col_eventtype]])
    df[[col_eventtype]] <- ifelse(df[[col_eventtype]] %in% ignore_event, NA, df[[col_eventtype]])
  }
  
  # Convert timestamps to something we can use with scattergl.
  df$vis_t_time = df[[col_time]]
  if (!is.numeric(df$vis_t_time)) {
    df <- df %>% mutate(vis_t_time = as.POSIXlt(vis_t_time, format = "%Y-%m-%d %H:%M:%OS"))
    hoursecs = (df$vis_t_time$hour - df$vis_t_time[1]$hour) * 60 * 60
    minsecs = (df$vis_t_time$min - df$vis_t_time[1]$min) * 60
    secs = (df$vis_t_time$sec - df$vis_t_time[1]$sec)
    df$vis_t_time = hoursecs + minsecs + secs
  }
  
  # Calculate Label positions
  num_events <- unique(df[col_eventtype])
  num_events <- num_events %>% drop_na()
  num_events$vis_t_labelpos = 1:nrow(num_events)
  num_events$vis_t_labelpos = scales::rescale(num_events$vis_t_labelpos, to=c(0.1,0.5))
  df <- df %>% left_join(num_events)
  
  # Add Events
  fig <- timetemplate %>% 
    add_segments(name=~df[[col_eventtype]], data=df, type='scattergl',
                 x =~vis_t_time, y=~vis_t_labelpos-0.02, xend=~vis_t_time, yend=0, size=I(1), color=I("Gray")) %>%
    add_trace(name=~df[[col_eventtype]], data=df,
              x =~vis_t_time, y =~vis_t_labelpos, color =~df[[col_eventtype]],
              type='scattergl',mode='text', text=~df[[col_event]], textfont = list(size = 10))

  # Add Continous Data
  for (col_c in col_streams) {
    if (col_c %in% names(df)) {
      colname = paste(col_c,"s")
      df[[col_c]] <- ifelse(df[[col_c]] == -1, NA, df[[col_c]])
      df[[colname]] = scales::rescale(df[[col_c]], to=c(0,1))
      dfc <- data.frame(x=df$vis_t_time, y=df[[colname]], text=df[[col_c]])
      fig <- fig %>%
        add_trace(name=col_c, data=dfc, x =~x, y =~y, 
                  type='scattergl',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~text)
    }
  }
  
  fig <- fig %>% 
  layout(
    yaxis=list(dtick = 0.1,title=" "),
    xaxis=list(title="Time")
  )
  #fig
  return(fig)
}
