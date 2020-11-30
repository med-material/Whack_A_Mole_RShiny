library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)

# Suppport for Milliseconds
options("digits.secs"=6)

vistemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan")

# The input arguments represent lists of column names.
# col_event: Creates a vertical line with text labels on top for the columns specified.
# col_eventtype: Differentiates the different events.
# col_continuous: this data is plotted between 0 and 1 with connected scatter dots.
# col_period: column to mark periods. NA values differentiate a period start and stop.
vis_timeline_whack <- function(df, col_time, col_event, col_eventtype, col_streams, ignore_event = "") {
  # Debugging
  #df = D
  #ignore_event = c("NoData", "Sample")
  #col_time = "Timestamp"
  #col_event = "Event"
  #col_eventtype = "EventType"
  #col_streams = c("LeftControllerRotEulerZ","RightControllerLaserRotEulerX")
  col_period = "EventColor"
  
  timetemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
    layout(dragmode = "pan", xaxis = list(tickformat="ms"), yaxis = list(range=c(0,1.1)))
  
  if (col_eventtype == "") {
    col_eventtype = col_event
  }
  
  if (ignore_event != "") {
    df[[col_eventtype]] = as.character(df[[col_eventtype]])
    df[[col_eventtype]] <- ifelse(df[[col_eventtype]] %in% ignore_event, NA, df[[col_eventtype]])
  }
  
  # Whack-A-Mole Specific Data Manipulation
  # Make Head X Rotation from 0:360 to -180:180 and rescale it to 0:1.
  # Rotation Pitch (X) is typically at 
  # Rotation Pitch (Y) is typically at 
  # Rotation Pitch (Z) is typically at 
  df <- df %>%
    mutate(HeadCameraRotEulerX = ifelse(HeadCameraRotEulerX >= 180, HeadCameraRotEulerX-360,HeadCameraRotEulerX),
           HeadCameraRotEulerX = scales::rescale(HeadCameraRotEulerX, from=c(-180,180), to=c(0,1)),
           HeadCameraRotEulerY = ifelse(HeadCameraRotEulerY >= 180, HeadCameraRotEulerY-360,HeadCameraRotEulerY),
           HeadCameraRotEulerY = scales::rescale(HeadCameraRotEulerY, from=c(-180,180), to=c(0,1)),
           HeadCameraRotEulerZ = ifelse(HeadCameraRotEulerZ >= 180, HeadCameraRotEulerZ-360,HeadCameraRotEulerZ),
           HeadCameraRotEulerZ = scales::rescale(HeadCameraRotEulerZ, from=c(-180,180), to=c(0,1)))

  # Rescale Head Positions between -5 and 5 unity meters to be between 0:1.
  df <- df %>%
    mutate(HeadCameraPosWorldX = scales::rescale(HeadCameraPosWorldX, from=c(-5,5), to=c(0,1)),
           HeadCameraPosWorldY = scales::rescale(HeadCameraPosWorldY, from=c(-5,5), to=c(0,1)),
           HeadCameraPosWorldZ = scales::rescale(HeadCameraPosWorldZ, from=c(-5,5), to=c(0,1)))
  
  # Rescale Head Positions between -5 and 5 unity meters to be between 0:1.
  df <- df %>%
    mutate(HeadCameraPosWorldX = scales::rescale(HeadCameraPosWorldX, from=c(-5,5), to=c(0,1)),
           HeadCameraPosWorldY = scales::rescale(HeadCameraPosWorldY, from=c(-5,5), to=c(0,1)),
           HeadCameraPosWorldZ = scales::rescale(HeadCameraPosWorldZ, from=c(-5,5), to=c(0,1)))
  
  # Rescale Controller Positions between -5 and 5 unity meters to be between 0:1.
  df <- df %>%
    mutate(RightControllerPosWorldX = scales::rescale(RightControllerPosWorldX, from=c(-5,5), to=c(0,1)),
           RightControllerPosWorldY = scales::rescale(RightControllerPosWorldY, from=c(-5,5), to=c(0,1)),
           RightControllerPosWorldZ = scales::rescale(RightControllerPosWorldZ, from=c(-5,5), to=c(0,1)),
           LeftControllerPosWorldX = scales::rescale(LeftControllerPosWorldX, from=c(-5,5), to=c(0,1)),
           LeftControllerPosWorldY = scales::rescale(LeftControllerPosWorldY, from=c(-5,5), to=c(0,1)),
           LeftControllerPosWorldZ = scales::rescale(LeftControllerPosWorldZ, from=c(-5,5), to=c(0,1)))
  
  # Make Gaze Positions between -5 and 5 unity meters to be between 0:1.
  wall_size = df %>% summarise(
    left_mole = min(MolePositionWorldX, na.rm=T),
    right_mole = max(MolePositionWorldX, na.rm=T),
    up_mole = max(MolePositionWorldY, na.rm=T),
    down_mole = min(MolePositionWorldY, na.rm=T),
    close_mole = min(MolePositionWorldZ, na.rm=T),
    away_mole = max(MolePositionWorldZ, na.rm=T)
  )
    
  df <- df %>%
    mutate(WorldGazeHitPositionX = scales::rescale(WorldGazeHitPositionX, from=c(wall_size$left_mole-1,wall_size$right_mole+1), to=c(0,1)),
           WorldGazeHitPositionY = scales::rescale(WorldGazeHitPositionY, from=c(wall_size$down_mole-1,wall_size$up_mole+1), to=c(0,1)),
           WorldGazeHitPositionZ = scales::rescale(WorldGazeHitPositionZ, from=c(wall_size$close_mole-1,wall_size$away_mole+1), to=c(0,1)))

  # Make Controller Rotation from 0:360 to -180:180 and rescale it to 0:1.
  df <- df %>%
    mutate(RightControllerRotEulerX = ifelse(RightControllerRotEulerX >= 180, RightControllerRotEulerX-360,RightControllerRotEulerX),
           RightControllerRotEulerX = scales::rescale(RightControllerRotEulerX, from=c(-180,180), to=c(0,1)),
           RightControllerRotEulerY = ifelse(RightControllerRotEulerY >= 180, RightControllerRotEulerY-360,RightControllerRotEulerY),
           RightControllerRotEulerY = scales::rescale(RightControllerRotEulerY, from=c(-180,180), to=c(0,1)),
           RightControllerRotEulerZ = ifelse(RightControllerRotEulerZ >= 180, RightControllerRotEulerZ-360,RightControllerRotEulerZ),
           RightControllerRotEulerZ = scales::rescale(RightControllerRotEulerZ, from=c(-180,180), to=c(0,1)),
           LeftControllerRotEulerX = ifelse(LeftControllerRotEulerX >= 180, LeftControllerRotEulerX-360,LeftControllerRotEulerX),
           LeftControllerRotEulerX = scales::rescale(LeftControllerRotEulerX, from=c(-180,180), to=c(0,1)),
           LeftControllerRotEulerY = ifelse(LeftControllerRotEulerY >= 180, LeftControllerRotEulerY-360,LeftControllerRotEulerY),
           LeftControllerRotEulerY = scales::rescale(LeftControllerRotEulerY, from=c(-180,180), to=c(0,1)),
           LeftControllerRotEulerZ = ifelse(LeftControllerRotEulerZ >= 180, LeftControllerRotEulerZ-360,LeftControllerRotEulerZ),
           LeftControllerRotEulerZ = scales::rescale(LeftControllerRotEulerZ, from=c(-180,180), to=c(0,1)))
  
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
                  type='scattergl',mode='markers+lines', hoverinfo='text',text=~text)
    }
  }
  
  fig <- fig %>% event_register("plotly_selected") %>%
  layout(
    yaxis=list(dtick = 0.1,title="", titlefont = list(size=1)),
    xaxis=list(title="Time")
  )
  #fig
  return(fig)
}
