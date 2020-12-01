library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)

# Suppport for Milliseconds
options("digits.secs"=6)

# Requires WallColumnCount, WallRowCount, Event, MoleIndexX, MoleIndexY, MoleSpawnOrder
vis_whackgrid <- function(df = NULL, selection = NULL, col_time, col_streams) {
  df <<- df
  vistemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
    layout(dragmode = "pan", showlegend = FALSE)
  
  df$vis_w_time = df[[col_time]]
  if (!is.numeric(df$vis_w_time)) {
    df <- df %>% mutate(vis_w_time = as.POSIXlt(vis_w_time, format = "%Y-%m-%d %H:%M:%OS"))
    hoursecs = (df$vis_w_time$hour - df$vis_w_time[1]$hour) * 60 * 60
    minsecs = (df$vis_w_time$min - df$vis_w_time[1]$min) * 60
    secs = (df$vis_w_time$sec - df$vis_w_time[1]$sec)
    df$vis_w_time = hoursecs + minsecs + secs
  }
  
  # Create wall background
  col_count = df %>% filter(!is.na(WallColumnCount)) %>% select(WallColumnCount)
  row_count = df %>% filter(!is.na(WallRowCount)) %>% select(WallRowCount)
  Wall_moles <- expand.grid(1:tail(col_count, n=1)[,1], 1:tail(row_count, n=1)[,1]) %>%
    dplyr::rename(x = Var1, y = Var2)

  fig <- vistemplate %>%
    add_trace(name="Spawn Points", data=Wall_moles,
              x=~x-1, y=~y-1, type='scattergl',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none') 
    
  has_selection = !is.null(selection$x)
  if (has_selection) {
    df$wgrid_rowID <- 1:nrow(df)
    df = df %>% filter(vis_w_time %in% selection$x)
  }
  
  # Make Conversion between wall background and mole positions
  # Make Gaze Positions between -5 and 5 unity meters to be between 0:1.
  wall_Index = df %>% summarise(
    left = min(MoleIndexX-1, na.rm=T),
    right = max(MoleIndexX-1, na.rm=T),
    up = max(MoleIndexY-1, na.rm=T),
    down = min(MoleIndexY-1, na.rm=T)
  )
  wall_World = df %>% summarise(
    left = min(MolePositionWorldX, na.rm=T),
    right = max(MolePositionWorldX, na.rm=T),
    up = max(MolePositionWorldY, na.rm=T),
    down = min(MolePositionWorldY, na.rm=T)
  )
  
  
  fig <- fig %>%
    add_trace(name="Valid Moles", data=df %>% filter(Event == 'Mole Spawned'),
              x=~MoleIndexX-1, y=~MoleIndexY-1, type='scattergl', mode='markers',marker=list(size=32, color='rgba(77, 220, 32, 0.2)')) %>%
    add_trace(name="Fake Moles", data=df %>% filter(Event == 'Fake Mole Spawned'),
              x=~MoleIndexX-1, y=~MoleIndexY-1, type='scattergl', mode='markers',marker=list(size=32, color='rgba(240, 77, 66, 0.2)')) %>%
    add_trace(name="MoleSpawnOrder", data=df,
              x=~MoleIndexX-1, y=~MoleIndexY-1, type='scattergl', mode='text', text=~MoleSpawnOrder) %>%
    layout(
      xaxis=list(dtick = 1, showticklabels=F, title = NULL, titlefont = list(size=1)),
      yaxis=list(dtick = 1, showticklabels=F, title = NULL, titlefont = list(size=1)),
      margin=list(l = 0,r = 0,b = 0,t = 0,pad = 1)
    )
  
  # Add Continous Data
  #col_streams = c("WorldGazeHitPositionX","WorldGazeHitPositionY")
  if (c("WorldGazeHitPositionX","WorldGazeHitPositionY") %in% col_streams) {
    colnameX = paste("WorldGazeHitPositionX","s")
    df[[colnameX]] = scales::rescale(df$WorldGazeHitPositionX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    colnameY = paste("WorldGazeHitPositionY","s")
    df[[colnameY]] = scales::rescale(df$WorldGazeHitPositionY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    dfc <- data.frame(x=df[[colnameX]], y=df[[colnameY]], text=paste("x:",df$WorldGazeHitPositionX,"y:",df$WorldGazeHitPositionY))
    fig <- fig %>%
      add_trace(name="WorldGazeHitPosition", data=dfc, x =~x, y =~y, 
                type='scattergl',mode='markers+lines', hoverinfo='text',text=~text)
  }
  if ( c("MolePositionWorldX","MolePositionWorldY") %in% col_streams) {
    colnameX = paste("MolePositionWorldX","s")
    df[[colnameX]] = scales::rescale(df$MolePositionWorldX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    colnameY = paste("MolePositionWorldY","s")
    df[[colnameY]] = scales::rescale(df$MolePositionWorldY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    dfc <- data.frame(x=df[[colnameX]], y=df[[colnameY]], text=paste("x:",df$MolePositionWorldX,"y:",df$MolePositionWorldY))
    fig <- fig %>%
      add_trace(name="MolePositionWorld", data=dfc, x =~x, y =~y, 
                type='scattergl',mode='markers+lines', hoverinfo='text',text=~text)
  }
  if ( c("RightControllerLaserPosWorldX","RightControllerLaserPosWorldY") %in% col_streams) {
    colnameX = paste("RightControllerLaserPosWorldX","s")
    df[[colnameX]] = scales::rescale(df$RightControllerLaserPosWorldX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    colnameY = paste("RightControllerLaserPosWorldY","s")
    df[[colnameY]] = scales::rescale(df$RightControllerLaserPosWorldY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    dfc <- data.frame(x=df[[colnameX]], y=df[[colnameY]], text=paste("x:",df$RightControllerLaserPosWorldX,"y:",df$RightControllerLaserPosWorldY))
    fig <- fig %>%
      add_trace(name="RightControllerLaserPosWorld", data=dfc, x =~x, y =~y, 
                type='scattergl',mode='markers+lines', hoverinfo='text',text=~text)
  }
  if ( c("LeftControllerLaserPosWorldX","LeftControllerLaserPosWorldY") %in% col_streams) {
    colnameX = paste("LeftControllerLaserPosWorldX","s")
    df[[colnameX]] = scales::rescale(df$LeftControllerLaserPosWorldX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    colnameY = paste("LeftControllerLaserPosWorldY","s")
    df[[colnameY]] = scales::rescale(df$LeftControllerLaserPosWorldY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    dfc <- data.frame(x=df[[colnameX]], y=df[[colnameY]], text=paste("x:",df$LeftControllerLaserPosWorldX,"y:",df$LeftControllerLaserPosWorldY))
    fig <- fig %>%
      add_trace(name="LeftControllerLaserPosWorld", data=dfc, x =~x, y =~y, 
                type='scattergl',mode='markers+lines', hoverinfo='text',text=~text)
  }

  return(fig)
}