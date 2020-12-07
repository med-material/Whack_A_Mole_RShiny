library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)

# Suppport for Milliseconds
options("digits.secs"=6)

# Requires WallColumnCount, WallRowCount, Event, MoleIndexX, MoleIndexY, MoleSpawnOrder
vis_whackgrid <- function(df = NULL, selection = NULL, col_time, col_streams) {
  #df_tmp <<- df
  #df <- df_tmp
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

  # Make Conversion between wall background and mole positions
  # Make Gaze Positions between -5 and 5 unity meters to be between 0:1.
  wall_Index = df %>% summarise(
    left = min(1, na.rm=T),
    right = max(col_count, na.rm=T),
    up = max(row_count, na.rm=T),
    down = min(1, na.rm=T)
  )
  wall_World = df %>% summarise(
    left = min(MolePositionWorldX, na.rm=T),
    right = max(MolePositionWorldX, na.rm=T),
    up = max(MolePositionWorldY, na.rm=T),
    down = min(MolePositionWorldY, na.rm=T)
  )
    

  fig <- vistemplate %>%
    add_trace(name="Spawn Points", data=Wall_moles,
              x=~x, y=~y, type='scattergl',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none') 
    
  has_selection = !is.null(selection$x)
  if (has_selection) {
    df$wgrid_rowID <- 1:nrow(df)
    df = df %>% filter(vis_w_time %in% selection$x)
  }

  fig <- fig %>%
    add_trace(name="Valid Moles", data=df %>% filter(Event == 'Mole Spawned'),
              x=~MoleIndexX, y=~MoleIndexY, type='scattergl', mode='markers',marker=list(size=32, color='rgba(77, 220, 32, 0.2)')) %>%
    add_trace(name="Fake Moles", data=df %>% filter(Event == 'Fake Mole Spawned'),
              x=~MoleIndexX, y=~MoleIndexY, type='scattergl', mode='markers',marker=list(size=32, color='rgba(240, 77, 66, 0.2)')) %>%
    add_trace(name="MoleSpawnOrder", data=df,
              x=~MoleIndexX, y=~MoleIndexY, type='scattergl', mode='text', text=~MoleSpawnOrder)

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

  if ("ViewportLowerLeftX" %in% col_streams) {
    # Add Viewport Visualization
    #viewport_vis = data.frame(x=c(min(df$ViewportLowerLeftX,na.rm=T),min(df$ViewportLowerMiddleX,na.rm=T),max(df$ViewportLowerRightX,na.rm=T),max(df$ViewportUpperRightX,na.rm=T),min(df$ViewportUpperMiddleX,na.rm=T),min(df$ViewportUpperLeftX,na.rm=T),min(df$ViewportLowerLeftX,na.rm=T)),
    #                          y=c(min(df$ViewportLowerLeftY,na.rm=T),min(df$ViewportLowerMiddleY,na.rm=T),min(df$ViewportLowerRightY,na.rm=T),max(df$ViewportUpperRightY,na.rm=T),max(df$ViewportUpperMiddleY,na.rm=T),max(df$ViewportUpperLeftY,na.rm=T),min(df$ViewportLowerLeftY,na.rm=T)))
    #viewport_vis$text = paste('x:',viewport_vis$x,'y:',viewport_vis$y)
    
    #viewport_vis$x_scaled = scales::rescale(viewport_vis$x,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    #viewport_vis$y_scaled = scales::rescale(viewport_vis$y,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
  
    df$ViewportLowerLeftX_scaled = scales::rescale(df$ViewportLowerLeftX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    df$ViewportLowerLeftY_scaled = scales::rescale(df$ViewportLowerLeftY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    df$ViewportUpperRightX_scaled = scales::rescale(df$ViewportUpperRightX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    df$ViewportUpperRightY_scaled = scales::rescale(df$ViewportUpperRightY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    df$ViewportLowerMiddleX_scaled = scales::rescale(df$ViewportLowerMiddleX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    df$ViewportLowerMiddleY_scaled = scales::rescale(df$ViewportLowerMiddleY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    df$ViewportUpperMiddleX_scaled = scales::rescale(df$ViewportUpperMiddleX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    df$ViewportUpperMiddleY_scaled = scales::rescale(df$ViewportUpperMiddleY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    df$ViewportUpperLeftX_scaled = scales::rescale(df$ViewportUpperLeftX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    df$ViewportUpperLeftY_scaled = scales::rescale(df$ViewportUpperLeftY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    df$ViewportLowerRightX_scaled = scales::rescale(df$ViewportLowerRightX,from=c(wall_World$left,wall_World$right), to=c(wall_Index$left,wall_Index$right))  
    df$ViewportLowerRightY_scaled = scales::rescale(df$ViewportLowerRightY,from=c(wall_World$down,wall_World$up), to=c(wall_Index$down,wall_Index$up))  
    
    fig <- fig %>%
      add_trace(name="ViewportLowerLeft", data=df,x=~ViewportLowerLeftX_scaled,y=~ViewportLowerLeftY_scaled,
                type='scattergl',mode='lines+markers') %>%
      add_trace(name="ViewportLowerMiddle", data=df,x=~ViewportLowerMiddleX_scaled,y=~ViewportLowerMiddleY_scaled,
                type='scattergl',mode='lines+markers') %>%
      add_trace(name="ViewportLowerRight", data=df,x=~ViewportLowerRightX_scaled,y=~ViewportLowerRightY_scaled,
                type='scattergl',mode='lines+markers') %>%
      add_trace(name="ViewportUpperRight", data=df,x=~ViewportUpperRightX_scaled,y=~ViewportUpperRightY_scaled,
                type='scattergl',mode='lines+markers') %>%
      add_trace(name="ViewportUpperMiddle", data=df,x=~ViewportUpperMiddleX_scaled,y=~ViewportUpperMiddleY_scaled,
                type='scattergl',mode='lines+markers') %>%
      add_trace(name="ViewportUpperLeft", data=df,x=~ViewportUpperLeftX_scaled,y=~ViewportUpperLeftY_scaled,
                type='scattergl',mode='lines+markers')
  }
  fig <- fig %>%
    layout(
      xaxis=list(dtick = 1, showticklabels=F, title = NULL, titlefont = list(size=1),range=c(wall_Index$left-1,wall_Index$right+1)),
      yaxis=list(dtick = 1, showticklabels=F, title = NULL, titlefont = list(size=1),range=c(wall_Index$down-1,wall_Index$up+1)),
      margin=list(l = 0,r = 0,b = 0,t = 0,pad = 1)
    )
  #fig
  
  return(fig)
}