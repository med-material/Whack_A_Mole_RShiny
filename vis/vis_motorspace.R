
vis_motorspace <- function(df, selection = NULL, col_time) {
  df_tmp <<- df
  vistemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
    layout(dragmode = "pan", showlegend = T)
  
  ms_names <- as.character(unique(df$MotorSpaceName))
  ms_names <- ms_names[!is.na(ms_names)]
  motorspace = data.frame()
  fig <- vistemplate
  for (ms_name in ms_names) {
    motorspace_new = df %>% filter(MotorSpaceName == ms_name, !is.na(MotorSpaceCenterPositionX)) %>% summarise(
      center_x = tail(MotorSpaceCenterPositionX, n=1),
      center_y = tail(MotorSpaceCenterPositionY, n=1),
      center_z = tail(MotorSpaceCenterPositionZ, n=1),
      width = tail(MotorSpaceWidth, n=1),
      height = tail(MotorSpaceHeight, n=1),
      multiplier = tail(MotorSpaceMultiplier, n=1),
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

  has_selection = !is.null(selection$x)
  if (has_selection) {
    
    df$vis_mo_time = df[[col_time]]
    if (!is.numeric(df$vis_mo_time)) {
      df <- df %>% mutate(vis_mo_time = as.POSIXlt(vis_mo_time, format = "%Y-%m-%d %H:%M:%OS"))
      hoursecs = (df$vis_mo_time$hour - df$vis_mo_time[1]$hour) * 60 * 60
      minsecs = (df$vis_mo_time$min - df$vis_mo_time[1]$min) * 60
      secs = (df$vis_mo_time$sec - df$vis_mo_time[1]$sec)
      df$vis_mo_time = hoursecs + minsecs + secs
    }
    
    df = df %>% filter(vis_mo_time %in% selection$x)
  }

  
  df_right <- df %>% filter(!RightControllerPosWorldX %in% c(NA, 0))
  if (length(df_right) > 0) {
  fig <-  fig %>% 
      add_trace(name="RightController", data=df_right,
                x=~RightControllerPosWorldX, y=~RightControllerPosWorldY, type='scattergl', mode='lines+markers')
  }
  df_left <- df %>% filter(!LeftControllerPosWorldX %in% c(NA, 0))
  if (length(df_left) > 0) {
  fig <- fig %>% 
      add_trace(name="LeftController", data=df_left,
                x=~LeftControllerPosWorldX, y=~LeftControllerPosWorldY, type='scattergl', mode='lines+markers')
  }

  axX <- list(titlefont = list(size=1), showticklabels=T, title = NULL, linecolor = toRGB("black"), linewidth = 1, showline = TRUE, mirror = T, range=c(min(motorspace$left)-0.1,max(motorspace$right)+0.1))  
  axY <- list(titlefont = list(size=1), showticklabels=T, title = NULL, linecolor = toRGB("black"), linewidth = 1, showline = TRUE, mirror = T, scaleanchor = "x", scaleratio = 1, range=c(min(motorspace$down)-0.1,max(motorspace$up)+0.1))
  
  fig <- fig %>% layout(
    title = list(text= 'MOTORSPACE'),
    showlegend=F,
    font = list(size=9),
    xaxis = axX,
    yaxis = axY,
    margin=list(l = 1,r = 1,b = 1,t = 20),
    height = 200,
    width = 300
  )
  
  return(fig)
}