library(dplyr)
vis_directionTable <- function(df, col_time) {
  #df_tmp <<- df
  #df <- df_tmp
  #df <- df_tmp
  
  df$vis_t_time = df[[col_time]]
  if (!is.numeric(df$vis_t_time)) {
    df <- df %>% mutate(vis_t_time = as.POSIXlt(vis_t_time, format = "%Y-%m-%d %H:%M:%OS"))
    hoursecs = (df$vis_t_time$hour - df$vis_t_time[1]$hour) * 60 * 60
    minsecs = (df$vis_t_time$min - df$vis_t_time[1]$min) * 60
    secs = (df$vis_t_time$sec - df$vis_t_time[1]$sec)
    df$vis_t_time = hoursecs + minsecs + secs
  }
  
  df_sample <- df %>% filter(Event == "Sample")
  
  # Look Summary (WorldGazeHitPositionX)
  df_lookbin <- df_sample %>% filter(!is.na(WorldGazeHitPositionX)) %>% mutate(
    looked_left_bin = ifelse(WorldGazeHitPositionX < 0,1,0),
    looked_left_change = ifelse(looked_left_bin != lag(looked_left_bin),1,0),
    looked_left_change = ifelse(is.na(looked_left_change), 0, looked_left_change),
    looked_left_cumsum = cumsum(looked_left_change),
    )
  df_looktime <- df_lookbin %>% group_by(looked_left_cumsum) %>% summarise(
   looked_left_bin = min(looked_left_bin),
   looked_time_max =  max(vis_t_time, na.rm=T),
   looked_time_min =  min(vis_t_time, na.rm=T),
   looked_time = looked_time_max - looked_time_min
  )
  df_looksummary <- df_looktime %>% group_by(looked_left_bin) %>% summarise(
    looktime = sum(looked_time)
  )
  
  # Rotation Summary (HeadCameraRotEulerY)
  df_sample <- df_sample %>% mutate(
    HeadCameraRotEulerY_wrap = ifelse(HeadCameraRotEulerY >= 180, HeadCameraRotEulerY-360,HeadCameraRotEulerY)
  )
  df_rotbin <- df_sample %>% filter(!is.na(HeadCameraRotEulerY_wrap)) %>% mutate(
    rotated_left_bin = ifelse(HeadCameraRotEulerY_wrap < 0,1,0),
    rotated_left_change = ifelse(rotated_left_bin != lag(rotated_left_bin),1,0),
    rotated_left_change = ifelse(is.na(rotated_left_change), 0, rotated_left_change),
    rotated_left_cumsum = cumsum(rotated_left_change),
  )
  df_rottime <- df_rotbin %>% group_by(rotated_left_cumsum) %>% summarise(
    rotated_left_bin = min(rotated_left_bin),
    rotated_time_max =  max(vis_t_time, na.rm=T),
    rotated_time_min =  min(vis_t_time, na.rm=T),
    rotated_time = rotated_time_max - rotated_time_min
  )
  df_rotsummary <- df_rottime %>% group_by(rotated_left_bin) %>% summarise(
    rottime = sum(rotated_time)
  )
  
  table = data.frame(name = c("Game", "Look L", "Look R", "Rot. L", "Rot. R"),
                     value = c(max(df$GameDuration,na.rm=T), df_looksummary$looktime[2], df_looksummary$looktime[1], df_rotsummary$rottime[1], df_rotsummary$rottime[2]))
  names(table) = c("Name", "Qty (secs)")
    
  #  summarise(
  #  looked_left = sum(WorldGazeHitPositionX < WallCenterX),
  #  looked_right = sum(WorldGazeHitPositionX < WallCenterX)
  #) %>% View()
  # we measure the sampling rate.
  # then we count the number of occurences
  # then we multiply by the sampling rate
  
  #df_tmp %>% summarise(
  #  looked_left = sum(WorldGazePositionX > Wall)
  #)
  return(table)
}
