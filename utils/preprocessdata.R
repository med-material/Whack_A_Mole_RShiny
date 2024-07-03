PreprocessGlobalData <- function(df) {
  D <- df %>% 
    dplyr::mutate(Timestamp = as.POSIXct(Timestamp.Event, format = "%Y-%m-%d %H:%M:%OS")) %>% 
    arrange(Timestamp) %>%
    dplyr::mutate(row_duration = as.numeric(difftime(Timestamp, lag(Timestamp)),units="secs"))

  # Introduce cols that unexpectedly don't exist, init them as NA.
  cols = read.csv("wam_expected_cols.csv", sep=";") %>% select(-type)
  cols = cols %>% filter(!name %in% colnames(D)) 
  if (nrow(cols) > 0) { cols = cols %>% mutate(val = as.numeric(NA)) %>% pivot_wider(names_from = name, values_from = val) }
  D = D %>% bind_cols(cols)
    
  ####
  # HitOrder: Determine what constitutes each player action
  ####
  # we cant use mole expired in multi-mole scenarios, as a consequence we just get longer actions which is fine.
  
  # Create duplicate rows of Mole Hit and Mole Expired, but call them "Action Ended"
  D = D %>% dplyr::mutate(orderflag = 1)
  D_ends = D %>% filter(Event %in% c("Mole Hit")) %>%
    dplyr::mutate(Event = "Hit End", orderflag=0)
  D_begins = D %>% filter(Event %in% c("Mole Hit")) %>%
    dplyr::mutate(Event = "Hit Begin", orderflag=2)
  D = D %>% bind_rows(D_ends) %>% bind_rows(D_begins) %>% arrange(Timestamp,orderflag)
  
  # Create HitOrder from Mole Hit and Mole Expired, but exclude them from HitOrder, which should use Hit Begin and Hit End
  D = D %>% 
    dplyr::mutate(HitOrder = ifelse(Event %in% c("Mole Hit"), 1, 0),
                  HitOrder = cumsum(HitOrder),
                  HitOrder = ifelse(Event %in% c("Mole Hit"), NA, HitOrder),
    )
  
  # consider only moles after the first hit, since we have clear start and end positions for them.
  #D = D %>% filter(HitOrder > 0)
  # Debug
  #D %>% filter(HitOrder == 1, Event %in% c("Mole Spawned","Sample", "Mole Hit","Pointer Hover Begin","Pointer Hover End","Hit End","Hit Begin")) %>% select(ControllerName,Timestamp, Event,MoleId, HitOrder, PatternSegmentLabel) %>% view()
  
  # Move MoleId from HitBegin to "MoleIdStart" and "MoleIdToHit"
  D = D %>% dplyr::group_by(HitOrder) %>% dplyr::mutate(
    MoleIdStart = ifelse(Event == "Hit Begin", MoleId,NA),
    MoleIdToHit = ifelse(Event == "Hit End", MoleId, NA),
    ControllerNameHit = ifelse(Event == "Hit End", ControllerName, NA)
  ) %>% tidyr::fill(MoleIdStart,MoleIdToHit,ControllerNameHit, .direction="downup")
  # Debug
  #D %>% filter(Event %in% c("Mole Spawned","Mole Hit","Pointer Hover Begin","Pointer Hover End","Hit End","Hit Begin")) %>% select(Timestamp, ControllerNameHit,Event,MoleId,MoleIdStart,MoleIdToHit, HitOrder, PatternSegmentLabel) %>% view()
  
  # 0: filter out the first HitOrder, since the users cursor has unknown starting point (no Hit Begin).
  # 1: flag the first action (mole to hit) after each break, so they can be filtered out.
  # important to do before we filter out cols coming before mole spawned, otherwise
  # they cant be identified.
  # 2: filter out actions that never complete (mole spawn, but never is hit, fx often the last mole)
  # 3: filter out actions that were interrupted (game paused)
  D = D %>% dplyr::group_by(HitOrder) %>% dplyr::mutate(
    flag = any(HitOrder == 0),  #0
    HitOrder = ifelse(flag, NA, HitOrder),
    flag = any(Event %in% c("CountDown 0")), #1
    HitOrder = ifelse(flag, NA, HitOrder), 
    flag = any(Event %in% c("Hit Begin")), #2
    flag2 = any(Event %in% c("Hit End")),
    HitOrder = ifelse(flag == flag2, HitOrder, NA), 
    flag = any(Event %in% c("Game Paused")), #3
    HitOrder = ifelse(flag, NA, HitOrder),
  ) %>% select(-flag)
  # Debug
  #D %>% filter(Event %in% c("Mole Spawned","Mole Hit","Pointer Hover Begin","Pointer Hover End","Hit End","Hit Begin")) %>% select(Timestamp, Event,MoleId,MoleIdStart,MoleIdToHit, HitOrder, PatternSegmentLabel) %>% view()
  
  ####
  # Introduce Direction columns that allows us to filter data based on directions.
  ###
  
  D = D %>% dplyr::group_by(HitOrder) %>% 
    dplyr::mutate(
      MoleStartPositionX = case_when(Event == "Hit Begin" ~ MolePositionWorldX),
      MoleEndPositionX = case_when(Event == "Hit End" ~ MolePositionWorldX),
      MoleStartPositionY = case_when(Event == "Hit Begin" ~ MolePositionWorldY),
      MoleEndPositionY = case_when(Event == "Hit End" ~ MolePositionWorldY),
    ) %>% tidyr::fill(MoleStartPositionX,MoleEndPositionX,MoleEndPositionY,MoleStartPositionY, .direction="downup") %>%
    dplyr::mutate(
      HitHDirection = case_when(MoleStartPositionX > MoleEndPositionX ~ "Left",
                                MoleStartPositionX < MoleEndPositionX ~ "Right",
                                TRUE ~ "Vertical"),
      HitVDirection = case_when(MoleStartPositionY > MoleEndPositionY ~ "Down",
                                MoleStartPositionY < MoleEndPositionY ~ "Up",
                                TRUE ~ "Horisontal")
    )
  
  ####
  # Create columns mapping any controller currently hitting things.
  ###
  D = D %>% ungroup() %>% dplyr::mutate(
    ControllerNameHit = ifelse(ControllerNameHit == "Controller (left)", "Left",ControllerNameHit),
    ControllerNameHit = ifelse(ControllerNameHit == "Controller (right)", "Right",ControllerNameHit)
  )
  
  # Fill down ControllerNameHit, since in some cases Mole Hit has no information about ControllerName
  # instead it appears to come from Pointer Shoot?
  D = D %>% tidyr::fill(ControllerNameHit, .direction="down")
  
  
  D = D %>% dplyr::group_by(HitOrder) %>%
    dplyr::mutate(
      ControllerPosWorldX = case_when(ControllerNameHit == "Right" ~ RightControllerPosWorldX, 
                                      ControllerNameHit == "Left" ~ LeftControllerPosWorldX),
      ControllerPosWorldY = case_when(ControllerNameHit == "Right" ~ RightControllerPosWorldY, 
                                      ControllerNameHit == "Left" ~ LeftControllerPosWorldY),
      ControllerLaserPosWorldX = case_when(ControllerNameHit == "Right" ~ RightControllerLaserPosWorldX, 
                                           ControllerNameHit == "Left" ~ LeftControllerLaserPosWorldX),
      ControllerLaserPosWorldY = case_when(ControllerNameHit == "Right" ~ RightControllerLaserPosWorldY, 
                                           ControllerNameHit == "Left" ~ LeftControllerLaserPosWorldY),
    )
  

  
  # important to ungroup everything once preprocessing is over.
  D = D %>% ungroup()
  return(D)
}
