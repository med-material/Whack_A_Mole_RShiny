player_overview_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
      column(6,
             uiOutput(ns("player_header"))
             )
    ),
    fluidRow(
      column(3,
             tags$h3("Session Info"),
             tableOutput(ns("session_info"))
             ),
      column(3,
             tags$h3("Session Performance"),
             fluidRow(column(6,uiOutput(ns("moles_whacked"))),
                      column(6,uiOutput(ns("mole_speed"))),
                      )
            )
    ),
    fluidRow(
      column(3,
             tags$h3("Player Characteristics"),
             tableOutput(ns("profile_info"))
      ),
      column(3,
             tags$h3("Play History"),
             tableOutput(ns("play_history"))
      ),
    )
  )
}

player_overview <- function(input, output, session, df, meta) {
  ns <- session$ns
  
  # Calculating Whack Speed:
  # Median reaction time for each mole.
  

  
  output$moles_whacked <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    moleEvents = c("Mole Spawned", "Mole Hit")
    mole_aggr = df() %>%
      filter(Event %in% moleEvents) %>%
      group_by(Event) %>% 
      dplyr::summarise(Count = n()) %>% pivot_wider(names_from = "Event", values_from="Count")
    mole_aggr[["Mole Hit"]] / mole_aggr[["Mole Spawned"]]
    
    mole_hit_text = "0"
    if (length(mole_aggr[["Mole Hit"]]) > 0) {
      mole_hit_text = mole_aggr[["Mole Hit"]]
    }
    mole_spawned_text = "0"
    if (length(mole_aggr[["Mole Spawned"]]) > 0) {
      mole_spawned_text = mole_aggr[["Mole Spawned"]]
    }
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      mole_hit_text,
      "&#47;",
      mole_spawned_text,
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Moles Hit",
      "</span>",
      "</p>"
    ))
    return(ui)
  })
  
  output$mole_speed <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    moles_hit = df() %>% filter(Event == "Mole Hit")
    speed_text = "0"
    if (nrow(moles_hit) > 0) {
      speed = moles_hit %>% dplyr::summarise(speed = mean(MoleActivatedDuration, na.rm=T))
      speed_text = paste(sprintf("%.2f",speed),"s")
    }
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      speed_text,
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Avg. Speed",
      "</span>",
      "</p>"
    ))
    return(ui)
  })
  
  output$session_info <- renderTable(colnames = FALSE, {
    validate(need(!is.na(df()),"No session data available."))
    req(!is.null(df()))
    
    eyetracking = "Not Available"
    if (length(unique(df()$GazeConfidence, na.rm=T)) > 1) { eyetracking = "On" }
    
    controller = "Unknown"
    controller_left = df() %>% filter(MotorSpaceName == "MotorSpaceL")
    controller_right = df() %>% filter(MotorSpaceName == "MotorSpaceR")
    if (nrow(controller_left) > 0) { controller = "Left" }
    if (nrow(controller_right) > 0) { controller = "Right" }
    if (nrow(controller_left) > 0 && nrow(controller_right) > 0) { controller = "Both" }

    table <- tibble(x = c(), y = c())
     table <- table %>% add_row(x = "Treatment Program:", y = as.character(df()[1,]$SessionProgram)) %>%
       add_row(x = "Duration:", y = as.character(df()[1,]$SessionDuration)) %>%
       add_row(x = "Eye-tracking:", y = as.character(eyetracking)) %>%
       add_row(x = "Main Controller:", y = as.character(controller))
    
    return(table)
  })
  
  output$player_header <- renderUI({
    validate(need(!is.na(df()),"No session data available."))
    req(!is.null(df()))
    
    ui <- HTML(paste(
      "<span style=\"font-size: 24px;\">",
      "<span class=\"fa fa-user fa-2x\" style=\"margin-right: 10px;\"></span>",
      as.character(df()[1,]$ProfileName),
      "</span>"
    ))
    return(ui)
  })
  
  output$profile_info <- renderTable(colnames = FALSE,{
    validate(need(!is.na(df()),"No session data available."))
    req(!is.null(df()))
    
    injuryDate = as.character(df()[1,]$InjuryDate)
    if (is.na(injuryDate)) { injuryDate = "None"}
    profileGroup = as.character(df()[1,]$ProfileGroup)
    if (is.na(profileGroup)) { profileGroup = "None"}
    
    table <- tibble(x = c(), y = c())
    table <- table %>% add_row(x = "Age:", y = as.character(df()[1,]$Age)) %>%
              add_row(x = "Gender:", y = as.character(df()[1,]$Gender)) %>%
              add_row(x = "Handedness:", y = as.character(df()[1,]$Handedness)) %>%
              add_row(x = "InjuryDate:", y = as.character(injuryDate)) %>%
              add_row(x = "Group:", y = as.character(profileGroup)) %>%
              add_row(x = "Participant ID:", y = as.character(df()[1,]$ParticipantID)) %>%
              add_row(x = "Test ID:", y = as.character(df()[1,]$TestID))
                        
    return(table)
  })
 
  output$play_history <- renderTable(colnames = FALSE,{
    validate(need(!is.na(meta()),"No meta data available."))
    req(!is.null(meta()))
    
    profile_id = df()[1,]$ProfileID
    prev_sessions <- meta() %>% filter(ProfileID == profile_id) %>% select(Timestamp) 
    prev_sessions <- prev_sessions %>%
      mutate(p_wday = wday(as.character(prev_sessions$Timestamp), abbr = F, label=T),
             p_date = format(date(as.character(prev_sessions$Timestamp)), "%d %b %Y"),
             p_time = sprintf("%02d:%02d", hour(as.character(prev_sessions$Timestamp)), minute(as.character(prev_sessions$Timestamp))),
             p_timestamp = paste(p_wday, p_date, p_time))
    
    return(prev_sessions$p_timestamp)
  })
  
}