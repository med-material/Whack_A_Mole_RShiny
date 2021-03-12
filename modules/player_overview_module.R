player_overview_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    column(3,
           tags$h3("Session Info"),
           uiOutput(ns("session_info"))
           ),
    column(3,
           tags$h3("Performance"),
           fluidRow(column(6,uiOutput(ns("moles_whacked"))),
                    column(6,uiOutput(ns("mole_speed"))),
                    )
          )
  )
}

player_overview <- function(input, output, session, df) {
  ns <- session$ns
  
  output$profile_info <- renderUI({
    HTML(paste(
      "<h3>",
      "Profile",
      "</h2>",
      "Name: John John",
      "<br>",
      "E-mail: Johnson@johnson"
    ))
  })  
  
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
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      mole_aggr[["Mole Hit"]],
      "&#47;",
      mole_aggr[["Mole Spawned"]],
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
    speed = df() %>%
      filter(Event == "Mole Hit") %>%
      dplyr::summarise(speed = mean(MoleActivatedDuration))
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      sprintf("%.2f",speed),
      "s</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Avg. Speed",
      "</span>",
      "</p>"
    ))
    return(ui)
  })
  
  output$session_info <- renderUI({
    validate(need(!is.na(df()),"No session data available."))
    req(!is.null(df()))
    
    eyetracking = "Not Available"
    if (length(unique(df()$GazeConfidence, na.rm=T)) > 1) { eyetracking = "On" }
    
    controller = "Unknown"
    controller_left = df() %>% filter(Event == "Controller Main Set Left")
    controller_right = df() %>% filter(Event == "Controller Main Set Right")
    controller_both = df() %>% filter(Event == "Controller Main Set Both")
    if (nrow(controller_left) > 0) { controller = "Left" }
    if (nrow(controller_right) > 0) { controller = "Right" }
    if (nrow(controller_both) > 0) { controller = "Both" }
    
    ui <- HTML(paste(
      "Treatment Program:",
      min(as.character(df()$PlayedPattern), na.rm=T),
      "<br>",
      "Duration:",
      max(as.character(df()$GameDuration), na.rm=T),
      "<br>",
      "Eye-tracking:",
      eyetracking,
      "<br>",
      "Main Controller:",
      controller
    ))
    return(ui)
  })  
}