individual_game_performance_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
             tags$h3("Whack Performance Over Time"),
             tags$p("Timeline plot. accumulative plot. Under construction..")
             # Call to action:
             # Choose Measurement Type: Moles Hit + Distractors Hit? Speed?
             # Reset Graph (Visible button)
             # Scroll to zoom indicator.
             
             #Call to graph module here.
             #uiOutput(ns("session_info"))
             ),
    fluidRow(
      tags$h3("Whack Performance Left/Right"),
             uiOutput(ns("moles_whack_lr"))
    )
  )
}

individual_game_performance <- function(input, output, session, df, meta) {
  ns <- session$ns
  
  # Calculating Whack Speed:
  # Median reaction time for each mole.
  

  
  output$moles_whack_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    moleEvents = c("Mole Spawned", "Mole Hit")
    mole_aggr = df() %>%
      filter(Event %in% moleEvents) %>%
      dplyr::mutate(mole_lr = ifelse(MolePositionWorldX > 0, "Right", "Left")) %>%
      group_by(Event, mole_lr) %>% 
      dplyr::summarise(Count = n()) %>% pivot_wider(names_from = "Event", values_from="Count") %>%
      mutate(percentage = (`Mole Hit` / `Mole Spawned`) * 100)
    #mole_aggr[["Mole Hit"]] / mole_aggr[["Mole Spawned"]]
    
    mole_hit_text = "No data available."
    if (length(mole_aggr[["Mole Hit"]]) > 0 & length(mole_aggr[["Mole Spawned"]]) > 0) {
      mole_hit_left_text = paste("<strong>Left Wall Side:</strong>",
                            mole_aggr %>% filter(mole_lr == "Left") %>% select("Mole Hit"),
                            "out of",
                            mole_aggr %>% filter(mole_lr == "Left") %>% select("Mole Spawned"),
                            "moles hit (",
                            format(as.numeric(mole_aggr %>% filter(mole_lr == "Left") %>% select("percentage")),digits=2),
                            "%).")
      mole_hit_right_text = paste("<strong>Right Wall Side:</strong>",
                                  mole_aggr %>% filter(mole_lr == "Right") %>% select("Mole Hit"),
                                 "out of",
                                 mole_aggr %>% filter(mole_lr == "Right") %>% select("Mole Spawned"),
                                 "moles hit (",
                                 format(as.numeric(mole_aggr %>% filter(mole_lr == "Right") %>% select("percentage")), digits=2),
                                 "%).")
    }
    progress_left = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",mole_aggr %>% filter(mole_lr == "Left") %>% select("percentage"), "%;"))
    progress_right = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",mole_aggr %>% filter(mole_lr == "Right") %>% select("percentage"), "%;"))
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      mole_hit_left_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_left,
      "</div><br>",
      "<p style='text-align:left;'>",
      mole_hit_right_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_right,
      "</div>"
    ))
    return(ui)
  })
  
}