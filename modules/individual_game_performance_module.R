individual_game_performance_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
      tags$h3("Performance Summary", class="page-header")
    ),
    fluidRow(
      column(6, uiOutput(ns("moles_whack_lr"))),
      column(6, uiOutput(ns("moles_speed_lr")))
    ),
    fluidRow(
      tags$h3("Spatial Whack Performance", class="page-header"),
      tags$p("Shows the player's hitting performance in seconds on a visual grid
              representing the Whack-A-Mole wall."),
      plot_grid_performance_UI(ns("grid_mole_hit"))
    ),
    fluidRow(
      tags$h3("Whack Performance Over Time", class="page-header"),
      tags$p("Player hitting performance, on a scale between 0 and 5 seconds,
              from game start to finish."),
      plot_scatter_timeline_UI(ns("ind_mole_hit"))
    )
  )
}

individual_game_performance <- function(input, output, session, df, meta) {
  ns <- session$ns
  observe({
    req(!is.na(df()))
    req(!is.null(df()))
    callModule(plot_scatter_timeline, "ind_mole_hit", df)
    callModule(plot_grid_performance, "grid_mole_hit", df)
  })


  output$moles_speed_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    
    moleEvents = c("Mole Hit")
    mole_aggr = df() %>%
      filter(Event %in% moleEvents) %>%
      dplyr::mutate(mole_lr = "Center", 
                    mole_lr = ifelse(MolePositionWorldX > 0, "Right", mole_lr),
                    mole_lr = ifelse(MolePositionWorldX < 0, "Left", mole_lr)) %>%
      group_by(Event, mole_lr) %>% 
      dplyr::summarise(speed = mean(MoleActivatedDuration, na.rm=T)) %>%
      mutate(speed_text = sprintf("%.2f",speed),
             speed_pct = (5 - speed) / 5 * 100 ) %>%
      ungroup()
    #browser()
    #speed = moles_hit %>% dplyr::summarise(speed = mean(MoleActivatedDuration, na.rm=T))
    #speed_text = paste(sprintf("%.2f",speed),"s")
    
    #mole_aggr[["Mole Hit"]] / mole_aggr[["Mole Spawned"]]
    
    mole_hit_text = "No data available."
    if (length(mole_aggr[["Event"]]) > 0) {
      #browser()
      mole_speed_left_text = paste("<strong>Left Speed:</strong>",
                                 mole_aggr %>% filter(mole_lr == "Left") %>% select(speed_text),
                                 "seconds.")
      mole_speed_right_text = paste("<strong>Right Speed::</strong>",
                                  mole_aggr %>% filter(mole_lr == "Right") %>% select(speed_text),
                                  "seconds.")
    
    progress_left = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",mole_aggr %>% filter(mole_lr == "Left") %>% select(speed_pct), "%;"))
    progress_right = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",mole_aggr %>% filter(mole_lr == "Right") %>% select(speed_pct), "%;"))
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      mole_speed_left_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_left,
      "</div><br>",
      "<p style='text-align:left;'>",
      mole_speed_right_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_right,
      "</div>"
    ))
    } else {
      ui <- HTML(paste(mole_hit_text))
    }
    return(ui)
  })  
  
  output$moles_whack_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    moleEvents = c("Mole Spawned", "Mole Hit")
    mole_aggr = df() %>%
      filter(Event %in% moleEvents) %>%
      dplyr::mutate(mole_lr = "Center", 
                    mole_lr = ifelse(MolePositionWorldX > 0, "Right", mole_lr),
                    mole_lr = ifelse(MolePositionWorldX < 0, "Left", mole_lr)) %>%
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