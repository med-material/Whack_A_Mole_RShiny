individual_gaze_movement_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
      tags$h3("Performance Summary", class="page-header")
    ),
    fluidRow(
      column(6, uiOutput(ns("gaze_lr"))),
      column(6, uiOutput(ns("hemi_lr")))
    ),
    fluidRow(
      tags$h3("3D Gaze Density", class="page-header")
    ),
    fluidRow(
    plot_gaze_density_UI(ns("gaze_density"))
    )
  )
}

individual_gaze_movement <- function(input, output, session, df, meta) {
  ns <- session$ns

  observe({
    req(!is.na(df()))
    req(!is.null(df()))
    callModule(plot_gaze_density, "gaze_density", df)
  })

  output$gaze_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))

    timestamp_start = df() %>% filter(Event == "Game Started") %>% select(Timestamp) %>% slice(1) %>% pull(1)
    
    df_g <- df() %>% filter(Timestamp > timestamp_start)
    aggr = df_g %>% drop_na(WorldGazeHitPositionX) %>%
      dplyr::mutate(gaze_lr = "Center",
                    gaze_lr = ifelse(WorldGazeHitPositionX > 0, "Right", gaze_lr),
                    gaze_lr = ifelse(WorldGazeHitPositionX < 0, "Left", gaze_lr)) %>%
    group_by(gaze_lr) %>% 
      dplyr::summarise(s = sum(row_duration, na.rm=T)) %>%
      pivot_wider(names_from = "gaze_lr", values_from="s")
    
    fallback_text = "No data available."
    if (nrow(aggr) > 0) {
      left_text = paste("<strong>Left Gaze:</strong>",
                                sprintf("%.2f", aggr$Left),
                                 "seconds.")
      right_text = paste("<strong>Right Gaze:</strong>",
                                sprintf("%.2f",aggr$Right),
                                  "seconds.")
    
    progress_left = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",aggr$Left / (aggr$Left + aggr$Right) * 100, "%;"))
    progress_right = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",aggr$Right / (aggr$Left + aggr$Right) * 100, "%;"))
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      left_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_left,
      "</div><br>",
      "<p style='text-align:left;'>",
      right_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_right,
      "</div>"
    ))
    } else {
      ui <- HTML(paste(fallback_text))
    }
    return(ui)
  })  
  
  output$hemi_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    
    timestamp_start = df() %>% filter(Event == "Game Started") %>% select(Timestamp) %>% slice(1) %>% pull(1)
    df_h <- df() %>% filter(Timestamp > timestamp_start)
    aggr = df_h %>%
      drop_na(GazeNormal0X) %>%
      dplyr::mutate(hemi_lr = "Center",
                    hemi_lr = ifelse(GazeNormal0X > 0, "Right", hemi_lr),
                    hemi_lr = ifelse(GazeNormal0X < 0, "Left", hemi_lr)) %>%
      group_by(hemi_lr) %>% 
      dplyr::summarise(Sum = sum(row_duration, na.rm=T)) %>%
      pivot_wider(names_from = "hemi_lr", values_from="Sum")
    fallback_text = "No data available."
    if (nrow(aggr)) {
      left_text = paste("<strong>Left Hemi-Gaze:</strong>",
                        sprintf("%.2f", aggr$Left),
                        "seconds.")
      right_text = paste("<strong>Right Hemi-Gaze:</strong>",
                         sprintf("%.2f", aggr$Right),
                         "seconds.")

      progress_left = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",aggr$Left / (aggr$Left + aggr$Right) * 100, "%;"))
      progress_right = tags$span("",class="mini-progress-bar-fill",style=paste0("width:",aggr$Right / (aggr$Left + aggr$Right) * 100, "%;"))
      
      ui <- HTML(paste(
        "<p style='text-align:left;'>",
        left_text,
        "</p>",
        "<div class='mini-progress-bar' style='width:100px;'>",
        progress_left,
        "</div><br>",
        "<p style='text-align:left;'>",
        right_text,
        "</p>",
        "<div class='mini-progress-bar' style='width:100px;'>",
        progress_right,
        "</div>"
      ))
    } else {
      ui <- HTML(paste(fallback_text))
    }
    return(ui)
  })  
  
}