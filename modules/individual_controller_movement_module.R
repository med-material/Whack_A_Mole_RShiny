individual_controller_movement_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
    fluidRow(
      tags$h3("Performance Summary", class="page-header")
    ),
    fluidRow(
      column(6, uiOutput(ns("Lcontroller_lr"))),
      column(6, uiOutput(ns("Rcontroller_lr")))
    ),
    fluidRow(
      tags$h3("Controller Density Plot", class="page-header")
    ),
    fluidRow(
    plot_controller_density_UI(ns("controller_density"))
    )
  )
}

individual_controller_movement <- function(input, output, session, df, meta) {
  ns <- session$ns

  observe({
    req(!is.na(df()))
    req(!is.null(df()))
    callModule(plot_controller_density, "controller_density", df)
  })

  output$Lcontroller_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))

    timestamp_start = df() %>% filter(Event == "Game Started") %>% select(Timestamp) %>% slice(1) %>% pull(1)
    
    df_g <- df() %>% filter(Timestamp > timestamp_start)
    aggr = df_g %>% drop_na(LeftControllerPosWorldX) %>%
      dplyr::mutate(lr = "Center",
                    lr = ifelse(LeftControllerPosWorldX > 0, "Right", lr),
                    lr = ifelse(LeftControllerPosWorldX < 0, "Left", lr)) %>%
    group_by(lr) %>% 
      dplyr::summarise(s = sum(row_duration, na.rm=T)) %>%
      pivot_wider(names_from = "lr", values_from="s")
    
    fallback_text = "No data available."
    if (nrow(aggr) > 0) {
      left_text = paste("<strong>Left Controller L :</strong>",
                                sprintf("%.2f", aggr$Left),
                                 "seconds.")
      right_text = paste("<strong>Right Controller L:</strong>",
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
  
  output$Rcontroller_lr <- renderUI({
    req(!is.na(df()))
    req(!is.null(df()))
    
    timestamp_start = df() %>% filter(Event == "Game Started") %>% select(Timestamp) %>% slice(1) %>% pull(1)
    df_h <- df() %>% filter(Timestamp > timestamp_start)
    aggr = df_h %>%
      drop_na(RightControllerPosWorldX) %>%
      dplyr::mutate(lr = "Center",
                    lr = ifelse(RightControllerPosWorldX > 0, "Right", lr),
                    lr = ifelse(RightControllerPosWorldX < 0, "Left", lr)) %>%
      group_by(lr) %>% 
      dplyr::summarise(Sum = sum(row_duration, na.rm=T)) %>%
      pivot_wider(names_from = "lr", values_from="Sum")
    fallback_text = "No data available."
    if (nrow(aggr)) {
      left_text = paste("<strong>Left Controller R:</strong>",
                        sprintf("%.2f", aggr$Left),
                        "seconds.")
      right_text = paste("<strong>Right Controller R:</strong>",
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