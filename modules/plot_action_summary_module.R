library(lubridate)
library(shinyjs)
library(plotly)
library(dplyr)

options("digits.secs"=6)

plot_action_summary_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
        #actionButton(ns("resetPlot"), "Reset Plot"),
        checkboxGroupInput(ns("directionFilter"), label = "Travel Directions",
                                     choices = c("\u2190" = "Left","\u2192" = "Right","\u2191" = "Up","\u2193" = "Down"),
                                     selected = c("Left","Right","Up","Down"), inline = TRUE),
        radioButtons(ns("viewFilter"), label = " ",
                                     choices = c("Combined", "Left-vs-Right","Up-vs-Down"),
                                     selected = c("Combined"), inline = TRUE),
      )
    ),
    fluidRow(class="vis-plot",
      plotlyOutput(ns("actionPlot")),
    )
  )
}

plot_action_summary <- function(input, output, session, df) {
  ns <- session$ns
  
  r <- reactiveValues(filter = c("Left","Right","Up","Down"), reset = 0,
                      view = c("Combined"))
  
  observeEvent(input$directionFilter, {
    r$filter <- input$directionFilter
  })
  
  observeEvent(input$viewFilter, {
    
    r$view <- input$viewFilter
  })
  
  observeEvent(input$resetPlot, {
    r$reset = r$reset + 1
  })
  
  # Add counts to our checkbox groups.
  observeEvent(df, {
    df_c <- df()
    countsH = df_c %>% group_by(HitHDirection) %>%
      dplyr::summarize(Count = length(unique(HitOrder,na.rm=T))) %>%
      pivot_wider(names_from = "HitHDirection", values_from="Count")
    countsV = df_c %>% group_by(HitVDirection) %>%
      dplyr::summarize(Count = length(unique(HitOrder,na.rm=T))) %>%
      pivot_wider(names_from = "HitVDirection", values_from="Count")
    
    leftlabel = paste0("\u2190 (",countsH[["Left"]]  %>% replace(is.null(.), "0"), ")")
    rightlabel = paste0("\u2192 (",countsH[["Right"]]  %>% replace(is.null(.), "0"), ")")
    uplabel = paste0("\u2191 (",countsV[["Up"]]  %>% replace(is.null(.), "0"), ")")
    downlabel = paste0("\u2193 (",countsV[["Down"]]  %>% replace(is.null(.), "0"), ")")
    
    new_choices = c(leftlabel,rightlabel,uplabel,downlabel)
    new_choiceValues = c("Left", "Right","Up","Down")
    
    updateCheckboxGroupInput(session, label = NULL, inputId = "directionFilter", 
                             choiceNames = new_choices, 
                             choiceValues = new_choiceValues, 
                             selected = c("Left","Right","Up","Down"), inline = TRUE)
  })
  
  output$actionPlot <- renderPlotly({
    validate(need(df(), "Loading.."), errorClass = "vis")
    # Triggers plot reset when pressing button.
    if (r$reset > 0 ) {
      print(r$reset)
    }
    
    fig <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))
    
    
    # Define function that will create the resulting plot
    plot_action <- function(df, speed_df) {
      #browser()

      m = length(unique(df$viewmode)) # m = more padding,based on number of views to show.
      # Bars only
      fig_b = fig %>% add_trace(data=df, yaxis = 'y2', y=~viewmode, x =~ControllerLeaveTarget_ms, text=' ', textposition='inside',
                                insidetextanchor='middle',type = 'bar', orientation = 'h', hoverinfo='text',
                                hovertext=~paste0('Leaving Previous Target\n','Start: 0 ms', '\n','End: ', round(ControllerLeaveTarget_ms,0),'ms'),
                                marker = list(color = '#d3ebfaff', line = list(color = '#1e7bb7ff', width = 3))) %>%
        add_trace(data=df, yaxis = 'y2', y=~viewmode, x =~time_to_peak_speed_ms, 
                  text=~paste('Acceleration','\n',round(time_to_peak_speed_ms,0),'ms'), textposition='inside', hoverinfo='text',
                  hovertext=~paste0('Acceleration Phase\n','Start: ',round(ControllerLeaveTarget_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms,0),'ms'),
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#1e7bb7ff', width = 3)),
                  insidetextanchor='middle',type = 'bar', color=I('#000000'), orientation = 'h') %>%
        add_trace(data=df, yaxis = 'y2', y=~viewmode, x =~peak_speed_to_target_ms, 
                  text=~paste('Deceleration','\n',round(peak_speed_to_target_ms,0),'ms'), textposition='inside', hoverinfo='text',
                  hovertext=~paste0('Deceleration Phase\n','Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms,0),'ms'),
                  insidetextanchor='middle',type = 'bar', orientation = 'h', 
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#8ad2f3ff', width = 3))) %>% 
        add_trace(data=df, yaxis = 'y2', y=~viewmode, x =~ControllerHoverTarget_ms, hoverinfo='text',
                  hovertext=~paste0('Dwell Phase\n', 'Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms+ControllerHoverTarget_ms,0),'ms'),
                  text=~paste('Hover','\n',round(ControllerHoverTarget_ms,0),'ms'), textposition='inside', 
                  insidetextanchor='middle',type = 'bar', orientation = 'h',
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#a4de7fff', width = 3))) %>%
        layout(showlegend=F, barmode='stack', hoverlabel=list(font=list(size=15)),yaxis2 = list(overlaying = 'y', side = 'left',range=c(-0.5,0.5+m+m+m)),
               xaxis=list(title=' ',range=~c(-50,ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms+ControllerHoverTarget_ms+75),zeroline=F,showtitle=F,mirror=F,ticksuffix = " ms"),
               yaxis=list(title=' ',dtick=NULL,ticks=NULL,zeroline=F,mirror=F, showline=F,showticklabels=F,range=c(-0.65,1.0)))
      
      
      # bars + speed chart
      for (i in 1:length(df$viewmode)) {
        v = df$viewmode[i]
        vc = c('#c4c8d0ff','#8d9096ff')
        speed_df = df %>% filter(viewmode == v) %>% pull(trajectory) %>% as.data.frame()
        fig_b = fig_b %>% add_trace(data=as.data.frame(speed_df),
                            x=~time, y=~speed_norm, type='scatter',mode='lines', hoverinfo='text',
                            hovertext=~paste0('Speed: ',round(speed*100,2),'cm/s', '\n','Time: ', round(time,2),'ms'),
                            line = list(color = vc[i], width = 3)) %>%
          add_trace(data=speed_df, x=~c(first(time),last(time)),y=~c(mean(na.omit(speed_norm)),mean(na.omit(speed_norm))),type='scatter',mode='lines',
                    line = list(color = '#8d9096ff', width = 1,dash='dash', hoverinfo='text', hovertext=~paste0('Mean Speed'))) %>%
          add_trace(data=speed_df, x=~c(first(time),last(time)),y=~c(0,0),type='scatter',mode='lines',
                    line = list(color = '#8d9096ff', width = 1),hoverinfo='text', hovertext=~paste0('Y-Axis Zeroline')) %>%
          add_annotations(ax=0,ay=-20,arrowhead=7,data=df %>% filter(viewmode == v),y=~max(na.omit(peak_speed_norm)),x=~time_to_peak_speed_ms,text=~paste0('Peak Speed:\n',round(peak_speed*100,2),'cm/s')) %>%
          add_annotations(ax=0,ay=20,arrowhead=6,xanchor = "center",data=speed_df,y=~last(speed_norm),x=~last(time),text=~paste0(round(last(time),0),'ms')) %>%
          layout(hoverlabel=list(bgcolor = '#eaeaeaff'))
      }
      
      mean_speed = df %>% group_by(viewmode) %>% dplyr::summarise(m = paste(viewmode, round(mean(na.omit(as.data.frame(trajectory)$speed*100)),2),'cm/s'))
      fig_b %>% 
        add_annotations(xshift=25,yshift=15,showarrow=F,align="left",
                        data=speed_df,y=~0.8,x=~first(time),
                        text=~paste('Mean Speed:\n',paste(mean_speed$m,collapse='\n')))
      
    }
    
    
    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    # load dataset
    D <- df()
    D = D %>% rowid_to_column()
    # create action analysis
    #browser()
    # summarise performace across all actions
    
    #Debug
    #D %>% filter(Event %in% c("Mole Spawned","Mole Hit","Pointer Hover Begin","Pointer Hover End","Hit End","Hit Begin")) %>% select(HitVDirection,HitHDirection,MoleStartPositionX,MoleEndPositionX,Timestamp, Event,MoleId,MoleIdStart,MoleIdToHit, HitOrder, PatternSegmentLabel) %>% view()
    
    f = r$filter
    f = c(f,ifelse(any(f == "Left"), "Horisontal",""))
    f = c(f,ifelse(any(f == "Right"), "Horisontal",""))
    f = c(f,ifelse(any(f == "Up"), "Vertical",""))
    f = c(f,ifelse(any(f == "Down"), "Vertical",""))
    
    D = D %>% filter(HitHDirection %in% f, HitVDirection %in% f)
    
    # Filter out HitOrders smaller than 2.
    D = D %>% group_by(HitOrder) %>% dplyr::summarise(n_hitcount = n()) %>% right_join(D) %>% filter(n_hitcount > 1) %>% select(-n_hitcount)
    
    browser()
    validate(need(nrow(D %>% filter(Event == "Sample",!is.na(HitOrder))) > 0, "Trajectory samples needed to visualize.."), errorClass = "vis")
    # Create filtered dataset based on user selection
    #df_moles = df_vis %>% filter(Event %in% r$filter)
    
    ####
    # ControllerHover: Create estimations of hover/leave time for each hit (Reconstruction method)
    ####
    D = D %>% group_by(HitOrder) %>% dplyr::mutate(
      flag = ifelse(any(Event == "Pointer Hover Begin"), max(rowid[Event == "Pointer Hover Begin"],na.rm=T),NA),
      hover_time = case_when(rowid == flag ~ Timestamp),
      hit_time = last(Timestamp),
    ) %>% tidyr::fill(hover_time, .direction="downup") %>% dplyr::mutate(
      ControllerHoverTarget_ms = ifelse(!is.na(hover_time), difftime(hit_time,hover_time),NA),
      ControllerHoverTarget_ms = ControllerHoverTarget_ms * 1000
    )
    
    D = D %>% group_by(HitOrder) %>% dplyr::mutate(
      flag = ifelse(any(Event == "Pointer Hover End"), min(rowid[Event == "Pointer Hover End"],na.rm=T),NA),
      leave_time = case_when(rowid == flag ~ Timestamp),
      start_time = first(Timestamp),
    ) %>% tidyr::fill(leave_time, .direction="downup") %>% dplyr::mutate(
      ControllerLeaveTarget_ms = ifelse(!is.na(leave_time), difftime(leave_time,start_time),NA),
      ControllerLeaveTarget_ms = ControllerLeaveTarget_ms * 1000
    )
    # Debug
    #D %>% filter(Event %in% c("Mole Spawned","Mole Hit","Pointer Hover Begin","Pointer Hover End","Hit End","Hit Begin")) %>% select(ControllerLeaveTarget_ms,ControllerHoverTarget_ms,rowid,Timestamp,hover_time,hit_time,leave_time,start_time,Event,MoleId,MoleIdStart,MoleIdToHit, HitOrder, PatternSegmentLabel) %>% view()      
    #D %>% filter(!is.na(HitOrder)) %>% pull(ControllerHoverTarget_ms) %>% hist(.)
    
    
    ####
    # Speed Calculations and Trajectory
    ####
    Di = D %>% filter(Event=="Sample", !is.na(HitOrder)) %>% group_by(HitOrder) %>%
      dplyr::summarise(
        #Event = unique(Event),
        HitHDirection = unique(HitHDirection),
        HitVDirection = unique(HitVDirection),
        timestampmin = min(Timestamp),
        timestampmax = max(Timestamp),
        movement_time = timestampmax-timestampmin,
        hertz = 1 / as.numeric(movement_time),
        time_delta = 0.01, # every row is now 10ms
        timestamp_interp = seq(timestampmin, timestampmax, by=0.01),
        RightControllerPosWorldX = approx(Timestamp, RightControllerPosWorldX, xout = timestamp_interp)$y,
        RightControllerPosWorldY = approx(Timestamp, RightControllerPosWorldY, xout = timestamp_interp)$y,
        RightControllerLaserPosWorldX = approx(Timestamp, RightControllerLaserPosWorldX, xout = timestamp_interp)$y,
        RightControllerLaserPosWorldY = approx(Timestamp, RightControllerLaserPosWorldY, xout = timestamp_interp)$y,
        HeadCameraRotEulerX = approx(Timestamp, HeadCameraRotEulerX, xout = timestamp_interp)$y,
        HeadCameraRotEulerY = approx(Timestamp, HeadCameraRotEulerY, xout = timestamp_interp)$y,
        HeadCameraRotEulerZ = approx(Timestamp, HeadCameraRotEulerZ, xout = timestamp_interp)$y,
        ControllerHoverTarget_ms = unique(ControllerHoverTarget_ms),
        ControllerLeaveTarget_ms = unique(ControllerLeaveTarget_ms),
      ) %>% dplyr::rename(Timestamp = timestamp_interp) %>% group_by(HitOrder) %>% rowid_to_column() %>%
      dplyr::mutate(
        dx = c(diff(RightControllerPosWorldX),NA),
        dy = c(diff(RightControllerPosWorldY),NA),
        dt = 0.01,
        speed = sqrt(dx^2 + dy^2) / dt,
        timestampi_max = max(Timestamp),
        timestampi_min = min(Timestamp),
        timestamp_rel = as.numeric(Timestamp - timestampi_min),
        timestamp_rel_max = max(timestamp_rel),
      ) 

    Di = Di %>% ungroup() %>% dplyr::mutate(
      viewmode = r$view,
      viewmode = case_when(viewmode == "Left-vs-Right" ~ HitHDirection,
                           viewmode == "Up-vs-Down" ~ HitVDirection,
                           TRUE ~ "Combined"),
      # remove horisontal/vertical from viewmode.
      viewmode = ifelse(viewmode %in% c("Horisontal","Vertical"),NA,viewmode)
    ) %>% filter(!is.na(viewmode))
    
    validate(need(nrow(Di) > 0, "Trajectory samples needed to visualize.."), errorClass = "vis")
    
    Di = Di %>% group_by(viewmode) %>% dplyr::mutate(
      timestamp_mean = mean(timestamp_rel_max)
    )
        
    Di = Di %>% ungroup() %>% rowwise %>% dplyr::mutate(
        timestamp_rel_norm = scales::rescale(timestamp_rel,from=c(0,timestamp_rel_max), to=c(0,timestamp_mean),finite=F),
        timestamp_rel_norm_ms = timestamp_rel_norm * 1000
      )
    # Debug
    #D %>% filter(HitOrder == 3) %>% select(Framecount.Event,Timestamp, Event, RightControllerPosWorldX,RightControllerPosWorldY,ControllerHover,HitOrder,MoleId,RightControllerLaserPosWorldX,RightControllerLaserPosWorldY) %>% view()
    # Plot speed curve   
    #plot_ly() %>% add_trace(name="real",data = Di %>% filter(HitOrder %in% c(10)), type='scatter',mode='markers', 
    #                          x=~timestamp_rel, y=~speed) %>%
    #  layout(title=list(font=list(size=15), xanchor="center", xref="paper"),
    #         xaxis=list(zeroline=F, tickfont=list(size=15)),
    #         yaxis=list(zeroline=F, tickfont=list(size=15), showticklabels=T))
    

    
    S = Di %>% group_by(viewmode, HitOrder) %>% dplyr::summarise(
      #duration = sum(time_delta, na.rm=T),
      duration = max(timestamp_rel),
      duration_ms = duration * 1000,
      speed_data = list(data.frame('speed'=speed,'time'=timestamp_rel_norm_ms)),
      peak_speed = max(speed,na.rm=T),
      peak_speed_index = first(rowid[speed==max(speed,na.rm=T)]), # first() takes care of NAs
      time_to_peak_speed = sum(time_delta[rowid < peak_speed_index],na.rm=T),
      time_to_peak_speed_ms = time_to_peak_speed * 1000,
      peak_speed_to_target = sum(time_delta[rowid > peak_speed_index],na.rm=T),
      peak_speed_to_target_ms = peak_speed_to_target * 1000,
      peak_speed_to_target_pct = (peak_speed_to_target / duration) * 100,
      ControllerLeaveTarget_ms = unique(ControllerLeaveTarget_ms),
      ControllerHoverTarget_ms = unique(ControllerHoverTarget_ms),
    )
    
    ####
    # Global Average
    ####
    create_speed_trajectory <- function(speed_data_list) {
      
      max_time <- max(sapply(speed_data_list, function(df) max(df$time)))
      min_time <- min(sapply(speed_data_list, function(df) min(df$time)))
      
      common_time_grid <- seq(min_time, max_time, length.out = 300)
      interpolated_matrix <- matrix(NA, nrow = length(speed_data_list), ncol = length(common_time_grid))
      
      interpolate_speeds <- function(df, common_time_grid) {
        if (nrow(df) > 1) {
          interpolated_speeds <- approx(df$time, df$speed, xout = common_time_grid)$y
        } else {
          interpolated_speeds <- rep(NA, length(common_time_grid)) # Handle single-point data
        }
        interpolated_speeds[is.na(interpolated_speeds)] <- 0
        interpolated_speeds
      }
      
      interpolated_list <- map(speed_data_list, ~ interpolate_speeds(.x, common_time_grid))
      interpolated_matrix <- do.call(rbind, interpolated_list)
      mean_trajectory <- colMeans(interpolated_matrix, na.rm = TRUE)
      mean_trajectory_df <- data.frame(time = common_time_grid, speed = mean_trajectory)
      mean_trajectory_df = mean_trajectory_df %>% dplyr::mutate(
        speed_norm = scales::rescale(speed,from=c(0,max(speed)), to=c(0,0.9))
      )
      return(mean_trajectory_df)
    }
    
    # Attempt a global average
    # 1: we use medians for our aproximators for now because there are some pretty big outliers.
    # 2: Subtract Controller hover target from peak speed to target
    S_a = S %>% group_by(viewmode) %>% dplyr::summarise(
      ControllerLeaveTarget_ms = median(ControllerLeaveTarget_ms), #1
      ControllerHoverTarget_ms = median(ControllerHoverTarget_ms), #1
      duration_ms = mean(duration_ms),
      trajectory = list(data.frame(create_speed_trajectory(speed_data))),
      peak_speed = max(data.frame(trajectory)$speed,na.rm=T),
      peak_speed_norm = max(data.frame(trajectory)$speed_norm,na.rm=T),
      time_to_peak_speed_total_ms = na.omit(data.frame(trajectory)$time[data.frame(trajectory)$speed == peak_speed]),
      peak_speed_to_target_total_ms = max(data.frame(trajectory)$time) - time_to_peak_speed_total_ms,
      peak_speed_to_target_ms = peak_speed_to_target_total_ms - ControllerHoverTarget_ms, #2
      time_to_peak_speed_ms = time_to_peak_speed_total_ms - ControllerLeaveTarget_ms, #2
    )
    
    fig <- plot_action(S_a, S_a$trajectory)

    return(fig)
  })
  
  
  
}