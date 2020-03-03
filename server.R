library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggpubr)

server = function(input, output, session) {
  
  # define colors to use in plots.
  colorPalette <- c("#c94232","#239a37")
  all_accounts = RetreiveUniqueColVals("whack_vr_rtii","Email")
  
  # a variable we use, if we filter based on pid.
  pid_index <- NULL
  pid_name <- NULL
  pid_email <- NULL
  pid_query <- NULL
  subject <- "trainingperformance"
  
  df_hits <- df %>% filter(Event == "Mole Hit")
  
  # a variable we use to keep track of the currently available participants
  participants <- NULL
  choices <- NULL
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[['subject']])) {
      subject <<- query[['subject']]
      updateTabsetPanel(session, "subjectChooser", selected = subject)
    }
    if (!is.null(query[['pid']])) {
      pid = query[['pid']]
      pid_query <<- pid
      pid_name <<- pid
    }
    if (!is.null(query[['email']])) {
      sel = query[['email']]
      pid_email = query[['email']]
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"))
    }
  })

  observeEvent({input$subjectChooser}, {
    if (input$subjectChooser != subject) {
      subject <<- input$subjectChooser
      UpdatePIDSelection()      
      UpdateVisualizations()      
    }
  })
  
  observeEvent(ignoreNULL=FALSE, {input$pidChooser}, {
    print(paste("email: ", input$emailSelect))
    # prevent infinite loop - only update pid_name to null, if the value is not already null.
    if (is.null(pid_name) & is.null(input$pidChooser)) {
      print(paste("pidChooser: pid_index ", pid_index))
      print(paste("pidChooser: pid_name ", pid_name))
      print("ignored..")
      return()
    }
    # CheckboxInputGroup sends an initial NULL value which overrides any query values.
    # Make sure we check whether a specific PID was specified as URL param before.
    if (!is.null(pid_query)) {
      print("pid_query exists, ignoring pidChooser")
    } else if (!is.null(input$pidChooser)) {
      pid_index <<- input$pidChooser
      pid_name <<- unlist(participants[input$pidChooser,"PID"])
      pid_email <<- unlist(participants[input$pidChooser,"Email"])
    } else {
      pid_index <<- NULL
      pid_name <<- NULL
      pid_email <<- NULL
    }
    print(paste("pidChooser: pid_index ", pid_index))
    print(paste("pidChooser: pid_name ", pid_name))
    UpdateVisualizations()  
  })
  observeEvent({input$emailSelect},{
    if (input$emailSelect == "-1") {
      return()
    }
    RefreshDataSets(input$emailSelect)

    # VARIABLES
    UpdatePIDSelection()
    
    UpdateVisualizations()
  })
  observeEvent(input$Param, {  
    UpdateVisualizations()
  })
 # update PID selection -------- 
  UpdatePIDSelection <- function() {
    # Update PID Choosers to show PID numbers based on the data
    if (subject == "trainingperformance") {
      participants <<- unique(df %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    }

    if (!is.null(pid_query)) {
      pid_name <<- pid_query
      pid_query <<- NULL
      pid_index <<- unname(choices[names(choices) == pid_name])
      print(paste("PIDQuery: e-mail", input$emailSelect))
      print(paste("PIDQuery: pid_name", pid_name))
      print(paste("PIDQuery: pid_index", pid_index))
    }
    print(choices)
    print(nrow(participants))
    if (is.null(choices)) {
      updateCheckboxGroupInput(session, label = "No Participant Data", "pidChooser", choices = NULL, selected = NULL, inline = TRUE)
    }
    else if (is.null(pid_index)) {
      print("UpdateCheckbox: pid is null")
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = NULL, inline = TRUE)
    } else {
      print(paste("UpdateCheckbox: ", pid_index))
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = pid_index, inline = TRUE)
    }
  }

  UpdateVisualizations <- function() {
    if (input$emailSelect == "-1") {
      return()
    }
    print(paste("UpdateVis pid: ", pid_name))
    print(paste("UpdateVis subject: ", subject))
    print(paste("df nrow:",nrow(df)))
    # Filter visualization data based on pid_name
    if (!is.null(pid_name)) {
      df <- df %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
    }
    if (subject == "trainingperformance") {
      print(paste("df filtered nrow:",nrow(df)))
      #output$goalHitType <- renderText(paste(length(df$ID[df$HitType == "Hit"]), " Successful Hits", sep=" "))
      #output$goalWrongHits <- renderText(paste(length(df$ID[df$HitType == "Miss"]), " Errors", sep=" "))
      #output$goalAverage <- renderText(paste("Average time : ", format(mean(df$DeltaTime)), sep=" "))
      #output$goalType <- renderText(paste("Input Type :", unique(df$InputType), sep = " "))  
      #output$goalRespond <- renderText(paste("Input Responder :", unique(df$InputResponder), sep = " "))
      
      output$speedOverTime <- renderPlotly(
        plot_ly(type = 'scatter',
                mode='markers',
                color = df$PID,
                data = df_hits, x=~df$GameTimeSpent, y=~df$MoleActivatedDuration) %>%
      
          layout(xaxis = list(title = "Game Time (s)"),
                 yaxis = list(title = "ReactionTime (s)"),
                 legend = list(orientation = 'h'))
      )
      
      #output$goalComparison <- renderPlotly(
      #  plot_ly(type = 'scatter',
      #          mode='markers',
      #          color = df$InputType ,
      #          # colors = pal,
      #  ) %>%
      #    layout(xaxis = list(title = ""),
      #           yaxis = list(title = "Movement Time (s)"),
      #           legend = list(orientation = 'h'))
      #)
      #output$goalLRPlot <- renderPlot({goalLRcompGG<- ggplot(df, aes(FittsID,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ stat_regline_equation()+ ylab("movement time in seconds")+xlab("ID")+ theme_bw()+geom_point()+facet_grid(~InputResponders)
      #print(goalLRcompGG)})
    }
  }
  
}
  
