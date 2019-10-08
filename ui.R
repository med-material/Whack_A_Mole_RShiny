library(shiny)

# Define UI for application that plots features of movies 
shinyUI(fluidPage(
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      selectInput("Participants", label = h3("Participant"), 
                  choices = GenerateSelectChoices("All participants", "Participant", "ParticipantNo"), 
                  selected = -1),
      selectInput("Difficulty", label = h3("Difficulty"), 
                  choices = GenerateSelectChoices("All difficulties", "Difficulty", "Difficulty"), 
                  selected = -1),
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot", hover = "plot_hover", height="650px"),
      # Show data table
      dataTableOutput(outputId = "hovertable"),
      br()
    )
  )
))