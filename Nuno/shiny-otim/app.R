library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "week",
                  label = "Semana:",
                  min = 1,
                  max = 52,
                  value = 30),
      
      selectInput("demo_select",
                  inputId = "year",
                  label = "Ano: ", 
                  choices = list("", 
                                 "2019" = "y", 
                                 "2020" = "n"),
                  selected = NULL,
                  width = "100%")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)