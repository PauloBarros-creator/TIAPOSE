my_ui <- fluidPage(
  navbarPage("TP1-G12",
    tabPanel("Previsão",
            
    ),
    tabPanel("Otimização",
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "year",
                        label = "Ano:",
                        choices = c("2019", "2020"),
                        selected = NULL),
            sliderInput(inputId = "week",
                        label = "Semana:",
                        min = 1,
                        max = 52,
                        value = 30),
            actionButton("chooseYear", "Selecionar Ano"),
            textInput('p1', 'Enter p1 (separado por vírgulas)', "160,8,0,52,20,0,0"),
            textInput('p2', 'Enter p2 (separado por vírgulas)', "200,200,0,0,30,0,0"),
            textInput('p3', 'Enter ARM (separado por vírgulas)', "6,3,0,1,1,0,1"),
            textInput('v1', 'Enter V1 (separado por vírgulas)', "2,0,0,1,0,0,0"),
            textInput('v2', 'Enter V2 (separado por vírgulas)', "2,1,0,0,1,0,0"),
            textInput('v3', 'Enter V3 (separado por vírgulas)', "2,1,0,0,0,0,0"),
            actionButton("execute", "Selecionar Plano")
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            verbatimTextOutput(outputId = "output"),
            verbatimTextOutput(outputId = "output_f")
          )
        )
    ),
    fluidRow(
      column(12,
             navbarMenu("Menu",
                        id = "resetBtn",
                        icon = icon("refresh")
             )
      )
    )
    
  )
)