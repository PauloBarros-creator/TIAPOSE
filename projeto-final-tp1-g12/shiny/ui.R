# interface simples e semelhante a html.
# cada tabPanel é uma página
# sidebarLayout define o layout com sidebar que estamos a usar 
# sidebarPanel inclui todos os paineis de input
# mainPanel inclui todos os paineis de output

my_ui <- fluidPage(
  navbarPage("TP1-G12",
    tabPanel("Previsão + Otimização",
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            tags$h3("Escolha do modelo de previsão"),
            selectInput(inputId = "pred_model",
                        label = "Selecione modelo:",
                        choices = c("ksvm" = "ksvm"),
                        selected = NULL),
            actionButton("choosePred", "Selecionar Modelo"),
            tags$h3("Escolha do modelo de otimização"),
            selectInput(inputId = "otim_model",
                        label = "Selecione modelo:",
                        choices = c("Hill Climbing" = "hill", "Monte Carlo" = "mcarlo", "Simulated Annealing" = "sann"),
                        selected = NULL),
            actionButton("chooseOtim", "Otimizar"),
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            verbatimTextOutput(outputId = "output_previsao"),
            #verbatimTextOutput(outputId = "output_plano"),
            verbatimTextOutput(outputId = "output_otim")
          )
        )
    )
    
  )
)