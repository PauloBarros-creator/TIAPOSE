# interface simples e semelhante a html.
# cada tabPanel é uma página
# sidebarLayout define o layout com sidebar que estamos a usar 
# sidebarPanel inclui todos os paineis de input
# mainPanel inclui todos os paineis de output

library(ggplot2)
theme_set(theme_minimal())

my_ui <- fluidPage(
  navbarPage("TP1-G12",
    tabPanel("Previsão + Otimização",
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            tags$h3("Escolha do modelo de previsão"),
            selectInput(inputId = "pred_model",
                        label = "Selecione modelo:",
                        choices = c("#1: KSVM (RW)" = "ksvm", "#2: CUBIST (RW/GW)" = "cubist","#3: XGBOOST (RW/GW)"="xgboost","#4: LM (GW)" = "lm","#5: MR (RW)" = "mr","#6: PLSR (RW/GW)"="plsr"),
                        selected = NULL),
            actionButton("choosePred", "Selecionar Modelo"),
            actionButton("choosePlot", "Plot do Modelo"),
            tags$h3("Escolha do modelo de otimização"),
            selectInput(inputId = "otim_model",
                        label = "Selecione modelo:",
                        choices = c("Hill Climbing" = "hill", "Monte Carlo" = "montecarlo", "Simulated Annealing" = "sann"),
                        selected = NULL),
            actionButton("chooseOtim", "Otimizar"),
            actionButton("choosePlan", "Elaborar Plano Detalhado")
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            shinycssloaders::withSpinner(
              tableOutput(outputId = "output_previsao"),
            ),
            shinycssloaders::withSpinner(
              plotOutput(outputId = "output_prev_plot")
            ),
            shinycssloaders::withSpinner(
              verbatimTextOutput(outputId = "output_otim")
            ),
            shinycssloaders::withSpinner(
              tableOutput(outputId = "output_arm")
            )
          )
        )
    )
    
  )
)