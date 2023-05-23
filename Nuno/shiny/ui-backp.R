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
                          selectInput(inputId = "model",
                                      label = "Selecione modelo:",
                                      choices = c("ksvm"),
                                      selected = NULL),
                          actionButton("choosePred", "Selecionar Modelo"),
                          tags$h3("Escolha do plano"),
                          p("Introduza os valores para cada dia separados por vírgulas (dia1,dia2,...)."),
                          textInput('p1', '\nBebidas STELLA empacotadas e distribuidas', "160,8,0,52,20,0,0"),
                          textInput('p2', 'Bebidas BUD empacotadas e distribuidas', "200,200,0,0,30,0,0"),
                          textInput('arm', 'Recursos de armazém', "6,3,0,1,1,0,1"),
                          textInput('v1', 'Recurso de distribuição V1', "2,0,0,1,0,0,0"),
                          textInput('v2', 'Recurso de distribuição V2', "2,1,0,0,1,0,0"),
                          textInput('v3', 'Recurso de distribuição V3', "2,1,0,0,0,0,0"),
                          actionButton("choosePlan", "Calcular solução inicial"),
                          tags$h3("Escolha do modelo de otimização"),
                          selectInput(inputId = "otim-model",
                                      label = "Selecione modelo:",
                                      choices = c("Monte Carlo"),
                                      selected = NULL),
                          actionButton("chooseOtim", "Otimizar"),
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          verbatimTextOutput(outputId = "output_previsao"),
                          verbatimTextOutput(outputId = "output_plano"),
                          verbatimTextOutput(outputId = "output_otim")
                        )
                      )
             )
             
  )
)