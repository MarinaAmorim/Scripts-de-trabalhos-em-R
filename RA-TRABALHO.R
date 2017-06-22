#####################################################
# Trabalho Prático
#####################################################

##PACOTES NECESSÁRIOS

# Para instalar o pacote INLA é necessário usar o comando abaixo
source("http://www.math.ntnu.no/inla/givemeINLA.R")
require(INLA)
require(shiny)
{
  ui <- fluidPage(
    headerPanel("TRABALHO PRÁTICO - R AVANÇADO"),
    sidebarPanel(
      fileInput("dados","Escolha um arquivo .txt",multiple = FALSE,accept = ".txt",buttonLabel = "Ler"),
      tags$hr(),
      checkboxInput("header", "Arquivo com título", TRUE),
      selectInput('resp', "Variável Resposta:", choices = ""),
      checkboxGroupInput("exp", "Variável Explicativa" )
    ),
    mainPanel(
      tableOutput("results")
    )
  )
  
  server <- function(input, output, session) {
    output$results <- renderTable({
      dados <- input$dados
      if (is.null(dados))
        return(NULL)
      dados <- read.table(dados$datapath, header = input$header)
      variaveis <- names(dados)
      updateSelectInput(session = session, inputId = "resp", choices = variaveis)
      y <- input$resp
      dados2 <- dados[,-input$resp]
      variaveis2 <- names(dados2)
      choiceValues = names(dados2)
      updateSelectInput(session = session, inputId = "exp", choices = variaveis2)
    })
  }
  shinyApp(ui = ui, server = server)
}
