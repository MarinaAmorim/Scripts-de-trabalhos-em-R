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
    
    uiOutput(outputId = "varResp"),
    uiOutput(outputId = "varExp"),
    uiOutput(outputId = "ana")  
    ),
  mainPanel(
     tableOutput("result")
  )
)

server <- function(input, output, session) {
  
  loadData <- reactive({
    dados <- input$dados
    if (is.null(dados))
      return(NULL)
    dados <- read.table(dados$datapath, header = input$header)
  })
  output$varResp <- renderUI({
    dados <- loadData()
    variaveis <- names(dados)
    selectInput('resp', "Variável Resposta:", choices = variaveis)
  })  
  output$varExp <- renderUI({
    dados <- loadData()
    if (is.null(dados))
      return(NULL)
    y <- input$resp
    y_index <- which(names(dados) == y)
    variaveis2 <- names(dados)[-y_index]	
    checkboxGroupInput('exp', "Variáveis Explicativas:", choices = variaveis2)
    
  })
  output$ana<- renderUI({actionButton("analysis","Ajustar") })
  
  
  observeEvent(input$analysis, {
  output$result <- renderPrint({
    dados <- loadData()
    model=lm(input$resp ~ input$exp, data =dados )
      summary(model)
    })
  })
}


shinyApp(ui = ui, server = server)
}


