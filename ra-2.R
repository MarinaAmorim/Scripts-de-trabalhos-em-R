#####################################################
# Trabalho Prático
#####################################################

##PACOTES NECESSÁRIOS

# Para instalar o pacote INLA é necessário usar o comando abaixo
source("http://www.math.ntnu.no/inla/givemeINLA.R")
require(INLA)
require(shiny)

ui <- fluidPage(
  headerPanel("TRABALHO PRÁTICO - R AVANÇADO"),
  sidebarPanel(
    fileInput("dados","Escolha um arquivo .txt",multiple = FALSE,accept = ".txt",buttonLabel = "Ler"),
    tags$hr(),
    checkboxInput("header", "Arquivo com título", TRUE),
    
    uiOutput(outputId = "varResp"),
    uiOutput(outputId = "varExp")
    # selectInput('resp', "Variável Resposta:", choices = ""),
    # checkboxGroupInput('exp', "Variáveis Explicativas:", choices = "")
  ),
  mainPanel(
    # tableOutput("res0ults")
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
  
  
}
shinyApp(ui = ui, server = server)


#############

#####################################################
# Trabalho Prático
#####################################################

##PACOTES NECESSÁRIOS

# Para instalar o pacote INLA é necessário usar o comando abaixo
source("http://www.math.ntnu.no/inla/givemeINLA.R")
require(INLA)
require(shiny)
#install.packages("shiny", dependencies = T)
ui <- fluidPage(
  headerPanel("TRABALHO PRÁTICO - R AVANÇADO"),
  sidebarPanel(
    fileInput("dados","Escolha um arquivo .txt",multiple = FALSE,accept = ".txt",buttonLabel = "Ler"),
    tags$hr(),
    checkboxInput("header", "Arquivo com título", TRUE),
    selectInput('resp', "Variável Resposta:", 
                choices = ""),
    checkboxGroupInput('exp', "Variáveis Explicativas:", choices = "")
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
    
    variaveis <- colnames(dados)
    updateSelectInput(session = session, inputId = "resp", choices = variaveis)
    dados2 <- dados[,(!(variaveis==input$resp))]
    variaveis2 <- colnames(dados2)
    updateCheckboxGroupInput(session = session, inputId = "exp", choices= variaveis2)
  })
}
shinyApp(ui = ui, server = server)
