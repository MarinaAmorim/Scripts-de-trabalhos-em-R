#####################################################
# TRABALHO PRÁTICO
#####################################################

##PACOTES NECESSÃRIOS
# Para instalar o pacote INLA Ã© necessÃ¡rio usar o comando abaixo
source("http://www.math.ntnu.no/inla/givemeINLA.R")
require(INLA)
require(shiny)


ui <- fluidPage(
  headerPanel("TRABALHO PRÁTICO - R AVANÇADO"),
  sidebarPanel(
    selectInput("separador","Como as colunas do seu arquivo são separadas?", choices = c("tab","espaço",",",";"), selected = "tab"),
    fileInput("dados","Escolha um arquivo .txt",multiple = FALSE,accept = ".txt",buttonLabel = "Ler"),
    tags$hr(),
    checkboxInput("header", "Arquivo com título", TRUE),    
    uiOutput(outputId = "varResp"),
    uiOutput(outputId = "varExp"),
    actionButton("modelar", "Gerar saídas")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", 
               verbatimTextOutput("summary")
      ),
      
      tabPanel("Valores Ajustados",
               verbatimTextOutput("fitted")
      ),
      tabPanel("Gráfico de PIT",
               plotOutput("pit")
      )
    )
  )
)

server <- function(input, output, session) {
  
  loadData <- reactive({
    if(input$separador == "tab")
      sep = "\t"
    else if (input$separador == "espaço")
      sep = ""
    else 
      sep = input$separador
    dados <- input$dados
    if (is.null(dados))
      return(NULL)
    dados <- read.table(dados$datapath, header = input$header, sep = sep)
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
  observeEvent( input$modelar, {
    dados <- loadData()
    if (is.null(dados))
      return(NULL)
    
    y <- input$resp
    x <- input$exp
    model=lm(as.formula(paste(input$resp," ~ ",paste(input$exp,collapse="+"))),data=dados)
    
    output$summary <- renderPrint({
      summary(model)
    })
    
    output$fitted <- renderPrint({
      as.matrix(model$fitted.values)
    })
    
    output$pit <- renderPlot({
      inla.ks.plot(as.vector(model$residuals), pnorm) 
    })
  })
}
shinyApp(ui = ui, server = server)


