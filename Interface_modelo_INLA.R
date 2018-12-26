#####################################################
# Interface no Shiny para o modelo INLA
# MArina Amorim
# marinaaamorim@hotmail.com
#####################################################

##PACOTES NECESS¡RIOS
# Para instalar o pacote INLA √© necess√°rio usar o comando abaixo
source("http://www.math.ntnu.no/inla/givemeINLA.R")
#install.packages("INLA")
require(INLA) 
#install.packages("shiny")
require(shiny)


ui <- fluidPage(
  headerPanel("TRABALHO PR¡TICO - R AVAN«ADO"),
  sidebarPanel(
    selectInput("separador","Como as colunas do seu arquivo s„o separadas?", choices = c("tab","espaÁo",",",";"), selected = "tab"),
    fileInput("dados","Escolha um arquivo .txt",multiple = FALSE,accept = ".txt",buttonLabel = "Ler"),
    tags$hr(),
    checkboxInput("header", "Arquivo com tÌtulo", TRUE),    
    uiOutput(outputId = "varResp"),
    uiOutput(outputId = "varExp"),
    actionButton("modelar", "Gerar saÌdas")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", 
               verbatimTextOutput("summary")
      ),
      
      tabPanel("Valores Ajustados",
               verbatimTextOutput("fitted")
      ),
      tabPanel("Gr·fico de PIT",
               plotOutput("pit")
      )
    )
  )
)

server <- function(input, output, session) {
  
  loadData <- reactive({
    if(input$separador == "tab")
      sep = "\t"
    else if (input$separador == "espaÁo")
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
    selectInput('resp', "Vari·vel Resposta:", choices = variaveis)
  })  
  
  output$varExp <- renderUI({
    dados <- loadData()
    if (is.null(dados))
      return(NULL)
    y <- input$resp
    y_index <- which(names(dados) == y)
    variaveis2 <- names(dados)[-y_index] 
    checkboxGroupInput('exp', "Vari·veis Explicativas:", choices = variaveis2)
  })
  observeEvent( input$modelar, {
    dados <- loadData()
    if (is.null(dados))
      return(NULL)
    
    y <- input$resp
    x <- input$exp
    formula <- as.formula(paste(input$resp," ~ ",paste(input$exp,collapse="+")))
    model <- inla(formula = formula, data = data.frame(dados),
                  control.compute = list(cpo = TRUE))
    
    output$summary <- renderPrint({
      summary(model)
    })
    
    output$fitted <- renderPrint({
      model$marginals.fitted.values
    })
    
    output$pit <- renderPlot({
      inla.ks.plot(model$cpo$pit, punif) 
    })
  })
}
shinyApp(ui = ui, server = server)


