####  Teste Gerando Imagens IA  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/Projeto_-_Gerador_de_Imagens_com_OpenIA_-_ChatGPT")
getwd()



# Carregando pacotes
library(shiny)
library(httr)
library(shinyjs)

# Configurar sua chave da API OpenAI GPT-3
OPENAI_API_KEY <- "sk-"

# Define a função para gerar a imagem com base no prompt
generate_image <- function(prompt) {
  # URL da API para criar imagens
  url <- "https://api.openai.com/v1/images/generations"
  
  # Dados do corpo da solicitação
  data <- list(
    "model" = "dall-e-3",
    "prompt" = prompt,
    "n" = 1,
    "size" = "1024x1024"
  )
  
  # Fazer a solicitação POST para a API OpenAI
  response <- POST(
    url,
    add_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", OPENAI_API_KEY)),
    body = data,
    encode = "json"
  )
  
  # Verificar se a resposta foi bem-sucedida
  if (http_status(response)$category == "Success") {
    # Extrair os dados da resposta
    response_data <- content(response, "parsed")
    
    # Retorna a URL da imagem gerada
    return(response_data$data[[1]]$url)
  } else {
    cat("Erro ao chamar a API OpenAI GPT-3.\n")
    return(NULL)
  }
}


# Define a interface da aplicação Shiny
ui <- fluidPage(
  useShinyjs(),  # Ativa a extensão shinyjs
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f8f9fa;
        padding: 10px;
      }
      body {
        background-color: #f8f9fa;
        padding-top: 10px;
      }
    "))
  ),
  tags$h1("Gerador de Imagens OpenAI GPT-3", style = "text-align: center; margin-bottom: 35px;"),  # Adiciona um título centralizado
  sidebarLayout(
    sidebarPanel(
      h4("Configurações:", style = "margin-bottom: 20px;"),  # Ajusta a margem abaixo do título
      selectInput("promptMenu", "Exemplos de prompt:", 
                  choices = c("Gerar imagem de um gato fofo",
                              "Gerar imagem de uma paisagem surreal",
                              "Gerar imagem de um retrato abstrato"),
                  selected = "Gerar imagem de um gato fofo"),
      br(),
      textInput("customPrompt", "Ou digite seu próprio prompt:"),
      br(),
      actionButton("generateBtn", "Gerar Imagem", class = "btn-primary"),
      br(),
      div(id = "loadingMessage", style = "text-align: center; color: blue;"),
      div(id = "imageLoadedMessage", style = "text-align: center; color: green; display: none;", "Imagem Carregada!")
    ),
    mainPanel(
      h4("Imagem Gerada", style = "text-align: center;"),
      uiOutput("imageOutput")
    )
  )
)


# Define a lógica do servidor Shiny
server <- function(input, output, session) {
  # Função reativa para gerar a imagem quando o botão for clicado
  generated_image <- eventReactive(input$generateBtn, {
    shinyjs::disable("generateBtn")  # Desabilita o botão
    shinyjs::html("loadingMessage", "Gerando imagem...")  # Exibe a mensagem de carregamento
    
    if (!is.null(input$customPrompt) && input$customPrompt != "") {
      prompt <- input$customPrompt
    } else {
      prompt <- input$promptMenu
    }
    
    img <- generate_image(prompt)
    
    shinyjs::enable("generateBtn")  # Habilita o botão após o término do processo
    shinyjs::html("loadingMessage", "")  # Oculta a mensagem de carregamento
    shinyjs::show("imageLoadedMessage")  # Exibe a mensagem de imagem carregada
    
    img  # Retorna a imagem
  })
  
  # Renderiza a imagem gerada na interface
  output$imageOutput <- renderUI({
    img_url <- generated_image()
    
    if (!is.null(img_url)) {
      img_tag <- tags$img(src = img_url, width = 800, height = 600)
      return(img_tag)
    } else {
      return(NULL)
    }
  })
}

shinyApp(ui, server)


"Menina de 10 anos de cabelo castanho claro e olhos azuis jogando Fortnite com uma sking de tartaruga e sendo observada por 1 gato e 1 tartaruga"
