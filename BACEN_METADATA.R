bc.metadata <- function(sgs_code){
  if(!require(httr)) { install.packages("httr"); library(httr) }
  if(!require(rvest)) { install.packages("rvest"); library(rvest) }
  
  base_url <- "https://www3.bcb.gov.br"
  handle_bcb <- handle(base_url)
  
  # URL principal para consulta de metadados
  main_url <- paste0(base_url, "/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=", sgs_code)
  
  # Primeiro, carregar página principal para obter cookies
  GET(main_url, handle = handle_bcb)
  
  # Realizar POST para garantir seleção correta da série
  POST(main_url,
       body = list(method = "consultarMetadadosSeriesInternet",
                   hdOidSerieSelecionada = sgs_code),
       encode = "form",
       handle = handle_bcb)
  
  # URL do iframe com os dados básicos
  iframe_url <- paste0(base_url, "/sgspub/JSP/consultarmetadados/cmiDadosBasicos.jsp")
  
  # Obter conteúdo do iframe com o handle da sessão já configurada
  iframe_resp <- GET(iframe_url, handle = handle_bcb,
                     add_headers(Referer = main_url))
  
  # Checar a resposta
  if(status_code(iframe_resp) != 200) {
    stop("Falha ao acessar o iframe.")
  }
  
  # Converter conteúdo para texto com encoding correto
  iframe_content <- content(iframe_resp, "text", encoding = "ISO-8859-1")
  
  iframe_page <- read_html(iframe_content)
  
  # Extrair todos os textos dentro dos spans da classe textoPequeno
  target_text <- iframe_page %>%
    html_nodes("span.textoPequeno") %>%
    html_text(trim = TRUE)
  
  # Conferir se há textos suficientes
  if(length(target_text) < 4){
    warning("A série não contém texto suficiente.")
    return(NA)
  }
  
  # Retornar o 4º elemento desejado
  return(target_text[4])
}

bc.metadata(432)
print(metadata_432)
