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



baixar_grupos <- function(grupo, diretorio = "U:/Macro/Projeções/dados R/BACEN_NOVO") {
  # Set working directory
  setwd(diretorio)
  
  # Load necessary packages
  pacotes <- c("dplyr", "httr", "jsonlite", "lubridate", "writexl", "purrr", "rvest", "rbcb", "stringr")
  sapply(pacotes, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  })
  
  # List of series by group
  series_sgs <- list(
    automotivo = c(1373, 1378, 7384, 28527, 28528),
    abras = c(28549),
    pib_mensal = c(4380),
    inflacao = c(190, 189, 1619, 188, 433, 11428, 10841, 10842, 10843, 10844,
                 4449, 16122, 16121, 11427, 11426, 4466, 27838, 27839, 28750, 21379,
                 4447, 4448, 27863, 27864, 1635, 1636, 1637, 1638, 1639, 1640,
                 1641, 1642, 1643, 7450, 7453, 7456),
    commodities = c(27574, 27575, 27576, 27577, 29042),
    renda_disponivel = c(29023, 29024, 29025, 29026, 29028),
    ibc_br = c(24363, 24364),
    cambio_mensal = c(3696, 3698, 11752),
    cambio_diario = c(1),
    selic = c(432),
    caged = c(28763, 28784, 28764, 28766, 28770, 28771,
              28772, 28785, 28787, 28791, 28792, 28793),
    nuci = c(24352, 28561)
  )
  
  if (!grupo %in% names(series_sgs)) stop("Grupo informado inválido.")
  
  series_com_erro <<- c()
  
  # Function to fetch data from BCB API or fallback to rbcb
  baixar_sgs <- function(codigo_sgs, tentativas = 10, espera_segundos = 30) {
    url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json", codigo_sgs)
    tentativa_atual <- 1
    
    while (tentativa_atual <= tentativas) {
      
      response <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
      
      if (status_code(response) == 200 && grepl("json", headers(response)$`content-type`)) {
        
        df <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
        df$data <- as.character(dmy(df$data))  # Convert Date to character
        df$valor <- as.numeric(gsub(",", ".", df$valor))
        return(df)
        
      } else {
        message(sprintf("Tentativa %d falhou para SGS %s. Nova tentativa em 30 segundos...", tentativa_atual, codigo_sgs))
        Sys.sleep(espera_segundos)
        tentativa_atual <- tentativa_atual + 1
      }
    }
    
    # fallback rbcb
    message(sprintf("Usando fallback para SGS %s via rbcb", codigo_sgs))
    resultado_rbcb <- tryCatch({
      df_rbcb <- rbcb::get_series(codigo_sgs)
      df_rbcb$data <- as.character(as.Date(df_rbcb$date))  # Convert Date to character
      df_rbcb <- df_rbcb %>% 
        rename(valor = value) %>% 
        select(-date)
      df_rbcb
    }, error = function(e) {
      message(sprintf("Falha no rbcb também para SGS %s: %s", codigo_sgs, e$message))
      series_com_erro <<- c(series_com_erro, codigo_sgs)
      NULL
    })
    
    return(resultado_rbcb)
  }
  
  # Download series for selected group
  lista_df <- list()
  nome_colunas <- list()  # Store series names
  sgs_codes <- c()  # Store SGS codes
  
  for (codigo in series_sgs[[grupo]]) {
    resultado <- baixar_sgs(codigo)
    
    if (!is.null(resultado)) {
      nome_serie <- bc.metadata(codigo)
      nome_colunas[[as.character(codigo)]] <- nome_serie  # Store real name
      sgs_codes <- c(sgs_codes, codigo)  # Store SGS code
      lista_df[[as.character(codigo)]] <- resultado
    } else {
      series_com_erro <<- c(series_com_erro, codigo)
    }
    
    Sys.sleep(5)  # Wait between requests
  }
  
  # Create final consolidated dataframe
  if (length(lista_df) > 0) {
    resultados_finais <- reduce(lista_df, full_join, by = "data") %>% arrange(data)
    
    # Rename columns using retrieved names
    colnames(resultados_finais)[-1] <- unlist(nome_colunas)
    
    # Add SGS row before the column names
    header = c("SGS Code", sgs_codes)
    # df_final <- rbind(
    #   c("SGS Code", sgs_codes),  # Row of SGS codes
    #   # c("Nome da Série", unlist(nome_colunas)),  # Row of names
    #   resultados_finais  # Actual data
    # )
    df_final = resultados_finais
    
    output_path <- sprintf("dados_sgs_%s.xlsx", grupo)  # Dynamic filename
    
    wb = openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb,sheetName = 'Sheet1')
    openxlsx::writeData(wb = wb,sheet =  'Sheet1', startCol = 1, x = t(header),colNames = F)
    openxlsx::writeData(wb = wb,sheet =  'Sheet1', x = df_final,startRow = 2)
    estilo_header = openxlsx::createStyle(
      borderStyle = 'medium',fgFill = 'orange',border = 'bottom',textDecoration = 'bold'
    )
    openxlsx::addStyle(wb, 'Sheet1', estilo_header,rows = 1, cols = 1:length(header))
    openxlsx::addStyle(wb, 'Sheet1', estilo_header,rows = 2, cols = 1:length(header))
    # write_xlsx(as.data.frame(df_final), path = output_path)  # Ensure correct format for Excel
    
    openxlsx::saveWorkbook(wb, file = output_path,overwrite = T)
    message(sprintf("File successfully saved as %s", output_path))
  } else {
    message("No series were successfully downloaded.")
  }
  
  # Show series that failed to download
  if (length(series_com_erro) > 0) {
    message("Series that failed to download:")
    print(unique(series_com_erro))
  } else {
    message("All series downloaded successfully.")
  }
}
