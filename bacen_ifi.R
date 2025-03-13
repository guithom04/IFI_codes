#-----------------#
#-----------------#
#automotivo-------#
#nuci-------------# 
#inflacao---------#
#automotivo-------#
#abras------------#
#pib_mensal-------#
#inflacao---------#
#commodities------# 
#renda_disponivel-#
#ibc_br-----------#
#cambio_mensal----#
#cambio_diario----#
#selic------------#
#caged------------#
#nuci-------------#
#-----------------#
baixar_grupos <- function(grupo,
                          diretorio = "U:/Macro/Projeções/dados R/BACEN_NOVO") {
  # Define diretório de trabalho
  setwd(diretorio)
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
  # Carrega os pacotes necessários
  pacotes <- c("dplyr", "httr", "jsonlite", "lubridate", "writexl", "purrr", "rvest", "rbcb", "stringr", "openxlsx")
  sapply(pacotes, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  })
  
  # Lista de séries por grupo
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
  
  # Variável global para registrar códigos que deram erro
  series_com_erro <<- c()
  
  # Função para baixar cada série via API; se falhar, tenta com o rbcb
  baixar_sgs <- function(codigo_sgs, tentativas = 10, espera_segundos = 2) {
    url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json", codigo_sgs)
    tentativa_atual <- 1
    
    while (tentativa_atual <= tentativas) {
      response <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
      
      if (status_code(response) == 200 && grepl("json", headers(response)$`content-type`)) {
        df <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
        df$data <- as.character(lubridate::dmy(df$data))  # Converte data para character
        df$valor <- as.numeric(gsub(",", ".", df$valor))
        return(df)
      } else {
        message(sprintf("Tentativa %d falhou para SGS %s. Nova tentativa em %d segundos...", tentativa_atual, codigo_sgs, espera_segundos))
        Sys.sleep(espera_segundos)
        tentativa_atual <- tentativa_atual + 1
      }
    }
    
    # Fallback usando rbcb
    message(sprintf("Usando fallback para SGS %s via rbcb", codigo_sgs))
    resultado_rbcb <- tryCatch({
      df_rbcb <- rbcb::get_series(codigo_sgs)
      df_rbcb$data <- as.character(as.Date(df_rbcb$date))
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
  
  # Inicializa listas para armazenar resultados e metadados
  lista_df <- list()
  nome_colunas <- list()  # Armazena nomes reais das séries
  sgs_codes <- c()        # Armazena os códigos SGS
  
  # Primeira tentativa de download para cada série
  for (codigo in series_sgs[[grupo]]) {
    resultado <- baixar_sgs(codigo)
    if (!is.null(resultado)) {
      nome_serie <- bc.metadata(codigo)
      nome_colunas[[as.character(codigo)]] <- nome_serie
      sgs_codes <- c(sgs_codes, codigo)
      lista_df[[as.character(codigo)]] <- resultado
    } else {
      series_com_erro <<- c(series_com_erro, codigo)
    }
    Sys.sleep(5)  # Pausa entre as requisições
  }
  
  # Tenta rebaixar as séries que falharam na primeira tentativa
  if (length(series_com_erro) > 0) {
    failed_codes <- unique(series_com_erro)
    message("Séries que falharam no primeiro download:")
    print(failed_codes)
    for (codigo in failed_codes) {
      message(sprintf("Tentando rebaixar a série %s", codigo))
      resultado <- baixar_sgs(codigo)
      if (!is.null(resultado)) {
        nome_serie <- bc.metadata(codigo)
        # Adiciona ou substitui o download na lista
        lista_df[[as.character(codigo)]] <- resultado
        nome_colunas[[as.character(codigo)]] <- nome_serie
        # Garante que o código esteja na lista de códigos baixados
        if (!(codigo %in% sgs_codes)) {
          sgs_codes <- c(sgs_codes, codigo)
        }
        message(sprintf("Série %s rebaixada com sucesso.", codigo))
      } else {
        message(sprintf("Série %s ainda falhou.", codigo))
      }
      Sys.sleep(5)
    }
    # Atualiza a lista de erros: os códigos que não tiveram sucesso mesmo após a segunda tentativa
    todos_codigos <- series_sgs[[grupo]]
    baixados <- as.numeric(names(lista_df))
    series_com_erro <<- setdiff(todos_codigos, baixados)
  }
  
  # Consolida os dados se houve algum download com sucesso
  if (length(lista_df) > 0) {
    resultados_finais <- purrr::reduce(lista_df, full_join, by = "data") %>% arrange(data)
    
    # Renomeia as colunas (exceto a coluna "data") com os nomes reais das séries
    colnames(resultados_finais)[-1] <- unlist(nome_colunas)
    
    # Cria cabeçalho com os códigos SGS
    header <- c("SGS Code", sgs_codes)
    
    # Cria e preenche a planilha Excel
    output_path <- sprintf("dados_sgs_%s.xlsx", grupo)
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheetName = 'Sheet1')
    openxlsx::writeData(wb = wb, sheet = 'Sheet1', startCol = 1, x = t(header), colNames = FALSE)
    openxlsx::writeData(wb = wb, sheet = 'Sheet1', x = resultados_finais, startRow = 2)
    
    estilo_header <- openxlsx::createStyle(
      borderStyle = 'medium',
      fgFill = 'orange',
      border = 'bottom',
      textDecoration = 'bold'
    )
    openxlsx::addStyle(wb, 'Sheet1', estilo_header, rows = 1, cols = 1:length(header))
    openxlsx::addStyle(wb, 'Sheet1', estilo_header, rows = 2, cols = 1:length(header))
    openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
    
    message(sprintf("Arquivo salvo com sucesso como %s", output_path))
  } else {
    message("Nenhuma série foi baixada com sucesso.")
  }
  
  # Exibe as séries que ainda não foram baixadas
  if (length(series_com_erro) > 0) {
    message("Séries que ainda falharam após nova tentativa:")
    print(unique(series_com_erro))
  } else {
    message("Todas as séries foram baixadas com sucesso.")
  }
}
baixar_grupos("caged")
