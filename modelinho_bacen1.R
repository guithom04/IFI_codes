rm(list = ls())
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
###############################################################################
############################# pasta de trabalho ###############################
###############################################################################
packages_env <- function(){
  setwd("U:/Pastas pessoais/LucasM/planilhas rafa")
  packages <- c("httr",
                "jsonlite",
                "lubridate",
                "dplyr",
                "purrr",
                "zoo",
                "ggplot2",
                "tidyverse",
                "glue",
                "readxl",
                "readODS",
                "pracma",
                "writexl",
                "openxlsx")
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) install.packages(packages[!installed])
  lapply(packages, library, character.only = TRUE)
}
packages_env()
###############################################################################
############################ função bacen direct ##############################
###############################################################################
R_BC_Direct <- function(codigo_sgs, return_json = FALSE) {
  # Lista de pacotes necessários
  packages <- c("httr", "jsonlite", "lubridate", "dplyr", "purrr")
  
  # Instala os pacotes que não estão instalados
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) install.packages(packages[!installed])
  
  # Carrega todos os pacotes
  lapply(packages, library, character.only = TRUE)
  
  # Construct the URL for the requested SGS series
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json", codigo_sgs)
  
  # Make the GET request to the API
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Error accessing the API.")
  }
  
  # Convert the JSON response to a data frame
  df <- content(response, as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  # Convert the 'data' column to Date format (assuming day-month-year format)
  df$data <- dmy(df$data)
  
  # Rename the 'valor' column to include the SGS code for clarity
  df <- df %>% rename(!!paste0("SGS_", codigo_sgs) := valor)
  
  # If return_json is TRUE, convert the data frame to JSON and return it;
  # otherwise, return the data frame.
  if (return_json) {
    json_data <- toJSON(df, dataframe = "columns", POSIXt = "ISO8601", pretty = TRUE)
    return(json_data)
  } else {
    return(df)
  }
}
###############################################################################
############################## função bls direct ##############################
###############################################################################
R_BLS_Direct_Full <- function(id, startyear = 2000) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  
  api_key <- "f7c6508d10e9495f99056dd26ea2c20a"
  url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
  final_year <- year(Sys.Date())
  
  year_ranges <- list()
  current_start <- startyear
  
  while (current_start <= final_year) {
    current_end <- min(current_start + 19, final_year)
    year_ranges[[length(year_ranges) + 1]] <- c(current_start, current_end)
    current_start <- current_end + 1
  }
  
  get_chunk <- function(start, end) {
    payload <- list(
      seriesid = list(id),
      startyear = as.character(start),
      endyear = as.character(end),
      registrationkey = api_key
    )
    
    response <- POST(url, body = payload, encode = "json", content_type_json())
    
    if (status_code(response) != 200) {
      warning(paste0("Erro HTTP em ", start, "-", end, ": ", status_code(response)))
      return(tibble())
    }
    
    result <- content(response, as = "parsed")
    
    if (!is.list(result) || is.null(result$Results) || length(result$Results$series) == 0) {
      warning(paste0("Resposta inválida para ", start, "-", end))
      return(tibble())
    }
    
    raw_data <- result$Results$series[[1]]$data
    
    # Verifica e filtra apenas observações válidas
    if (length(raw_data) == 0) return(tibble())
    
    # Constrói tibble linha por linha com segurança
    safe_df <- map_dfr(raw_data, function(row) {
      if (all(c("year", "period", "value") %in% names(row))) {
        year <- as.integer(row$year)
        month <- as.integer(gsub("M", "", row$period))
        value <- as.numeric(row$value)
        date <- as.Date(paste(year, month, "01", sep = "-"))
        tibble(year = year, month = month, value = value, date = date)
      } else {
        return(tibble())
      }
    })
    
    safe_df <- arrange(safe_df, date)
    
    return(safe_df)
  }
  
  all_data <- map_dfr(year_ranges, ~get_chunk(.x[1], .x[2]))
  
  all_data <- all_data %>%
    select(date, value)
  
  return(all_data)
}
###############################################################################
##################  função olinda expectativas mensais ########################
###############################################################################
get_focus_mtly_expectations <- function(indicador = "IPCA") {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  
  valid_indicators <- c(
    "Câmbio", "IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA",
    "IPCA Administrados", "IPCA Alimentação no domicílio", "IPCA Bens industrializados",
    "IPCA Livres", "IPCA Serviços", "IPCA-15", "IPC-Fipe",
    "Produção industrial", "Taxa de desocupação"
  )
  
  if (!indicador %in% valid_indicators) {
    stop("Indicador inválido. Use um dos seguintes:\n", paste(valid_indicators, collapse = ", "))
  }
  
  # Requisição
  filtro <- URLencode(paste0("?$filter=Indicador eq '", indicador, "'&$format=json"))
  url <- paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais", filtro)
  res <- GET(url)
  if (status_code(res) != 200) {
    stop("Erro ao acessar a API: ", status_code(res))
  }
  
  # Parse e tratamento
  df <- fromJSON(content(res, as = "text", encoding = "UTF-8"))$value %>%
    mutate(
      Data = as_date(Data),
      DataReferencia = dmy(paste0("01/", DataReferencia)),
      Media = as.numeric(Media),
      Mediana = as.numeric(Mediana)
    ) %>%
    filter(baseCalculo == 0, Data >= Sys.Date() - 30) %>%
    arrange(DataReferencia)
  
  # Seleciona apenas a data mais recente dentro dos últimos 30 dias
  df <- df %>%
    filter(Data == max(Data, na.rm = TRUE)) %>%
    select(Indicador, Data, DataReferencia, Media, Mediana, DesvioPadrao)
  
  return(df)
}
###############################################################################
################# função olinda móvel 12 meses a frente #######################
###############################################################################
get_expectativas_12m <- function(indicador = "IPCA", suavizada = "N") {
  # Monta o filtro incluindo baseCalculo = 1
  filtro <- URLencode(paste0(
    "?$filter=Indicador eq '", indicador,
    "' and Suavizada eq '", suavizada,
    "' and baseCalculo eq 1&$format=json"
  ))
  
  # URL final
  base_url <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoInflacao12Meses"
  full_url <- paste0(base_url, filtro)
  
  # Requisição GET
  res <- GET(full_url)
  
  # Verifica status
  if (status_code(res) != 200) {
    stop("Erro ao acessar a API: ", status_code(res))
  }
  
  # Parse do JSON
  dados <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(dados)
  
  # Limpa, organiza e remove duplicatas por data
  df <- json$value %>%
    mutate(Data = as_date(Data)) %>%
    select(Data, Media, Mediana, DesvioPadrao, Minimo, Maximo, baseCalculo) %>%
    distinct(Data, .keep_all = TRUE) %>%
    arrange(Data)
  
  return(df)
}
###############################################################################
###################  pegar dados das medidas de ipca ##########################
###############################################################################
dados.ipca <- function(){
  IPCA_12_Meses <- R_BC_Direct(13522)
  IPCA_Administrados <- R_BC_Direct(4449)
  IPCA_Administrados <- IPCA_Administrados %>%
    mutate(SGS_4449 = as.numeric(SGS_4449)) %>%
    arrange(data) %>%
    mutate(IPCA_adm_12m = (1 + SGS_4449 / 100)) %>%
    mutate(IPCA_adm_12m = zoo::rollapply(IPCA_adm_12m, 12, prod, align = "right", fill = NA)) %>%
    mutate(IPCA_adm_12m = round((IPCA_adm_12m - 1) * 100, 2)) %>%
    select(data, IPCA_adm_12m)
  IPCA_Livres <- R_BC_Direct(11428)
  IPCA_Livres <- IPCA_Livres %>%
    mutate(SGS_11428 = as.numeric(SGS_11428)) %>%
    arrange(data) %>%
    mutate(IPCA_Livres_12m = (1 + SGS_11428 / 100)) %>%
    mutate(IPCA_Livres_12m = zoo::rollapply(IPCA_Livres_12m, 12, prod, align = "right", fill = NA)) %>%
    mutate(IPCA_Livres_12m = round((IPCA_Livres_12m - 1) * 100, 2)) %>%
    select(data, IPCA_Livres_12m)
  merged_ipca <- reduce(list(IPCA_12_Meses,
                             IPCA_Administrados,
                             IPCA_Livres),
                        full_join, by = "data")
  colnames(merged_ipca) <- c("data","ipca","ipca_adm","ipca_livres")
  merged_ipca <- merged_ipca %>%
    mutate(
      data         = as.Date(data),     # If you want to convert the date column to a Date type
      ipca         = as.numeric(ipca),
      ipca_adm     = as.numeric(ipca_adm),
      ipca_livres  = as.numeric(ipca_livres)
    )
  
  
  return(merged_ipca)
}
ipca <- dados.ipca()
tail(ipca)
###############################################################################
###################  pegar expectativas atlz de ipca ##########################
###############################################################################
IPCA_Adm_Mexp <- get_focus_mtly_expectations(indicador = "IPCA Administrados")
IPCA_Livres_Mexp <- get_focus_mtly_expectations(indicador = "IPCA Livres")
IPCA_Mexp <- get_focus_mtly_expectations(indicador = "IPCA")
df_merged <- IPCA_Mexp %>%
  select(DataReferencia, Media_IPCA = Media) %>%
  left_join(IPCA_Adm_Mexp %>% select(DataReferencia, Media_IPCA_Adm = Media), by = "DataReferencia") %>%
  left_join(IPCA_Livres_Mexp %>% select(DataReferencia, Media_IPCA_Livres = Media), by = "DataReferencia")
###############################################################################
###################  converter expec mensais em a12m ##########################
###############################################################################
###para fazer isso preciso do IPCA Variação mensal das medidas de interesse ###
###############################################################################
get_and_merge_ipca_series <- function() {
  library(dplyr)
  library(zoo)
  
  # Obter dados reais
  IPCA <- R_BC_Direct(433)
  IPCA_Adm <- R_BC_Direct(4449)
  IPCA_Livres <- R_BC_Direct(11428)
  
  # Obter previsões
  IPCA_Mexp <- get_focus_mtly_expectations("IPCA")
  IPCA_Adm_Mexp <- get_focus_mtly_expectations("IPCA Administrados")
  IPCA_Livres_Mexp <- get_focus_mtly_expectations("IPCA Livres")
  
  # Função para concatenar e calcular acumulado 12 meses
  prepare_series <- function(actual_df, forecast_df, col_name){
    df_full <- actual_df %>%
      rename(DataReferencia = data, valor = {{col_name}}) %>%
      mutate(valor = as.numeric(valor)) %>%
      select(DataReferencia, valor) %>%
      bind_rows(forecast_df %>% mutate(valor = as.numeric(Media)) %>% select(DataReferencia, valor)) %>%
      arrange(DataReferencia) %>%
      mutate(acumulado_12m = round((rollapply(1 + valor / 100, width = 12, FUN = prod, fill = NA, align = "right") - 1) * 100, 2))
    return(df_full)
  }
  
  # Preparar séries individuais
  IPCA_full <- prepare_series(IPCA, IPCA_Mexp, SGS_433)
  IPCA_Adm_full <- prepare_series(IPCA_Adm, IPCA_Adm_Mexp, SGS_4449)
  IPCA_Livres_full <- prepare_series(IPCA_Livres, IPCA_Livres_Mexp, SGS_11428)
  
  # Mesclar dataframes
  df_merged <- IPCA_full %>%
    select(DataReferencia, IPCA = valor, IPCA_acum_12m = acumulado_12m) %>%
    left_join(IPCA_Adm_full %>% select(DataReferencia, IPCA_Adm = valor, IPCA_Adm_acum_12m = acumulado_12m), by = "DataReferencia") %>%
    left_join(IPCA_Livres_full %>% select(DataReferencia, IPCA_Livres = valor, IPCA_Livres_acum_12m = acumulado_12m), by = "DataReferencia")
  
  return(df_merged)
}
full <- get_and_merge_ipca_series()
###############################################################################
################ trimestralizar o dataframe end of period #####################
###############################################################################
trimestralizar_eop <- function(df) {
  df %>%
    mutate(Trimestre = as.yearqtr(DataReferencia)) %>%
    group_by(Trimestre) %>%
    filter(DataReferencia == max(DataReferencia)) %>%
    ungroup() %>%
    select(-Trimestre)
}
inflacao_qtly <- trimestralizar_eop(full)
View(inflacao_qtly)
# checar com planilha - check
###############################################################################
############## fazer expectativa 12 meses trimestre movel #####################
###############################################################################
frente12m <- get_expectativas_12m(indicador = "IPCA", suavizada = "S")
View(frente12m)


