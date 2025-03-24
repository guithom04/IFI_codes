rm(list = ls())
###############################################################################
############################# pasta de trabalho ###############################
###############################################################################
setwd("/home/usuario/Documentos/Modelinho BACEN/simplificado")
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
              "readODS")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)
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
################  função olinda móvel 24 meses a frente #######################
###############################################################################
get_expectativas_24m <- function(indicador = "IPCA", suavizada = "N") {
  # Monta o filtro incluindo baseCalculo = 1
  filtro <- URLencode(paste0(
    "?$filter=Indicador eq '", indicador,
    "' and Suavizada eq '", suavizada,
    "' and baseCalculo eq 1&$format=json"
  ))
  
  # URL base
  base_url <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoInflacao24Meses"
  full_url <- paste0(base_url, filtro)
  
  # Requisição GET
  res <- GET(full_url)
  
  # Verificação de erro
  if (status_code(res) != 200) {
    stop("Erro ao acessar a API: ", status_code(res))
  }
  
  # Parse do JSON
  dados <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(dados)
  
  # Organiza e remove duplicatas
  df <- json$value %>%
    mutate(Data = as_date(Data)) %>%
    select(Data, Media, Mediana, DesvioPadrao, Minimo, Maximo, baseCalculo) %>%
    distinct(Data, .keep_all = TRUE) %>%
    arrange(Data)
  
  return(df)
}
###############################################################################
##################  função olinda expectativas mensais ########################
###############################################################################
get_focus_mtly_expectations <- function(indicador = "IPCA") {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(zoo)
  
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
    filter(baseCalculo == 1) %>%
    arrange(DataReferencia)
  
  # Calcula o acumulado em 12 meses para Media e Mediana
  df <- df %>%
    mutate(
      acumulado_12m_media = round((rollapply(1 + Media / 100, 12, prod, align = "right", fill = NA) - 1) * 100, 2),
      acumulado_12m_mediana = round((rollapply(1 + Mediana / 100, 12, prod, align = "right", fill = NA) - 1) * 100, 2)
    )
  
  # Filtra apenas a Data mais recente para manter coerência com o último Focus
  ultima_data <- max(df$Data, na.rm = TRUE)
  df <- df %>% 
    filter(Data == ultima_data) %>%
    select(Indicador, DataReferencia,
           Media,
           Mediana,
           DesvioPadrao, 
           acumulado_12m_media,
           acumulado_12m_mediana)
  
  
  
  return(df)
}
###############################################################################
###############################################################################
###############################################################################
set.seed(123)
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
###############################################################################
###################Planilha Modelo BC - projeção inflação######################
###############################################################################
#-----------------------------------------------------------------------------#
#-----------------------------------dados-------------------------------------#
#medidas relevantes do IPCA---------------------------------------------------#
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
#-----------------------------------------------------------------------------#
#expectativas ipca janela móvel 12 meses a frente-----------------------------#
#-----------------------------------------------------------------------------#
ipca_12m <- get_expectativas_12m(indicador = "IPCA", suavizada = "N")
ipca_12m_mensal <- ipca_12m %>%
  mutate(ano_mes = floor_date(Data, "month")) %>%
  group_by(ano_mes) %>%
  slice_max(order_by = Data, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-ano_mes)

ipca_12m_trimestral <- ipca_12m %>%
  mutate(trimestre = paste0(year(Data), "Q", quarter(Data))) %>%
  group_by(trimestre) %>%
  slice_max(order_by = Data, n = 1, with_ties = FALSE) %>%
  ungroup()

# Converter coluna data para formato yearqtr
merged_ipca_qtr <- merged_ipca %>%
  mutate(Quarter = as.yearqtr(data)) %>%
  group_by(Quarter) %>%
  slice_max(order_by = data, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Quarter, ipca, ipca_adm, ipca_livres)
# Visualizar
tail(merged_ipca_qtr)
#-----------------------------------------------------------------------------#
#expectativas ipca janela móvel 24 meses a frente-----------------------------#
#-----------------------------------------------------------------------------#
ipca_24m <- get_expectativas_24m("IPCA", "N")
ipca_24m_mensal <- ipca_24m %>%
  mutate(ano_mes = floor_date(Data, "month")) %>%
  group_by(ano_mes) %>%
  slice_max(order_by = Data, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-ano_mes)

ipca_24m_trimestral <- ipca_24m %>%
  mutate(trimestre = paste0(year(Data), "Q", quarter(Data))) %>%
  group_by(trimestre) %>%
  slice_max(order_by = Data, n = 1, with_ties = FALSE) %>%
  ungroup()

#-----------------------------------------------------------------------------#
#expectativas ipca mensais----------------------------------------------------#
#-----------------------------------------------------------------------------#
#indicadores possíveis: 'IGP-DI', 'IGP-M', 'INPC', 'IPA-DI', 'IPA-M', 'IPCA',-#
#'IPCA-15', 'IPC-Fipe', 'IPCA Administrados', 'IPCA Alimentação no domicílio',#
#'IPCA Bens industrializados', 'IPCA Livres', 'IPCA Serviços'-----------------#
#-----------------------------------------------------------------------------#
ipca_df <- get_focus_mtly_expectations("IPCA")
ipca_adm_df <- get_focus_mtly_expectations("IPCA Administrados")
ipca_livres_df <- get_focus_mtly_expectations("IPCA Livres")
#-----------------------------------------------------------------------------#
#TR/CC CRB Excess Return Historical Data (requisição manual)------------------#
#-----------------------------------------------------------------------------#
dados <- read.csv("Dados Históricos - TR_CC CRB Excess Return.csv", encoding = "UTF-8")
dados_tratados <- dados %>%
  mutate(
    Data = dmy(Data),
    Ultimo = as.numeric(gsub(",", ".", Último)),
    Abertura = as.numeric(gsub(",", ".", Abertura)),
    Maxima = as.numeric(gsub(",", ".", Máxima)),
    Minima = as.numeric(gsub(",", ".", Mínima)),
    Variacao = as.numeric(gsub(",", ".", gsub("%", "", Var.)))
  ) %>%
  select(Data, Ultimo, Abertura, Maxima, Minima, Variacao) %>%
  arrange(Data)
crb_quarterly <- dados_tratados %>%
  mutate(Quarter = as.yearqtr(Data)) %>%
  group_by(Quarter) %>%
  slice_max(order_by = Data, n = 1) %>%  # pega o último valor do trimestre
  ungroup() %>%
  select(Quarter, CRB = Ultimo)

projetar_crb_interpolado <- function(dados_tratados, taxa_anual = 0.02, final_ano = 2034) {
  library(dplyr)
  library(lubridate)
  library(zoo)
  library(pracma)
  
  # Passo 1: Série trimestral com último valor de cada trimestre
  crb_quarterly <- dados_tratados %>%
    mutate(Quarter = as.yearqtr(Data)) %>%
    group_by(Quarter) %>%
    slice_max(order_by = Data, n = 1) %>%
    ungroup() %>%
    select(Quarter, CRB = Ultimo) %>%
    distinct() %>%
    arrange(Quarter)
  
  # Último valor observado
  last_obs <- tail(crb_quarterly, 1)
  last_qtr <- last_obs$Quarter
  last_val <- last_obs$CRB
  
  # Gerar sequência trimestral futura até 2034 Q4
  future_qtrs <- seq(from = last_qtr + 0.25, to = as.yearqtr(paste0(final_ano, " Q4")), by = 0.25)
  
  # Calcular valor final com taxa de crescimento composta
  anos_futuros <- as.numeric(format(as.Date(future_qtrs[length(future_qtrs)]), "%Y")) -
    as.numeric(format(as.Date(last_qtr), "%Y"))
  
  future_val <- last_val * (1 + taxa_anual)^anos_futuros
  
  # Interpolação polinomial suave entre último valor e meta
  x_known <- c(1, length(future_qtrs))
  y_known <- c(last_val, future_val)
  x_interp <- 1:length(future_qtrs)
  y_interp <- interp1(x_known, y_known, x_interp, method = "pchip")
  
  # Criar base futura
  crb_future <- data.frame(
    Quarter = future_qtrs,
    CRB = y_interp
  )
  
  # Juntar com observado
  crb_completo <- bind_rows(crb_quarterly, crb_future)
  
  return(crb_completo)
}
crb_proj <- projetar_crb_interpolado(dados_tratados)
head(crb_proj)
tail(crb_proj)
ggplot(crb_proj, aes(x = as.Date(Quarter), y = CRB)) +
  geom_line(color = "darkgreen") +
  labs(title = "CRB Index (histórico + projeção até 2034)",
       x = "Data", y = "CRB") +
  theme_minimal()
projetar_crb_nn <- function(dados_tratados, final_ano = 2034) {
  library(dplyr)
  library(lubridate)
  library(zoo)
  library(forecast)
  
  # 1. Série trimestral com último valor de cada trimestre
  crb_quarterly <- dados_tratados %>%
    mutate(Quarter = as.yearqtr(Data)) %>%
    group_by(Quarter) %>%
    slice_max(order_by = Data, n = 1) %>%
    ungroup() %>%
    select(Quarter, CRB = Ultimo) %>%
    distinct() %>%
    arrange(Quarter)
  
  # 2. Converter para ts
  start_qtr <- c(year(min(crb_quarterly$Quarter)), quarter(min(crb_quarterly$Quarter)))
  crb_ts <- ts(crb_quarterly$CRB, start = start_qtr, frequency = 4)
  
  # 3. Calcular número de trimestres futuros
  last_qtr <- as.yearqtr(tail(crb_quarterly$Quarter, 1))
  future_qtrs <- seq(from = last_qtr + 0.25, to = as.yearqtr(paste0(final_ano, " Q4")), by = 0.25)
  n_ahead <- length(future_qtrs)
  
  # 4. Ajustar NNAR
  modelo <- nnetar(crb_ts)
  
  # 5. Projetar
  forecasted <- forecast(modelo, h = n_ahead)
  proj_vals <- as.numeric(forecasted$mean)
  
  # 6. Montar série projetada
  crb_proj <- data.frame(
    Quarter = future_qtrs,
    CRB = proj_vals
  )
  
  # 7. Combinar com série observada
  crb_final <- bind_rows(crb_quarterly, crb_proj)
  
  return(crb_final)
}
crb_nn <- projetar_crb_nn(dados_tratados)
tail(crb_nn)
ggplot(crb_nn, aes(x = as.Date(Quarter), y = CRB)) +
  geom_line(color = "purple") +
  labs(title = "Projeção do CRB com NNAR (rede neural + sazonalidade)",
       x = "Data", y = "CRB") +
  theme_minimal()
#-----------------------------------------------------------------------------#
#Câmbio evoluindo de acordo com a PPC-----------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#projeções, para o ipca usa a projeção dos administrados, a para o cpi usa a--#
#projeção do CBO.-------------------------------------------------------------#
#-----------------------------------------------------------------------------#
cambio <- R_BC_Direct(3696)
df_cpi_full <- R_BLS_Direct_Full("CUUR0000SA0")
df_cpi_full <- df_cpi_full %>%
  arrange(date) %>%
  mutate(
    yoy = (value / lag(value, 12) - 1) * 100
  ) %>%
  select(date, yoy)
#-----------------------------------------------------------------------------#
#projeções para o CPI do CBO--------------------------------------------------#
#-----------------------------------------------------------------------------#
get_cpiu_yoy_from_cbo <- function() {
  # Pacotes
  if (!require("readr")) install.packages("readr")
  if (!require("stringr")) install.packages("stringr")
  if (!require("zoo")) install.packages("zoo")
  if (!require("ggplot2")) install.packages("ggplot2")
  
  library(readr)
  library(stringr)
  library(zoo)
  library(ggplot2)
  
  # Download e unzip
  url <- "https://www.cbo.gov/system/files/2025-01/55022-2025-01-HistoricalEconomicData.zip"
  temp_zip <- tempfile()
  temp_dir <- tempdir()
  download.file(url, temp_zip, mode = "wb")
  unzip(temp_zip, exdir = temp_dir)
  
  # Localizar o CSV trimestral
  csv_path <- list.files(temp_dir, pattern = "Quarterly_January2025.csv", 
                         full.names = TRUE, recursive = TRUE)
  
  if (length(csv_path) == 0) stop("Arquivo CSV não encontrado.")
  
  # Ler CSV
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  # Identificar a coluna 'cpiu'
  cpiu_col <- names(df)[str_detect(tolower(names(df)), "^\\s*cpiu\\s*$")]
  if (length(cpiu_col) == 0) stop("Coluna 'cpiu' não encontrada.")
  
  # Extrair datas e valores
  dates <- df[[1]]
  values <- df[[cpiu_col]]
  start_year <- as.numeric(substr(dates[1], 1, 4))
  start_quarter <- as.numeric(substr(dates[1], 6, 6))
  
  # Criar série ts
  ts_cpiu <- ts(values, start = c(start_year, start_quarter), frequency = 4)
  
  # Calcular YoY
  ts_yoy <- round((ts_cpiu / stats::lag(ts_cpiu, -4) - 1) * 100, 2)
  ts_yoy_clean <- na.omit(ts_yoy)
  
  # Obter datas EOP
  dates_qtr <- as.yearqtr(time(ts_yoy_clean))
  dates_eop <- as.Date(dates_qtr, frac = 1)  # EOP
  
  # Criar data.frame final
  df_final <- data.frame(
    date_eop = dates_eop,
    yoy = as.numeric(ts_yoy_clean)
  )
  
  # Plot com ggplot2
  ggplot(df_final, aes(x = date_eop, y = yoy)) +
    geom_line(color = "darkblue", linewidth = 1) +
    labs(title = "CPIU - Variação Interanual (YoY)",
         x = "Data (EOP)", y = "%") +
    theme_minimal(base_size = 13)
  
  # Retornar o data.frame com datas EOP
  return(df_final)
}
cpiu_yoy_df <- get_cpiu_yoy_from_cbo()
tail(cpiu_yoy_df)
#-----------------------------------------------------------------------------#
#agora falta somente o hiato--------------------------------------------------#
#-----------------------------------------------------------------------------#
#baixar hiato do site da IFI--------------------------------------------------#
#-----------------------------------------------------------------------------#
arquivo_ods <- "hiato_ifi_last.ods"
dados <- read_ods(arquivo_ods, sheet = 1)
colnames(dados) <- c("data","hiato")
#-----------------------------------------------------------------------------#
#juro neutro------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
arquivo_ods <- "juro_natural_last.ods"
dados <- read_ods(arquivo_ods, sheet = 1)
colnames(dados) <- c("data","juro_natural")
#-----------------------------------------------------------------------------#
#juro real--------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
arquivo_ods <- "juro_real.ods"
dados <- read_ods(arquivo_ods, sheet = 1)
colnames(dados) <- c("data","juro_real")
#-----------------------------------------------------------------------------#
#primário estrutural----------------------------------------------------------#
#-----------------------------------------------------------------------------#
arquivo_ods <- "primario_estrutural.ods"
dados <- read_ods(arquivo_ods, sheet = 1)
colnames(dados) <- c("data","convencional","recorrente")

# juntar dfs - ipcas 


  
