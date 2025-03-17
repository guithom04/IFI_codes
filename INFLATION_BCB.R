save_inflation_data <- function(directory, file_name) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    install.packages("openxlsx")
    library(openxlsx)
  }
  library(httr)
  if (!requireNamespace("httr", quietly = TRUE)) {
    install.packages("httr")
    library(httr)
  }
  library(jsonlite)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
    library(jsonlite)
  }
  library(lubridate)
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    install.packages("lubridate")
    library(lubridate)
  }
  library(dplyr)
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
    library(dplyr)
  }
  library(purrr)
  if (!requireNamespace("purrr", quietly = TRUE)) {
    install.packages("purrr")
    library(purrr)
  }
  # Define the function that retrieves the series, converts it to a dataframe,
  # and optionally returns the result as JSON.
  R_BC_Direct <- function(codigo_sgs, return_json = FALSE) {
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
  
  
  # IPCA Variação Mensal
  IPCA_Variacao_Mensal <- R_BC_Direct(433)
  
  # IPCA 12 meses
  IPCA_12_Meses <- R_BC_Direct(13522)
  
  # IPCA Comercializáveis
  IPCA_Comercializaveis <- R_BC_Direct(4447)
  
  # IPCA Não Comercializáveis
  IPCA_Nao_Comercializaveis <- R_BC_Direct(4448)
  
  # IPCA Administrados
  IPCA_Administrados <- R_BC_Direct(4449)
  
  # IPCA MS
  IPCA_MS <- R_BC_Direct(4466)
  
  # IPCA - EX0
  IPCA_EX0 <- R_BC_Direct(11427)
  
  # IPCA - EX1
  IPCA_EX1 <- R_BC_Direct(16121)
  
  # IPCA - EX2
  IPCA_EX2 <- R_BC_Direct(27838)
  
  # IPCA - EX3
  IPCA_EX3 <- R_BC_Direct(27839)
  
  # IPCA Não Duráveis
  IPCA_Nao_Duraveis <- R_BC_Direct(10841)
  
  # IPCA Semiduráveis
  IPCA_Semiduraveis <- R_BC_Direct(10842)
  
  # IPCA Duráveis
  IPCA_Duraveis <- R_BC_Direct(10843)
  
  # IPCA - Serviços
  IPCA_Servicos <- R_BC_Direct(10844)
  
  # IPCA - Livres
  IPCA_Livres <- R_BC_Direct(11428)
  
  # IPCA Preços Livres - Alimentação no domicílio
  IPCA_Livres_Alimentacao_Domicilio <- R_BC_Direct(27864)
  
  # IPCA Preços Livres - Industriais
  IPCA_Livres_Industriais <- R_BC_Direct(27863)
  
  # IPCA Preços Livres - Serviços
  IPCA_Livres_Servicos <- R_BC_Direct(10844)
  
  # IPCA Difusão
  IPCA_Difusao <- R_BC_Direct(21379)
  
  # IPCA - Núcleo médias aparadas com suavização
  IPCA_Nucleo_Medias_Aparadas_Suavizacao <- R_BC_Direct(4466)
  
  # IPCA - Núcleo médias aparadas sem suavização
  IPCA_Nucleo_Medias_Aparadas_Sem_Suavizacao <- R_BC_Direct(11426)
  
  # IPCA - Núcleo por exclusão - EX0
  IPCA_Nucleo_Exclusao_EX0 <- R_BC_Direct(11427)
  
  # IPCA - Núcleo por exclusão - EX1
  IPCA_Nucleo_Exclusao_EX1 <- R_BC_Direct(16121)
  
  # IPCA - Núcleo por exclusão - EX2
  IPCA_Nucleo_Exclusao_EX2 <- R_BC_Direct(27838)
  
  # IPCA - Núcleo por exclusão - EX3
  IPCA_Nucleo_Exclusao_EX3 <- R_BC_Direct(27839)
  
  # IPCA - Núcleo Percentil 55
  IPCA_Nucleo_Percentil_55 <- R_BC_Direct(28750)
  
  # IPCA - Núcleo de dupla ponderação
  IPCA_Nucleo_Dupla_Ponderacao <- R_BC_Direct(16122)
  
  # Renomeando a coluna de cada dataframe para o nome correto
  IPCA_Variacao_Mensal <- IPCA_Variacao_Mensal %>% rename(IPCA_Variacao_Mensal = SGS_433)
  IPCA_12_Meses <- IPCA_12_Meses %>% rename(IPCA_12_Meses = SGS_13522)
  IPCA_Comercializaveis <- IPCA_Comercializaveis %>% rename(IPCA_Comercializaveis = SGS_4447)
  IPCA_Nao_Comercializaveis <- IPCA_Nao_Comercializaveis %>% rename(IPCA_Nao_Comercializaveis = SGS_4448)
  IPCA_Administrados <- IPCA_Administrados %>% rename(IPCA_Administrados = SGS_4449)
  IPCA_MS <- IPCA_MS %>% rename(IPCA_MS = SGS_4466)
  IPCA_EX0 <- IPCA_EX0 %>% rename(IPCA_EX0 = SGS_11427)
  IPCA_EX1 <- IPCA_EX1 %>% rename(IPCA_EX1 = SGS_16121)
  IPCA_EX2 <- IPCA_EX2 %>% rename(IPCA_EX2 = SGS_27838)
  IPCA_EX3 <- IPCA_EX3 %>% rename(IPCA_EX3 = SGS_27839)
  IPCA_Nao_Duraveis <- IPCA_Nao_Duraveis %>% rename(IPCA_Nao_Duraveis = SGS_10841)
  IPCA_Semiduraveis <- IPCA_Semiduraveis %>% rename(IPCA_Semiduraveis = SGS_10842)
  IPCA_Duraveis <- IPCA_Duraveis %>% rename(IPCA_Duraveis = SGS_10843)
  IPCA_Servicos <- IPCA_Servicos %>% rename(IPCA_Servicos = SGS_10844)
  IPCA_Livres <- IPCA_Livres %>% rename(IPCA_Livres = SGS_11428)
  IPCA_Livres_Alimentacao_Domicilio <- IPCA_Livres_Alimentacao_Domicilio %>% rename(IPCA_Livres_Alimentacao_Domicilio = SGS_27864)
  IPCA_Livres_Industriais <- IPCA_Livres_Industriais %>% rename(IPCA_Livres_Industriais = SGS_27863)
  IPCA_Livres_Servicos <- IPCA_Livres_Servicos %>% rename(IPCA_Livres_Servicos = SGS_10844)
  IPCA_Difusao <- IPCA_Difusao %>% rename(IPCA_Difusao = SGS_21379)
  IPCA_Nucleo_Medias_Aparadas_Suavizacao <- IPCA_Nucleo_Medias_Aparadas_Suavizacao %>% rename(IPCA_Nucleo_Medias_Aparadas_Suavizacao = SGS_4466)
  IPCA_Nucleo_Medias_Aparadas_Sem_Suavizacao <- IPCA_Nucleo_Medias_Aparadas_Sem_Suavizacao %>% rename(IPCA_Nucleo_Medias_Aparadas_Sem_Suavizacao = SGS_11426)
  IPCA_Nucleo_Exclusao_EX0 <- IPCA_Nucleo_Exclusao_EX0 %>% rename(IPCA_Nucleo_Exclusao_EX0 = SGS_11427)
  IPCA_Nucleo_Exclusao_EX1 <- IPCA_Nucleo_Exclusao_EX1 %>% rename(IPCA_Nucleo_Exclusao_EX1 = SGS_16121)
  IPCA_Nucleo_Exclusao_EX2 <- IPCA_Nucleo_Exclusao_EX2 %>% rename(IPCA_Nucleo_Exclusao_EX2 = SGS_27838)
  IPCA_Nucleo_Exclusao_EX3 <- IPCA_Nucleo_Exclusao_EX3 %>% rename(IPCA_Nucleo_Exclusao_EX3 = SGS_27839)
  IPCA_Nucleo_Percentil_55 <- IPCA_Nucleo_Percentil_55 %>% rename(IPCA_Nucleo_Percentil_55 = SGS_28750)
  IPCA_Nucleo_Dupla_Ponderacao <- IPCA_Nucleo_Dupla_Ponderacao %>% rename(IPCA_Nucleo_Dupla_Ponderacao = SGS_16122)
  
  # Realizando o merge de todos os dataframes
  merged_data <- reduce(list(
    IPCA_Variacao_Mensal, 
    IPCA_12_Meses, 
    IPCA_Comercializaveis, 
    IPCA_Nao_Comercializaveis, 
    IPCA_Administrados, 
    IPCA_MS, 
    IPCA_EX0, 
    IPCA_EX1, 
    IPCA_EX2, 
    IPCA_EX3, 
    IPCA_Nao_Duraveis, 
    IPCA_Semiduraveis, 
    IPCA_Duraveis, 
    IPCA_Servicos, 
    IPCA_Livres, 
    IPCA_Livres_Alimentacao_Domicilio, 
    IPCA_Livres_Industriais, 
    IPCA_Livres_Servicos, 
    IPCA_Difusao, 
    IPCA_Nucleo_Medias_Aparadas_Suavizacao, 
    IPCA_Nucleo_Medias_Aparadas_Sem_Suavizacao, 
    IPCA_Nucleo_Exclusao_EX0, 
    IPCA_Nucleo_Exclusao_EX1, 
    IPCA_Nucleo_Exclusao_EX2, 
    IPCA_Nucleo_Exclusao_EX3, 
    IPCA_Nucleo_Percentil_55, 
    IPCA_Nucleo_Dupla_Ponderacao
  ), full_join, by = "data")
  
  
  
  file_path <- file.path(directory, file_name)
  
  
  # Save merged_data to an Excel file at the specified location
  write.xlsx(merged_data, file = file_path)
  
  message("File saved to: ", file_path)
}
save_inflation_data("U:/Pastas pessoais/LucasM/IPCA_BACEN",
                    file_name = "infl.xlsx")
