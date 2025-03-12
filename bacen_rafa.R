setwd("U:/Macro/Projeções/dados R/BACEN_NOVO")
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(writexl)
pkg <- c("dplyr", "httr", "jsonlite", "lubridate", "writexl")
sapply(pkg, function(x) if (!require(x, character.only = TRUE)) install.packages(x))
R_BC <- function(codigo_sgs, tentativas = 5, espera_segundos = 10){
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json", codigo_sgs)
  tentativa_atual <- 1
  
  while(tentativa_atual <= tentativas){
    
    response <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    content_type <- headers(response)$`content-type`
    
    if(status_code(response) == 200 && grepl("application/json", content_type)){
      
      df <- fromJSON(content(response, as="text", encoding="UTF-8"), flatten=TRUE)
      df$data <- dmy(df$data)
      df$valor <- as.numeric(gsub(",", ".", df$valor, fixed = TRUE))
      
      nome_coluna <- sprintf("SGS_%s", codigo_sgs)
      df <- df %>% rename(!!nome_coluna := valor)
      
      message(sprintf("SGS %s baixado com sucesso na tentativa %d", codigo_sgs, tentativa_atual))
      return(df)
      
    } else {
      message(sprintf("Falha na tentativa %d para SGS %s. Tentando novamente em %d segundos...", tentativa_atual, codigo_sgs, espera_segundos))
      Sys.sleep(espera_segundos)
      tentativa_atual <- tentativa_atual + 1
    }
  }
  
  stop(sprintf("Falha ao baixar a série SGS %s após %d tentativas.", codigo_sgs, tentativas))
}
series <- list(
  automotivo = c(1373,
                 1378,
                 7384,
                 28527,
                 28528),
  abras = c(28549),
  pib_mensal = c(4380),
  inflacao = c(190,
               189,
               1619,
               188,
               433,
               11428,
               10841,
               10842,
               10843,
               10844,
               4449,
               16122,
               16121,
               11427,
               11426,
               4466,
               27838,
               27839,
               28750,
               21379,
               4447,
               4448,
               27863,
               27864,
               1635,
               1636,
               1637,
               1638,
               1639,
               1640, 
               1641,
               1642,
               1643,
               7450,
               7453,
               7456),
  commodities = c(27574,
                  27575,
                  27576,
                  27577,
                  29042),
  renda_disponivel = c(29023,
                       29024,
                       29025,
                       29026,
                       29028),
  ibc_br = c(24363,
             24364),
  cambio_mensal = c(3696,
                    3698,
                    11752),
  cambio_diario = c(1),
  selic = c(432),
  caged = c(28763,
            28784,
            28764,
            28766,
            28770,
            28771,
            28772,
            28785,
            28787,
            28791,
            28792,
            28793),
  nuci = c(24352,
           28561)
)

series_com_erro <- c()

for (grupo in names(series)){
  codigos <- series[[grupo]]
  
  lista_df <- list()
  
  for (codigo in codigos){
    resultado <- tryCatch(
      R_BC(codigo),
      error = function(e) {
        message(sprintf("Erro ao baixar a série SGS %s: %s", codigo, e$message))
        series_com_erro <<- c(series_com_erro, codigo)
        return(NULL)
      }
    )
    
    if (!is.null(resultado)){
      lista_df[[as.character(codigo)]] <- resultado
    }
    Sys.sleep(5)  # espera de 5 segundos entre cada requisição
  }
  
  if (length(lista_df) > 0){
    df_final <- reduce(lista_df, full_join, by = "data") %>% arrange(data)
    write_xlsx(df_final, paste0("dados_sgs_", grupo, ".xlsx"))
    message(sprintf("Arquivo dados_sgs_%s.xlsx salvo com sucesso.", grupo))
  } else {
    message(sprintf("Nenhuma série do grupo %s foi baixada com sucesso.", grupo))
  }
}

if (length(series_com_erro) > 0) {
  message("As seguintes séries não foram baixadas após todas as tentativas:")
  print(unique(series_com_erro))
} else {
  message("Todas as séries foram baixadas com sucesso.")
}
