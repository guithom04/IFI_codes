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
