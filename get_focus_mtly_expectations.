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
    filter(baseCalculo == 0) %>%
    filter(Data >= Sys.Date() - 30) %>%
    arrange(DataReferencia)
  
  # Calcula o acumulado em 12 meses para Media e Mediana
  df <- df %>%
    group_by(DataReferencia) %>%
    arrange(Data) %>%
    mutate(
      acumulado_12m_media = round((rollapply(1 + Media / 100, 12, prod, align = "right", fill = NA) - 1) * 100, 2),
      acumulado_12m_mediana = round((rollapply(1 + Mediana / 100, 12, prod, align = "right", fill = NA) - 1) * 100, 2)
    ) %>%
    ungroup()
  
  # Seleciona apenas a data mais recente dentro dos últimos 30 dias
  df <- df %>%
    filter(Data == max(Data, na.rm = TRUE)) %>%
    select(Indicador, Data, DataReferencia,
           Media, Mediana, DesvioPadrao,
           acumulado_12m_media, acumulado_12m_mediana)
  
  return(df)
}
ipca = get_focus_mtly_expectations(indicador = "IPCA Administrados")
View(ipca)
# aprendi
