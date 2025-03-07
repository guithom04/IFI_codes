# -----------------------------------------------------------------------------
# series de atividade automatizáveis - IFI
# para nomear funções, que não pegam do IBGE, maiúsculas,
# LINGUAGEM_ORGAOFONTE 
# Para nomear funções que pegam do 
# IBGE: letras minúsculas
# linguagem_pesquisa_divisaopertinente1_divisaopertinente2_....d para dessaz
# linguagem_pesquisa_divisaopertinente1_divisaopertinente2_....nd para ndessaz
# -----------------------------------------------------------------------------
# função para pegar direto do bc
R_BC <- function(codigo_sgs){
  if(!require(dplyr)) {install.packages(dplyr); library(dplyr)}
  if(!require(httr)) {install.packages(httr); library(httr)}
  if(!require(jsonlite)) {install.packages(jsonlite); library(jsonlite)}
  if(!require(lubridate)) {install.packages(lubridate); library(lubridate)}
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json", codigo_sgs)
  response <- GET(url)
  if(status_code(response) != 200) stop("Erro ao acessar a API.")
  df <- fromJSON(content(response, as="text", encoding="UTF-8"), flatten=TRUE)
  df$data <- dmy(df$data)
  df <- df %>% rename(value = valor)
  return(df)
}
# -----------------------------------------------------------------------------
# IBC-Br
# -----------------------------------------------------------------------------
BCB.IBCBR.d <- R_BC(24364)
BCB.IBCBR.nd <- R_BC(24363)
# -----------------------------------------------------------------------------
# PIM e aberturas - Seções e Atividades
# -----------------------------------------------------------------------------
r_pim_sec_nd <- function(setor) {
  
  if (!require(httr)) {install.packages("httr"); library(httr)}
  if (!require(jsonlite)) {install.packages("jsonlite"); library(jsonlite)}
  if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
  
  url <- "https://apisidra.ibge.gov.br/values/t/8888/n1/all/v/12606,12607/p/all/c544/129314,129315,129316/d/v12606%205,v12607%205"
  response <- GET(url)
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  
  colnames(df) <- df[1,]
  df <- df[-1,]
  
  df <- df %>% 
    select(`Nível Territorial`, `Unidade de Medida`, `Variável`,
           `Seções e atividades industriais (CNAE 2.0)`, `Mês`, Valor) %>%
    rename(Nivel_Territorial = `Nível Territorial`,
           Unidade_Medida = `Unidade de Medida`,
           Variavel = `Variável`,
           Setor = `Seções e atividades industriais (CNAE 2.0)`,
           Mes = `Mês`) %>%
    mutate(Mes = my(gsub("janeiro","01",gsub("fevereiro","02",gsub("março","03",
                                                                   gsub("abril","04",gsub("maio","05",gsub("junho","06",
                                                                                                           gsub("julho","07",gsub("agosto","08",gsub("setembro","09",
                                                                                                                                                     gsub("outubro","10",gsub("novembro","11",gsub("dezembro","12",Mes))))))))))))),
           Valor = as.numeric(Valor))
  
  df_filtered <- df %>%
    filter(Setor == setor)
  
  return(df_filtered)
}
# Indústria Geral
ibge.pim.sec.ig.nd <- r_pim_sec_nd("1 Indústria geral")
tail(ibge.pim.sec.ig.nd)
# Indústrias Extrativas
ibge.pim.sec.ext.nd <- r_pim_sec_nd("2 Indústrias extrativas")
tail(ibge.pim.sec.ext.nd)
# Indústrias de Transformação
ibge.pim.sec.trans.nd <- r_pim_sec_nd("3 Indústrias de transformação")
tail(ibge.pim.sec.trans.nd)
# -----------------------------------------------------------------------------
# PIM e aberturas - Seções e Atividades
# -----------------------------------------------------------------------------
