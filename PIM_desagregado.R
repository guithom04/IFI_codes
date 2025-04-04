PIM_desagregado <- function(){
  # Instale (caso ainda não tenha) e carregue o pacote readxl
  install.packages("readxl")
  library(readxl)
  
  # Passo 1: Baixe o arquivo Excel da URL
  url <- "https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela8888.xlsx&terr=N&rank=-&query=t/8888/n1/all/v/12606,12607/p/all/c544/56689,129317,129318,129319,129320,129321,129322,129323,129324,129325,129326,129330,129331,129332,129333,129334,129335,129336,129337,129338,129339,129340,129341,129342/d/v12606%205,v12607%205/l/v,c544,t%2Bp"
  download.file(url, "tabela8888.xlsx", mode = "wb")
  
  # Passo 2: Importe o arquivo Excel para um dataframe
  df <- read_excel("tabela8888.xlsx")
  
  # filtrar o que quero
  df <- df[-c(1:2),-1]
  df <- df[-nrow(df),]
  
  
  
  # Carrega pacotes necessários
  library(dplyr)
  library(tibble)
  library(stringr)
  library(lubridate)
  
  # 1) Transformar a primeira linha de df em nomes de coluna
  #    Atribuímos os valores da primeira linha como colnames
  colnames(df) <- as.character(unlist(df[1, ]))
  
  # E removemos a linha que virou cabeçalho
  df <- df[-1, ]
  
  # 2) Renomear a primeira coluna para "Data" (ou o nome que preferir)
  colnames(df)[1] <- "Data"
  
  # Agora 'df$Data' contém textos como "janeiro 2020", "fevereiro 2020", etc.
  # Se quiser converter essas strings em datas de verdade, faça algo como:
  df <- df %>%
    mutate(
      Mes_num = case_when(
        str_detect(Data, regex("janeiro",    ignore_case=TRUE))    ~ 1,
        str_detect(Data, regex("fevereiro",  ignore_case=TRUE))    ~ 2,
        str_detect(Data, regex("março",      ignore_case=TRUE))    ~ 3,
        str_detect(Data, regex("abril",      ignore_case=TRUE))    ~ 4,
        str_detect(Data, regex("maio",       ignore_case=TRUE))    ~ 5,
        str_detect(Data, regex("junho",      ignore_case=TRUE))    ~ 6,
        str_detect(Data, regex("julho",      ignore_case=TRUE))    ~ 7,
        str_detect(Data, regex("agosto",     ignore_case=TRUE))    ~ 8,
        str_detect(Data, regex("setembro",   ignore_case=TRUE))    ~ 9,
        str_detect(Data, regex("outubro",    ignore_case=TRUE))    ~ 10,
        str_detect(Data, regex("novembro",   ignore_case=TRUE))    ~ 11,
        str_detect(Data, regex("dezembro",   ignore_case=TRUE))    ~ 12,
        TRUE ~ NA_real_
      ),
      Ano = str_extract(Data, "\\d{4}"),
      # Cria uma coluna DataIndex (objeto date), assumindo dia 1
      DataIndex = make_date(as.numeric(Ano), Mes_num, 1)
    )
  
  # 3) Transformar em tibble (se ainda não for)
  df <- as_tibble(df)
  
  # Vamos espiar as primeiras linhas
  return(df)
  
}
pim_desagregado <- PIM_desagregado()
tail(pim_desagregado)
