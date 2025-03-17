IPCA_DESAG_IBGE <- function(){
  # todos os dados relevantes IPCA desagregado IBGE
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  library(readxl)
  if (!requireNamespace("cellranger", quietly = TRUE)) {
    install.packages("cellranger")
  }
  library(cellranger)
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    install.packages("tidyr")
  }
  library(tidyr)
  
  url <- "https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela7060.xlsx&terr=N&rank=-&query=t/7060/n1/all/v/63,66,2265/p/all/c315/all/d/v63%202,v66%204,v2265%202/l/,v,t%2Bc315%2Bp"
  destfile <- "tabela7060.xlsx"
  download.file(url, destfile, mode = "wb")
  true_table <- read_excel(destfile, range = cellranger::cell_limits(c(3, 2), c(NA, NA)))
  View(true_table)
  colnames(true_table) <- c("categorias",
                            "datas",
                            "MoM",
                            "A12",
                            "Peso")
  true_table <- true_table %>% fill(categorias, .direction = "down")
  return(true_table)
}
ipca.d <- IPCA_DESAG_IBGE()

