get_ipca_panels <- function(){
  
  # Required packages
  packages <- c("stringr", "dplyr", "zoo", "lubridate", "readxl", "cellranger", "tidyr", "purrr")
  
  # Install and load packages automatically
  invisible(lapply(packages, function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }))
  
  # Fetch IPCA data
  url <- "https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela7060.xlsx&terr=N&rank=-&query=t/7060/n1/all/v/63,66,2265/p/all/c315/all/d/v63%202,v66%204,v2265%202/l/,v,t%2Bc315%2Bp"
  destfile <- "tabela7060.xlsx"
  download.file(url, destfile, mode = "wb")
  
  ipca_data <- read_excel(destfile, range = cell_limits(c(3, 2), c(NA, NA))) %>%
    setNames(c("categorias", "datas", "MoM", "A12", "Peso")) %>%
    fill(categorias, .direction = "down") %>%
    filter(str_detect(categorias, "^\\d{7}\\.")) %>%
    mutate(data_date = dmy(paste0("01 ", datas), locale = "pt_BR")) %>%
    select(-datas)
  
  # Create category-wise list
  categorias_list <- ipca_data %>%
    group_split(categorias) %>%
    setNames(unique(ipca_data$categorias)) %>%
    map(~ .x %>% select(data_date, MoM, A12, Peso) %>% arrange(data_date))
  
  # Build consolidated panel
  painel_ipca <- imap(categorias_list, function(df, nome_cat) {
    df %>% rename_with(.cols = -data_date, ~ paste(nome_cat, ., sep = "."))
  }) %>% reduce(full_join, by = "data_date") %>% arrange(data_date)
  
  # Separate into MoM, A12, and Peso panels
  painel_MoM <- painel_ipca %>% select(data_date, ends_with(".MoM"))
  painel_A12 <- painel_ipca %>% select(data_date, ends_with(".A12"))
  painel_Peso <- painel_ipca %>% select(data_date, ends_with(".Peso"))
  
  # Return list of dataframes
  return(list(MoM = painel_MoM, A12 = painel_A12, Peso = painel_Peso))
}

# Example of usage
ipca_panels <- get_ipca_panels()
tail(ipca_panels$MoM)
tail(ipca_panels$A12)
tail(ipca_panels$Peso)
