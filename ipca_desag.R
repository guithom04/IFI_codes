setwd("U:/Pastas pessoais/LucasM/IPCA_BACEN")
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


library(readxl)
Núcleos_IPCA <- read_excel("U:/Pastas pessoais/LucasM/Downloads/Núcleos IPCA.xlsx")
sum(is.na(Núcleos_IPCA))


# Items in Núcleos_IPCA:
items_nucleos <- Núcleos_IPCA$Item

# Items in painel MoM:
items_ipca_panel <- colnames(ipca_panels$MoM)[-1] %>% str_remove(".MoM")

# Check if all items from Núcleos_IPCA exist in painel MoM
setdiff(items_nucleos, items_ipca_panel)



library(dplyr)
library(stringr)

build_ipca_indicators <- function(nucleos_df, painel_MoM, painel_Peso){
  
  items <- colnames(painel_MoM)[-1] %>% str_remove("\\.MoM")
  
  indicators_df <- painel_MoM %>% select(data_date)
  
  for(indicator in colnames(nucleos_df)[-1]){ # skip first column 'Item'
    
    included_items <- nucleos_df %>% 
      filter(Item %in% items, .data[[indicator]] == 1) %>% 
      pull(Item)
    
    MoM_cols <- paste0(included_items, ".MoM")
    Peso_cols <- paste0(included_items, ".Peso")
    
    MoM_data <- painel_MoM %>% select(all_of(MoM_cols))
    Peso_data <- painel_Peso %>% select(all_of(Peso_cols))
    
    indicator_values <- rowSums(MoM_data * Peso_data, na.rm = TRUE) / rowSums(Peso_data, na.rm = TRUE)
    
    indicator_values <- round(indicator_values, 2)
    
    indicators_df[[indicator]] <- indicator_values
  }
  
  return(indicators_df)
}

ipca_indicators_panel <- build_ipca_indicators(
  nucleos_df = Núcleos_IPCA, 
  painel_MoM = ipca_panels$MoM, 
  painel_Peso = ipca_panels$Peso
)

# Quickly confirm result
head(ipca_indicators_panel)
tail(ipca_indicators_panel)

# see the result in excel and compare with central bank data
writexl::write_xlsx(ipca_indicators_panel, path = "medidas_inflacao.xlsx")














































