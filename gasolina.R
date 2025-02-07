# Paridade de Preço de Importação - Análise
# Detalhes Importantes: Checar Diesel S10 
# Checar Localidades, Checar Preços nas Cadeias de Produção

# BAIXAR O PDF - ATUALIZAR O LINK QUANDO MUDAR A DATA (ULTIMA VERSÃO 01.01.2025)
install.packages("httr")
install.packages("pdftools")
library(httr)
library(stringr)
library(pdftools)
library(tidyr)

# Definir a URL do PDF (atualizar de https://precos.petrobras.com.br/)
url_gasolina <- "https://precos.petrobras.com.br/documents/d/precos-dos-combustiveis/tabelas-de-precos-gasolina-01-02-25-pdf"
url_diesel <- "https://precos.petrobras.com.br/documents/d/precos-dos-combustiveis/tabelas-de-precos-diesel-s500-e-s10-01-02-25-pdf"

# Definir o nome do arquivo onde o PDF será salvo
arquivo_pdf_gasolina <- "gasolina.pdf"
arquivo_pdf_diesel <- "diesel.pdf"

# Fazer o download do PDF
download.file(url_gasolina, arquivo_pdf_gasolina, mode = "wb")
download.file(url_diesel, arquivo_pdf_diesel, mode = "wb")

# Verificar se o arquivo foi baixado com sucesso
file.exists(arquivo_pdf_gasolina)
file.exists(arquivo_pdf_diesel)

# Extrair o texto dos PDFs
texto_gasolina <- pdf_text(arquivo_pdf_gasolina)
texto_diesel <- pdf_text(arquivo_pdf_diesel)

# Mostrar um trecho do texto extraído (primeira página)
cat(texto_gasolina[11])
cat(texto_diesel[11])

# Definir os caminhos completos dos arquivos PDF
library(httr)
library(pdftools)
library(tidyverse)

# Define PDF URLs (Update when necessary)
url_gasolina <- "https://precos.petrobras.com.br/documents/d/precos-dos-combustiveis/tabelas-de-precos-gasolina-01-02-25-pdf"
url_diesel <- "https://precos.petrobras.com.br/documents/d/precos-dos-combustiveis/tabelas-de-precos-diesel-s500-e-s10-01-02-25-pdf"

# Define local file names
arquivo_gasolina <- "gasolina.pdf"
arquivo_diesel <- "diesel.pdf"

# Download PDFs
download.file(url_gasolina, arquivo_gasolina, mode = "wb")
download.file(url_diesel, arquivo_diesel, mode = "wb")

# Check if download was successful
if (!file.exists(arquivo_gasolina) | !file.exists(arquivo_diesel)) {
  stop("Error downloading the PDFs! Check the URLs.")
}

# Extract text from PDF
texto_gasolina <- pdf_text(arquivo_gasolina)
texto_diesel <- pdf_text(arquivo_diesel)

# Function to extract tables from text
extrair_tabela_simples <- function(texto_pdf) {
  linhas <- unlist(strsplit(texto_pdf, "\n"))  # Split text into lines
  linhas <- trimws(linhas)  # Remove extra spaces
  linhas <- linhas[linhas != ""]  # Remove empty lines
  
  # Find header position (date format in DD.MM.YYYY)
  header_index <- which(grepl("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", linhas))[1]
  if (is.na(header_index)) return(NULL)  # If no header, return NULL
  
  cabecalho <- unlist(strsplit(linhas[header_index], "\\s{2,}"))
  dados <- linhas[(header_index + 1):length(linhas)]  # Capture data
  
  dados_lista <- lapply(dados, function(linha) {
    str_split_fixed(linha, "\\s{2,}", n = length(cabecalho))
  })
  
  df_temp <- as.data.frame(do.call(rbind, dados_lista), stringsAsFactors = FALSE)
  colnames(df_temp) <- cabecalho  # Set column names
  
  return(df_temp)
}

# Apply function to extract tables
tabela_gasolina <- extrair_tabela_simples(texto_gasolina[1])
tabela_diesel <- extrair_tabela_simples(texto_diesel[1])

# Print extracted tables
print(head(tabela_gasolina, 10))
print(head(tabela_diesel, 10))


View(tabela_gasolina)




































