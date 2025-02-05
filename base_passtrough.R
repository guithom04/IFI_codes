# FAZER SVAR PARA PEGAR PASSTROUGH

# Hiato do Produto do RTI
val <- c(-2.14, -1.22, -0.96, -0.51, -0.10,  0.28,  0.51,  0.49,
         0.12, -0.25, -0.35, -0.33, -0.28, -0.13,  0.03,  0.32,
         0.59,  0.89,  1.22,  1.43,  1.66,  1.49, -0.47, -1.86,
         -1.88, -1.27, -0.37,  0.22,  0.64,  0.91,  1.16,  1.33,
         1.35,  1.07,  0.85,  0.91,  0.97,  1.13,  1.40,  1.68,
         1.96,  2.18,  2.40,  2.33,  2.15,  1.85,  1.52,  0.94,
         0.17, -0.97, -1.72, -2.32, -2.87, -3.06, -3.27, -3.18,
         -3.03, -2.73, -2.27, -1.89, -1.84, -1.74, -1.65, -1.57,
         -1.61, -1.62, -1.52, -3.20, -6.41, -4.15, -2.88, -2.00,
         -1.25, -0.74, -0.33, -0.05,  0.06,  0.11,  0.11,  0.09,
         0.11,  0.15,  0.23,  0.46,  0.64,  0.74,  0.67)

# Plotando a série com linhas verticais ("h") e cor vermelha
plot(hiato, type = "h", col = "red",
     main = "Hiato do Produto Brasil - Relatório Trimestral de Inflação (Dezembro)",
     xlab = "Tempo (trimestres)",
     ylab = "Hiato do Produto")


#ipcaa12

library(httr)
library(jsonlite)

# URL da API do IBGE (ajuste conforme necessário)
url <- "https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2265/p/all/d/v2265%202"

# Realiza a requisição e obtém o conteúdo em texto
response <- GET(url)
json_text <- content(response, "text", encoding = "UTF-8")

# Converte o JSON em objeto R
json_data <- fromJSON(json_text)

# A API do IBGE normalmente retorna a primeira linha com os nomes das colunas
header  <- json_data[1, ]
data_df <- json_data[-1, ]
colnames(data_df) <- header

# Visualize a estrutura dos dados
head(data_df)


# Converte "Mês (Código)" em ano e mês
data_df$Ano <- as.numeric(substr(data_df$`Mês (Código)`, 1, 4))
data_df$Mes <- as.numeric(substr(data_df$`Mês (Código)`, 5, 6))

# Converte a coluna de valores para numérico
# Caso os números venham com vírgula, substitui por ponto.
data_df$Valor <- as.numeric(gsub(",", ".", data_df$Valor))

# Ordena os dados por ano e mês (caso não estejam ordenados)
data_df <- data_df[order(data_df$Ano, data_df$Mes), ]

# Verifique as primeiras linhas para confirmar
head(data_df[, c("Ano", "Mes", "Valor")])

# --- Criação da série temporal mensal ---
# O primeiro período será o do primeiro registro disponível
start_year  <- data_df$Ano[1]
start_month <- data_df$Mes[1]
serie_ts <- ts(data_df$Valor, start = c(start_year, start_month), frequency = 12)

# Subset the time series from January 2000 onward
ipca_a12 <- window(serie_ts, start = c(2000, 1))

# Plot the subsetted series
plot(ipca_a12,
     type = "h",
     col  = "purple",
     main = "IPCA A12 (SIDRA IBGE)",
     xlab = "Ano",
     ylab = "Valor")

# selic 
# Load required packages
library(httr)
library(jsonlite)

# Define the URL for the API
url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados?formato=json"

# Get the data from the API
response <- GET(url)
json_text <- content(response, "text", encoding = "UTF-8")

# Convert the JSON into a data frame
data_df <- fromJSON(json_text)

# Display the first few rows to inspect the data
head(data_df)

# --- Data Preparation ---

# Convert the "data" column to Date format.
# The API returns dates in the format "dd/mm/yyyy"
data_df$data <- as.Date(data_df$data, format = "%d/%m/%Y")

# Convert the "valor" column to numeric.
# If the values use a comma as decimal separator, convert it to a dot.
data_df$valor <- as.numeric(gsub(",", ".", data_df$valor))

# Order the data frame by date (if not already sorted)
data_df <- data_df[order(data_df$data), ]

# --- Subset the data from January 2000 onward ---
subset_df <- subset(data_df, format(data, "%Y-%m") >= "2000-01")

# Check the subset
head(subset_df)

# --- Create the time series object ---
# Extract the starting year and month from the subset
start_year  <- as.numeric(format(subset_df$data[1], "%Y"))
start_month <- as.numeric(format(subset_df$data[1], "%m"))

# Create the time series (monthly frequency)
selic <- ts(subset_df$valor, start = c(start_year, start_month), frequency = 12)

# Plot the time series
plot(selic,
     type = "h",
     col  = "green",
     main = "Serie 20542 - Subset from January 2000 Onward",
     xlab = "Year",
     ylab = "Valor")

# exchange rate - Índice da taxa de câmbio real efetiva (IPCA) - Jun/1994=100	(11752)
# exchange rate - Índice de taxa de câmbio real corrigida pela produtividade - Junho/1994=100	(11774)
# Load required packages
library(httr)
library(jsonlite)

# Define a helper function to download and process a series from the BCB API
get_series <- function(series_id) {
  # Construct the API URL for the given series ID
  url <- paste0("https://api.bcb.gov.br/dados/serie/bcdata.sgs.", series_id, "/dados?formato=json")
  
  # Download the data from the API
  response <- GET(url)
  json_text <- content(response, "text", encoding = "UTF-8")
  
  # Convert JSON into a data frame
  data_df <- fromJSON(json_text)
  
  # Convert the "data" column to Date (format is dd/mm/yyyy)
  data_df$data <- as.Date(data_df$data, format = "%d/%m/%Y")
  
  # Convert the "valor" column to numeric (handling possible comma as decimal separator)
  data_df$valor <- as.numeric(gsub(",", ".", data_df$valor))
  
  # Order the data by date
  data_df <- data_df[order(data_df$data), ]
  
  return(data_df)
}

# Series IDs:
# 11752: Índice da taxa de câmbio real efetiva (IPCA) - Jun/1994=100
# 11774: Índice de taxa de câmbio real corrigida pela produtividade - Jun/1994=100

# Download the two series
data_11752 <- get_series(11752)
data_11774 <- get_series(11774)

# --- Subset the data from January 2000 onward ---
subset_from_2000 <- function(df) {
  # Create a subset for dates from January 2000 onward
  subset_df <- subset(df, format(data, "%Y-%m") >= "2000-01")
  return(subset_df)
}

data_11752 <- subset_from_2000(data_11752)
data_11774 <- subset_from_2000(data_11774)

# --- Create time series objects (monthly) ---
# For series 11752
start_year_11752  <- as.numeric(format(data_11752$data[1], "%Y"))
start_month_11752 <- as.numeric(format(data_11752$data[1], "%m"))
cambio_real <- ts(data_11752$valor, start = c(start_year_11752, start_month_11752), frequency = 12)

# For series 11774
start_year_11774  <- as.numeric(format(data_11774$data[1], "%Y"))
start_month_11774 <- as.numeric(format(data_11774$data[1], "%m"))
cambio_realp <- ts(data_11774$valor, start = c(start_year_11774, start_month_11774), frequency = 12)

# --- Plot the two series ---
# Set up a 2-panel plot layout
par(mfrow = c(2, 1))

# Plot series 11752
plot(ts_11752,
     type = "h",
     col  = "pink",
     main = "Índice da taxa de câmbio real efetiva (IPCA) - Jun/1994=100",
     xlab = "Ano",
     ylab = "Valor")

# Plot series 11774
plot(ts_11774,
     type = "h",
     col  = "pink",
     main = "Índice de taxa de câmbio real corrigida pela produtividade - Jun/1994=100",
     xlab = "Ano",
     ylab = "Valor")

# petroleo brent e wti

fredr_set_key("d1f0389449f280f9bf16aefddea3dc88")
library(dplyr)
library(lubridate)
library(fredr)
library(purrr)

# Retrieve the daily WTI data
wti_data <- fredr(series_id = "DCOILWTICO", observation_start = as.Date("2004-01-01")) %>%
  as_tibble()

# Convert to monthly by grouping by the month (taking the mean)
monthly_wti <- wti_data %>%
  mutate(date = as.Date(date),
         month = floor_date(date, "month")) %>%  # floor the date to the first day of the month
  group_by(month) %>%
  summarise(wti = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = month)

# Similarly for Brent:
brent_data <- fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("2004-01-01")) %>%
  as_tibble()

monthly_brent <- brent_data %>%
  mutate(date = as.Date(date),
         month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(brent = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = month)

# Inspect the results
head(monthly_wti)
head(monthly_brent)

wti.ts <- ts(monthly_wti$wti,
             start = c(2004,01),
             frequency = 12)

brent.ts <- ts(monthly_brent$brent,
             start = c(2004,01),
             frequency = 12)


# import prices 
p_imp = ipeadatar::ipeadata(code = "FUNCEX12_MDPT12")
p_imp = p_imp %>%
  select(date, value)
p_imp <- ts(p_imp$value,
            start = c(1978,01),
            frequency = 12)



library(httr)
library(jsonlite)

# Define the URL for the API
url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.225/dados?formato=json"

# Get the data from the API
response <- GET(url)
json_text <- content(response, "text", encoding = "UTF-8")

# Convert the JSON into a data frame
data_df <- fromJSON(json_text)

# Display the first few rows to inspect the data
head(data_df)

# --- Data Preparation ---

# Convert the "data" column to Date format.
# The API returns dates in the format "dd/mm/yyyy"
data_df$data <- as.Date(data_df$data, format = "%d/%m/%Y")

# Convert the "valor" column to numeric.
# If the values use a comma as decimal separator, convert it to a dot.
data_df$valor <- as.numeric(gsub(",", ".", data_df$valor))

# Order the data frame by date (if not already sorted)
data_df <- data_df[order(data_df$data), ]

# --- Subset the data from January 2000 onward ---
subset_df <- subset(data_df, format(data, "%Y-%m") >= "2000-01")

# Check the subset
head(subset_df)

# --- Create the time series object ---
# Extract the starting year and month from the subset
start_year  <- as.numeric(format(subset_df$data[1], "%Y"))
start_month <- as.numeric(format(subset_df$data[1], "%m"))

# Create the time series (monthly frequency)
prod_prices <- ts(subset_df$valor, start = c(start_year, start_month), frequency = 12)




library(zoo)
# Example for a monthly time series (e.g., ipca_a12)
df_ipca_a12 <- data.frame(
  date = as.Date(as.yearmon(time(ipca_a12))),
  ipca_a12 = as.numeric(ipca_a12)
)

# Do the same for your other series:
df_selic <- data.frame(
  date = as.Date(as.yearmon(time(selic))),  # if selic is monthly; if daily, use as.Date(time(selic))
  selic = as.numeric(selic)
)

df_cambio_real <- data.frame(
  date = as.Date(as.yearmon(time(cambio_real))),
  cambio_real = as.numeric(cambio_real)
)

df_cambio_realp <- data.frame(
  date = as.Date(as.yearmon(time(cambio_realp))),
  cambio_realp = as.numeric(cambio_realp)
)

df_wti <- data.frame(
  date = as.Date(as.yearmon(time(wti.ts))),
  wti = as.numeric(wti.ts)
)

df_brent <- data.frame(
  date = as.Date(as.yearmon(time(brent.ts))),
  brent = as.numeric(brent.ts)
)

df_p_imp <- data.frame(
  date = as.Date(as.yearmon(time(p_imp))),
  imp = as.numeric(p_imp)
)

df_prod_prices <- data.frame(
  date = as.Date(as.yearmon(time(prod_prices))),
  prod_prices = as.numeric(prod_prices)
)

# merge

library(dplyr)
library(purrr)
library(forecast)

# Create a list of the data frames you want to merge:
list_series <- list(
  df_ipca_a12,
  df_selic,
  df_cambio_real,
  df_cambio_realp,
  df_wti,
  df_brent,
  df_p_imp,
  df_prod_prices
)

# Merge all data frames by "date"
df_merged <- reduce(list_series, full_join, by = "date") %>% arrange(date)

# Inspect the merged data frame
print(head(df_merged))

# subset

df_final <- df_merged[313:(nrow(df_merged)),] 

# balancear
balanced.NN <- function(df) {
  library(tsfgrnn)
  library(zoo)
  library(dplyr)
  
  # Ensure the 'date' column is in Date format
  df$date <- as.Date(df$date)
  
  # Function to process each column
  process_column <- function(col, col_name) {
    if (all(is.na(col))) {
      message("Column '", col_name, "' has all NA values.")
      return(col)
    } else if (any(is.na(col))) {
      # Ensure the column is a time series object
      if (!is.ts(col)) {
        # Try to infer start and frequency if not already a ts object
        start_date <- as.Date(df$date[!is.na(df$date)][1])
        start_year <- as.numeric(format(start_date, "%Y"))
        start_month <- as.numeric(format(start_date, "%m"))
        col <- ts(col, start = c(start_year, start_month), frequency = 12)
      }
      col <- na.approx(col, na.rm = FALSE) # Interpolate missing values
      
      # Check if there is enough data for forecasting
      if (length(na.omit(col)) < 2) {
        message("Not enough data to forecast for column: ", col_name)
        return(col) # Not enough data to forecast
      }
      
      fit <- tryCatch({
        tsfgrnn::grnn_forecasting(na.omit(col), h = sum(is.na(col)))
      }, error = function(e) {
        message("Forecasting failed for column: ", col_name, " with error: ", e$message)
        return(NA) # Return NA if forecasting fails
      })
      
      if (length(fit$prediction) == sum(is.na(col))) {
        col[is.na(col)] <- fit$prediction
      } else {
        # Fallback to linear interpolation if forecasting fails
        col <- na.approx(col, na.rm = FALSE)
      }
    }
    return(col)
  }
  
  # Apply the process_column function to each column
  df_result <- df %>%
    mutate(across(-date, ~process_column(., cur_column())))
  
  return(df_result)
}
balanced_NN <- as_tibble(balanced.NN(df_final))
tail(balanced_NN)

# raiz unitaria
ndiffs(balanced_NN$ipca_a12)

ndiffs(balanced_NN$selic)
d.selic <- diff(balanced_NN$selic)

ndiffs(balanced_NN$cambio_real)
d.cambio.real <- diff(balanced_NN$cambio_real)

ndiffs(balanced_NN$cambio_realp)
d.cambio.realp <- diff(balanced_NN$cambio_realp)

ndiffs(balanced_NN$wti)

ndiffs(balanced_NN$brent)

ndiffs(balanced_NN$imp)
d.imp <- diff(balanced_NN$imp)

ndiffs(balanced_NN$prod_prices)


# new df with differenced series?







library(dplyr)
library(forecast)

# Helper function: if ndiffs(x) is 0, return x unchanged.
# Otherwise, difference the series ndiffs(x) times.
diff_if_needed <- function(x) {
  nd <- ndiffs(x)
  if (nd >= 1) {
    # Compute nd differences
    for (i in seq_len(nd)) {
      x <- c(NA, diff(x))
    }
    return(x)
  } else {
    return(x)
  }
}

# Assuming balanced_NN is your balanced data frame with a Date column named "date" 
# and other numeric columns (e.g., ipca_a12, cambio_real, cambio_realp, wti, brent, imp, prod_prices, etc.)

# We create a new data frame that applies our differencing function conditionally to each column (except "date")
df_diff <- balanced_NN %>%
  mutate(across(-date, diff_if_needed))

# Inspect the first few rows of the new data frame:
print(head(df_diff))

final_df <- df_diff



