# Load required packages
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

# Define the function that retrieves the series, converts it to a dataframe,
# and optionally returns the result as JSON.
R_BC_Direct <- function(codigo_sgs, return_json = FALSE) {
  # Construct the URL for the requested SGS series
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json", codigo_sgs)
  
  # Make the GET request to the API
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Error accessing the API.")
  }
  
  # Convert the JSON response to a data frame
  df <- content(response, as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  # Convert the 'data' column to Date format (assuming day-month-year format)
  df$data <- dmy(df$data)
  
  # Rename the 'valor' column to include the SGS code for clarity
  df <- df %>% rename(!!paste0("SGS_", codigo_sgs) := valor)
  
  # If return_json is TRUE, convert the data frame to JSON and return it;
  # otherwise, return the data frame.
  if (return_json) {
    json_data <- toJSON(df, dataframe = "columns", POSIXt = "ISO8601", pretty = TRUE)
    return(json_data)
  } else {
    return(df)
  }
}

# Example usage:
codigo_sgs <- 20542  # You can change this code dynamically

# To get the data as a data frame:
df_result <- peixonaut_bacen(codigo_sgs)



