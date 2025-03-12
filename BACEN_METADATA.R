bc.metadata <- function(sgs_code){
  library(httr)
  library(rvest)
  
  # Define the main page URL
  main_url <- paste0("https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarmetadadosSeriesInternet&hdOidSerieSelecionada=", sgs_code)
  
  # Request the main page with a browser-like User-Agent and headers
  main_resp <- GET(
    main_url, 
    add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "pt-BR,pt;q=0.5"
    )
  )
  
  # Retrieve cookies and convert them to a named vector
  session_cookies <- cookies(main_resp)
  cookies_vector <- with(session_cookies, setNames(value, name))
  
  # Construct the full iframe URL using its relative path
  iframe_relative <- "/sgspub/JSP/consultarmetadados/cmiDadosBasicos.jsp"
  iframe_url <- paste0("https://www3.bcb.gov.br", iframe_relative)
  cat("Iframe URL:", iframe_url, "\n")
  
  # Request the iframe content with additional headers and the cookies from the main page
  iframe_resp <- GET(
    iframe_url, 
    add_headers(
      Referer = main_url,
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "pt-BR,pt;q=0.5"
    ),
    set_cookies(.cookies = cookies_vector)
  )
  if (status_code(iframe_resp) != 200) stop("Failed to retrieve the iframe page")
  
  cat("Iframe page status:", status_code(iframe_resp), "\n")
  cat("Iframe content type:", headers(iframe_resp)$`content-type`, "\n")
  
  # IMPORTANT: Use ISO-8859-1 encoding since the content type indicates it
  iframe_content <- content(iframe_resp, "text", encoding = "ISO-8859-1")
  if (is.na(iframe_content) || nchar(iframe_content) == 0) {
    stop("The content of the iframe is NA or empty. Check if the GET request is returning the expected HTML.")
  }
  
  # Parse the iframe HTML content
  iframe_page <- read_html(iframe_content)
  
  # Extract the desired text from the span element with class "textoPequeno"
  target_text <- iframe_page %>%
    html_nodes("span.textoPequeno") %>%
    html_text(trim = TRUE)
  
  print(target_text[4])
  return(target_text[4])
} 
bc.metadata(432)
