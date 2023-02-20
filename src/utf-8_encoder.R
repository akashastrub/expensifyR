# Convert path to file to UTF-8 encoding to avoid character issues ####

to_utf8 <- function(path) {
  
  # Identify file encoding
  file_encoding <- readr::guess_encoding(path, n_max = 1000) %>% 
    head(1) %>% 
    pull(encoding)
  
  # Change file encoding to UTF-8
  writeLines(
    iconv(
      readLines(path), 
      from = file_encoding, 
      to = "UTF-8"), 
    file(path, encoding = "UTF-8")
  )

}