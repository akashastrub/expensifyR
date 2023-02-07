# Process Danske Bank file ####

import_danske <- function(path, currency = "dkk") {
  
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
  
  # Import data and rename
  df <- read.csv(path) %>% 
    rename(
      c('date' = 'Dato',
        'description' = 'Tekst')) 
  
  # Rename indexing due to Danish text
  colnames(df)[5] <- 'amount_dkk'
  
  # Mutate amounts, add columns, rearrange columns
  df <- df %>% 
    mutate(amount_dkk = str_replace(amount_dkk, '\\.', '')) %>% 
    mutate(amount_dkk = str_replace(amount_dkk, ',', '.')) %>% 
    mutate(amount_dkk = as.numeric(amount_dkk)) %>% 
    mutate(
      date = dmy(date),
      amount_chf = NA,
      amount_gbp = NA,
      amount_eur = NA,
      amount_usd = NA,
      bank = "DanskeBank"
    ) %>%
    select(date, description, amount_chf, amount_dkk, amount_eur, amount_usd,
           amount_gbp, bank)
  
  # Return dataframe
  return(df)
}
