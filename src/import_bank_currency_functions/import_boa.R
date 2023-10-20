# Process Bank of America file ####

import_boa <- function(path, currency = "usd") {
  
  # Convert file to UTF-8 file format
  to_utf8(path)
  
  df <- readr::read_csv(path, 
                        # skip first 6 rows of BoA summary
                        skip = 6)  %>% 
    rename(c("date" = "Date",
             "description" = "Description",
             "amount_usd" = "Amount",
             "balance" = "Running Bal.")) %>% 
    ## Filter NA rows due to BoA
    filter(!is.na(amount_usd)) %>% 
    mutate(
      date = lubridate::mdy(date),
      bank = "BoA",
      amount_chf = NA,
      amount_eur = NA, 
      amount_dkk = NA,
      amount_gbp = NA
      )  %>%
    select(
      date,
      description,
      amount_chf,
      amount_dkk, 
      amount_eur,
      amount_usd,
      amount_gbp,
      bank
    ) 
  
  return(df)
}

#df <- import_boa(path = "data/new_raw_bank_data/boa_usd_202309.csv")

