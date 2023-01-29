# Process Natwest file ####

import_natwest <- function(path, currency = "gbp") {
  
  df <- readr::read_csv(path) %>% 
    rename(
      c(date = Date,
        description = Description,
        amount_gbp = Value)
      ) %>% 
    ## Filter NA rows due to Natwest export
    filter(!is.na(amount_gbp)) %>%
    mutate(
      date = lubridate::dmy(date),
      bank = "NatWest",
      amount_chf = NA,
      amount_eur = NA, 
      amount_dkk = NA,
      amount_usd = NA
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
