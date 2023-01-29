#                               Process Revolut                             ####
# Function input variable as currency
import_revolut <- function(path, currency) {

  # User pre-selected currency
  var <- str_c("amount", tolower(currency), sep = "_")
  
  # Read in CSV and manipulate it
  df <- readr::read_csv(path) %>%
    rename(
      date = `Completed Date`,
      description = `Description`) %>% 
    mutate(date = as_date(date),
           amount_chf = NA, 
           amount_dkk = NA, 
           amount_eur = NA,
           amount_usd = NA,
           amount_gbp = NA,
           bank = str_c("Revolut ", toupper(currency)))
  
  # Name variable according to currency
  df[var] <- df["Amount"]
  
  # Select relevant columns
  df <- df %>% 
    select(date, description, amount_chf, amount_dkk, amount_eur, amount_usd,
           amount_gbp, bank)
  
  # Return output
  return(df)
}
