#' Process Revolut file
#'
#' @param path Path of raw Revolut file
#' @param currency Currency of Revolut file
#'
#' @return Same data in common data format.
#' @import readr stringr lubridate
#' @export
#'
import_revolut <- function(path, currency) {
  # Convert file to UTF-8 file format
  expensifyR::to_utf8(path)

  # User pre-selected currency
  var <- stringr::str_c("amount", tolower(currency), sep = "_")

  # Read in CSV and manipulate it
  df <- readr::read_csv(path) %>%
    dplyr::rename(date = `Completed Date`,
                  description = `Description`) %>%
    dplyr::mutate(
      date = lubridate::as_date(date),
      amount_chf = NA,
      amount_dkk = NA,
      amount_eur = NA,
      amount_usd = NA,
      amount_gbp = NA,
      bank = stringr::str_c("Revolut ", toupper(currency))
    )

  # Name variable according to currency
  df[var] <- df["Amount"]

  # Select relevant columns
  df <- df %>%
    dplyr::select(date,
                  description,
                  amount_chf,
                  amount_dkk,
                  amount_eur,
                  amount_usd,
                  amount_gbp,
                  bank)

  # Return output
  return(df)
}
