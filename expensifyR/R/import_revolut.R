#' Title
#'
#' @param path
#' @param currency
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate rename select
#' @importFrom lubridate as_date
#' @importFrom readr read_csv
#' @importFrom stringr str_c
#'
#' @examples
import_revolut <- function(path, currency) {
  # User pre-selected currency
  var <- stringr::str_c("amount", tolower(currency), sep = "_")

  # Read in CSV and manipulate it
  df <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::rename(date = `Completed Date`,
                  description = `Description`) |>
    dplyr::mutate(
      date = lubridate::as_date(date),
      amount_chf = NA,
      amount_dkk = NA,
      amount_eur = NA,
      amount_usd = NA,
      amount_gbp = NA,
      bank = str_c("Revolut ", toupper(currency))
    )

  # Name variable according to currency
  df[var] <- df["Amount"]

  # Select relevant columns
  df <- df |>
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
