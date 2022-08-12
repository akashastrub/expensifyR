#' Title
#'
#' @param path
#' @param currency
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate rename select
#' @importFrom lubridate dmy
#' @importFrom readr read_csv
#' @importFrom stringr str_c
#'
#' @examples
import_danske <- function(path, currency = "dkk") {

  # Import data and rename
  df <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::rename(
      c('date' = 'Dato',
        'description' = 'Tekst'))

  # Rename indexing due to Danish text
  colnames(df)[5] <- 'amount_dkk'

  # Mutate amounts, add columns, rearrange columns
  df <- df |>
    dplyr::mutate(amount_dkk = stringr::str_replace(amount_dkk, '\\.', '')) |>
    dplyr::mutate(amount_dkk = stringr::str_replace(amount_dkk, ',', '.')) |>
    dplyr::mutate(amount_dkk = as.numeric(amount_dkk)) |>
    dplyr::mutate(
      date = lubridate::dmy(date),
      amount_chf = NA,
      amount_gbp = NA,
      amount_eur = NA,
      amount_usd = NA,
      bank = "DanskeBank"
    ) |>
    dplyr::select(date, description, amount_chf, amount_dkk, amount_eur, amount_usd,
           amount_gbp, bank)

  # Return dataframe
  return(df)
}
