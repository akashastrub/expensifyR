#' Process Danske Bank file
#'
#' @param path Path of raw Danske Bank file.
#' @param currency Currency of Danske Bank file (DKK).
#'
#' @return Same data in common data format.
#' @import stringr dplyr readr
#' @export
#'
import_danske <- function(path, currency = "dkk") {

  # Convert file to UTF-8 file format
  expensifyR::to_utf8(path)

  # Import data and rename
  df <- readr::read_csv(path) %>%
    dplyr::rename(
      c('date' = 'Dato',
        'description' = 'Tekst'))

  # Rename indexing due to Danish text
  colnames(df)[5] <- 'amount_dkk'

  # Mutate amounts, add columns, rearrange columns
  df <- df %>%
    dplyr::mutate(amount_dkk = stringr::str_replace(amount_dkk, '\\.', '')) %>%
    dplyr::mutate(amount_dkk = stringr::str_replace(amount_dkk, ',', '.')) %>%
    dplyr::mutate(amount_dkk = as.numeric(amount_dkk)) %>%
    dplyr::mutate(
      date = dmy(date),
      amount_chf = NA,
      amount_gbp = NA,
      amount_eur = NA,
      amount_usd = NA,
      bank = "DanskeBank"
    ) %>%
    dplyr::select(date, description, amount_chf, amount_dkk, amount_eur, amount_usd,
           amount_gbp, bank)

  # Return dataframe
  return(df)
}
