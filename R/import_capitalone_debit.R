#' Process Capital One debit card file. User must manually modify
#' statements to ensure correct date ranges
#'
#' @param path Path of raw Capital One file
#' @param currency Currency of Capital One file (USD)
#'
#' @return Same data in common data format.
#' @import readr dplyr lubridate
#' @export
#'
import_capitalone_debit <- function(path, currency = "usd") {
  # Convert file to UTF-8 file format
  expensifyR::to_utf8(path)

  df <- readr::read_csv(path)  |>
    dplyr::rename(
      c(
        "date" = "Transaction Date",
        "description" = "Transaction Description",
        "amount_usd" = "Transaction Amount",
        "transaction_type" = "Transaction Type"
      )
    ) |>
    # Create +/- based on `Transaction Type`
    dplyr::mutate(
      amount_usd = dplyr::case_when(
        stringr::str_detect(transaction_type, "Debit") ~ -amount_usd,
        stringr::str_detect(transaction_type, "Credit") ~ amount_usd,
        TRUE ~ 9999)) |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      bank = "CapitalOne Debit",
      amount_chf = NA,
      amount_eur = NA,
      amount_dkk = NA,
      amount_gbp = NA
    )  |>
    dplyr::select(date,
                  description,
                  amount_chf,
                  amount_dkk,
                  amount_eur,
                  amount_usd,
                  amount_gbp,
                  bank)

  return(df)
}
