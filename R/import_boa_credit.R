#' Process Bank of America credit card file. User must manually modify
#' statements to ensure correct date ranges
#'
#' @param path Path of raw Bank of America credit card file
#' @param currency Currency of Bank of America file (USD)
#'
#' @return Same data in common data format.
#' @import readr dplyr lubridate
#' @export
#'
import_boa_credit <- function(path, currency = "usd") {
  # Convert file to UTF-8 file format
  expensifyR::to_utf8(path)

  df <- readr::read_csv(path)  |>
    dplyr::rename(
      c(
        "date" = "Posted Date",
        "description" = "Payee",
        "amount_usd" = "Amount"
      )
    ) |>
    ## Filter NA rows due to BoA
    dplyr::filter(!is.na(amount_usd)) |>
    # Filter positive rows from payments
    dplyr::filter(!amount_usd >= 0) |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      bank = "BoA Credit",
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
