#' Process Bank of America debit card file
#'
#' @param path Path of raw Bank of America debit card file
#' @param currency Currency of Bank of America file (USD)
#'
#' @return Same data in common data format.
#' @import readr dplyr lubridate
#' @export
#'
import_boa_debit <- function(path, currency = "usd") {
  # Convert file to UTF-8 file format
  expensifyR::to_utf8(path)

  df <- readr::read_csv(path,
                        # skip first 6 rows of BoA summary
                        skip = 6)  |>
    dplyr::rename(
      c(
        "date" = "Date",
        "description" = "Description",
        "amount_usd" = "Amount",
        "balance" = "Running Bal."
      )
    ) |>
    ## Filter NA rows due to BoA
    dplyr::filter(!is.na(amount_usd)) |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      bank = "BoA Debit",
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
