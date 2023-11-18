#' Process Natwest file
#'
#' @param path Path of raw Natwest file
#' @param currency Currency of Natwest file (GBP).
#'
#' @return Same data in common data format.
#' @import readr dplyr lubridate
#' @export
#'
import_natwest <- function(path, currency = "gbp") {

  # Convert file to UTF-8 file format
  to_utf8(path)

  df <- readr::read_csv(path) %>%
    dplyr::rename(
      c(date = Date,
        description = Description,
        amount_gbp = Value)
      ) %>%
    ## Filter NA rows due to Natwest export
    dplyr::filter(!is.na(amount_gbp)) %>%
    dplyr::mutate(
      date = lubridate::dmy(date),
      bank = "NatWest",
      amount_chf = NA,
      amount_eur = NA,
      amount_dkk = NA,
      amount_usd = NA
      )  %>%
    dplyr::select(
      date,
      description,
      amount_chf,
      amount_dkk,
      amount_eur,
      amount_usd,
      amount_gbp,
      dplyr::  bank
    )

  return(df)
}
