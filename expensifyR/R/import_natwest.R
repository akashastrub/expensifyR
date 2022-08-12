#' Title
#'
#' @param path
#' @param currency
#'
#' @return
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate rename
#'
#' @examples
import_natwest <- function(path, currency = "gbp") {

  df <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::rename(
      c(date = Date,
        description = Description,
        amount_gbp = Value)
      ) |>
    ## Filter NA rows due to Natwest export
    dplyr::filter(!is.na(amount_gbp)) |>
    dplyr::mutate(
      date = lubridate::dmy(date),
      bank = "NatWest",
      amount_chf = NA,
      amount_eur = NA,
      amount_dkk = NA,
      amount_usd = NA
      )  |>
    dplyr::select(
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
