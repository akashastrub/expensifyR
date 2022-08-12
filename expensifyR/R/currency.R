#' Get rates using priceR
#'
#' Note: Requires internet connection to download currency conversions.
#'
#' @param currency_in
#' @param currency_out
#' @param date_start
#' @param date_end
#'
#' @return A data.frame with date and exchange columns.
#' @export
#'
#' @examples
#' get_rates("eur", "gbp", "2022-01-01", "2022-02-01")
get_rates <- function(currency_in, currency_out, date_start, date_end){
  ## Get historical rates
  df <- priceR::historical_exchange_rates(
    toupper(currency_in),
    toupper(currency_out),
    date_start,
    date_end
  )

  ## Convert currency-specific column name in df_ex to generic name
  colnames(df) <- c("date", "exchange")

  return(df)
}

#' Convert expenses using exchanges data
#'
#' @param df
#' @param df_exchanges
#'
#' @return A data.frame with new columns for converted amounts.
#' @export
#'
convert_amount <- function(df, df_exchanges){
  ## Convert currency-specific column name in df to generic name for input
  currency_in_var_name <- str_c("amount", tolower(currency_in), sep = "_")
  df["amount_original"] <- df[currency_in_var_name]

  ## Convert currencies and deselect redundant columns
  df <- df |>
    inner_join(df_exchanges, by = "date"
    ) |>
    mutate(
      amount_original = round(amount_original * exchange, 2)
    ) |>
    select(-exchange)

  # Convert currency-specific column name in df to generic name for output
  currency_out_var_name <- str_c("amount", tolower(currency_out), sep = "_")
  df[currency_out_var_name] <- df["amount_original"]
  df <- df |> select(-amount_original)

  return(df)
}
