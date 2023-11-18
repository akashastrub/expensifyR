# Create currency converter function to incorporate into imports            ####
# Requires internet connection to download currency conversions

#' Convert currency using priceR package (and API)
#'
#' @param df Dataframe with multiple currency columns, one of which must be
#' converted to the other.
#' @param currency_in Currency to be converted from.
#' @param currency_out Currency to be converted into.
#'
#' @return Converted dataframe.
#' @import priceR dplyr stringr
#' @export
#'
convert_amount <- function(df, currency_in, currency_out){

  # Download conversion rates for each relevant date
  df_ex <- priceR::historical_exchange_rates(
    toupper(currency_in),
    toupper(currency_out),
    min(df$date),
    max(df$date)
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date))

  # Convert currency-specific column name in df_ex to generic name
  colnames(df_ex) <- c("date", "exchange")

  # Convert currency-specific column name in df to generic name for input
  currency_in_var_name <- stringr::str_c("amount", tolower(currency_in), sep = "_")
  df["amount_original"] <- df[currency_in_var_name]

  # Convert currencies and deselect redundant columns
  df <- df %>%
    dplyr::inner_join(df_ex, by = "date"
    ) %>%
    dplyr::mutate(
      amount_original = round(amount_original * exchange, 2)
    ) %>%
    dplyr::select(-exchange)

  # Convert currency-specific column name in df to generic name for output
  currency_out_var_name <- stringr::str_c("amount", tolower(currency_out), sep = "_")
  df[currency_out_var_name] <- df["amount_original"]
  df <- df %>% dplyr::select(-amount_original)

  return(df)
}
