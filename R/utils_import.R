# Named dispatch list — maps bank key → import function
.import_fns <- list(
  natwest          = import_natwest,
  revolut          = import_revolut,
  danske           = import_danske,
  santander        = import_santander,
  boa_debit        = import_boa_debit,
  boa_credit       = import_boa_credit,
  capitalone_debit = import_capitalone_debit,
  chase            = import_chase
)

# Parse bank + currency from filename (e.g. "boa_credit_usd_jan24.csv")
.parse_bank_currency <- function(filename) {
  parts <- stringr::str_split(filename, "_")[[1]]
  if (parts[1] %in% c("boa", "capitalone")) {
    list(bank = stringr::str_c(parts[1], parts[2], sep = "_"), currency = tolower(parts[3]))
  } else {
    list(bank = parts[1], currency = tolower(parts[2]))
  }
}

# Loop over a Shiny fileInput list, import each file, bind rows, convert currency
import_and_combine_bank_files <- function(files_df, master_currency) {
  df_out <- data.frame()
  for (i in seq_len(nrow(files_df))) {
    parsed   <- .parse_bank_currency(files_df$name[i])
    filepath <- stringr::str_replace_all(files_df$datapath[i], "\\\\", "/")
    df_addon <- .import_fns[[parsed$bank]](filepath, parsed$currency)
    if (nrow(df_addon) > 0 && parsed$currency != master_currency)
      df_addon <- convert_amount(df_addon, parsed$currency, master_currency)
    df_out <- dplyr::bind_rows(df_out, df_addon)
  }
  df_out
}
