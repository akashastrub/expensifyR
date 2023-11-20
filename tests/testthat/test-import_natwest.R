test_that("import_natwest function outputs GBP values", {

  # Import demo dataframe
  path <- testthat::test_path("fixtures", "natwest_gbp_test.csv")
  df_natwest_gbp_demo <- readr::read_csv(path)

  # Process demo dataframe
  df_natwest_cdm <- expensifyR::import_natwest(path)

  # Ensure all NULL columns are expected currencies
  null_cdm_cols <- names(which(sapply(df_natwest_cdm, anyNA)))
  expect_in(null_cdm_cols,
            c("amount_chf", "amount_dkk", "amount_eur", "amount_usd"))
})
