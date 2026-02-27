test_that("import_boa_debit function outputs USD values", {

  path <- testthat::test_path("fixtures", "boa_debit_usd_test.csv")
  df_cdm <- expensifyR::import_boa_debit(path, "usd")

  cdm_cols <- c("date", "description", "amount_chf", "amount_dkk",
                "amount_eur", "amount_usd", "amount_gbp", "bank")

  expect_true(nrow(df_cdm) > 0)
  expect_equal(names(df_cdm), cdm_cols)

  null_cdm_cols <- names(which(sapply(df_cdm, anyNA)))
  expect_in(null_cdm_cols,
            c("amount_chf", "amount_dkk", "amount_eur", "amount_gbp"))
})
