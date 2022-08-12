test_that("importing Revolut expenses works", {
  fle <- "demo_raw_revolut_eur.csv"
  pth <- file.path(system.file("data", package="expensifyR"), fle)
  df <- import_revolut(pth, "eur")
  expect_type(df, "list")
})
