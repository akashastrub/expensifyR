test_that("importing Natwest expenses works", {
  fle <- "demo_raw_natwest_gbp.csv"
  pth <- file.path(system.file("data", package="expensifyR"), fle)
  df <- import_natwest(pth)
  expect_type(df, "list")
})
