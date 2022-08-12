test_that("importing Danske expenses works", {
  fle <- "demo_raw_danske_dkk.csv"
  pth <- file.path(system.file("data", package="expensifyR"), fle)
  df <- import_danske(pth)
  expect_type(df, "list")
})
