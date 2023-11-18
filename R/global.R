# # Personal finances app
#
# # Setup                                                                     ####
# # Load required libraries
# library(dplyr)
# library(readxl)
# library(lubridate)
# library(stringr)
# library(shinydashboard)
# library(shiny)
# library(reactable)
# library(rhandsontable)
# library(plotly)
# library(readr)
# library(expensifyR)
#
#
# # Load required functions
# import_src_functions <- list.files(
#   here::here("src"),
#   # ignore potential non .R files, for example, .Rmd
#   pattern = "\\.R$",
#   full.names = TRUE,
#   recursive = TRUE)
#
# lapply(import_src_functions, source)
#
# # Clean-up
# rm(import_src_functions)
