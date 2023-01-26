# nolint start
library(mlr3)
library(mlr3misc)
library(paradox)
library(R6)
library(data.table)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3fselect"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
# nolint end
