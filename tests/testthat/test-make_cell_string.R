factor_info <- list(
  factor1 = list(levels = c("A", "B", "C"), reference = "A"),
  factor2 = list(levels = c("X", "Y", "Z"), reference = "X"),
  factor3 = list(levels = c("J", "K", "L"), reference = "J")
)

test_that("reference levels correct", {
  expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "X", factor3 = "J"), factor_info = factor_info), "Intercept")
  expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "B", factor2 = "X", factor3 = "J"), factor_info = factor_info), "Intercept + factor1B")
  expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "Y", factor3 = "J"), factor_info = factor_info), "Intercept + factor2Y")
  expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "X", factor3 = "K"), factor_info = factor_info), "Intercept + factor3K")
})

test_that("interaction terms correct", {
  expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "Y", factor3 = "K"), factor_info = factor_info), "Intercept + factor2Y + factor3K + factor2Y:factor3K")
  expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "B", factor2 = "Y", factor3 = "K"), factor_info = factor_info), "Intercept + factor1B + factor2Y + factor3K + factor1B:factor2Y + factor1B:factor3K + factor2Y:factor3K + factor1B:factor2Y:factor3K")
})
