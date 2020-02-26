factor_info <- list(factor1 = list(levels = c("A", "B", "C"), reference = "A"),
                    factor2 = list(levels = c("X", "Y", "Z"), reference = "X"),
                    factor3 = list(levels = c("J", "K", "L"), reference = "J"))

test_that("reference level is detected", {
    expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "X", factor3 = "J"), factor_info = factor_info), "Intercept")
    expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "Y"), factor_info = factor_info), "factor2Y")
    expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "C", factor2 = "X"), factor_info = factor_info), "factor1C")
                 })

test_that("combinations detected", {
    expect_equal(faintr::make_cell_string(factor_values = c(factor1 = "A", factor2 = "
