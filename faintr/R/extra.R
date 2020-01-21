library(brms)
library(tidyverse)

iris_tibble  <- iris %>%
    mutate(Species = factor(Species),
           Sepal.Length = ordered(Sepal.Length))

test_model <- brm(Sepal.Width ~ Species + Sepal.Length, data = iris_tibble)


get_variables <- function(model) {
    # list all independent variables by parsing the brms formula

    # extract formula from fit
    formula <- model[["formula"]]

    # extract all variables from fit and remove tilde
    vars <- parse_bf(formula)[["allvars"]] %>%
        stringr::str_split(" ~ ", simplify = T)

    # split predictors and remove pluses
    predictors <- stringr::str_split(vars[[3, 1]], " \\+ ")[[1]]
    predictors <- predictors[2:length(predictors)]
    
    return(list(predicted = vars[[2, 1]],
                predictors = predictors))
}

get_factor_information <- function(model) {
    # return independent variables that are factors and their levels

    variables <- get_variables(model)

    predictors <- variables[["predictors"]]

    # data that the model was fit on
    data <- stats::model.frame(model)

    # get vector of names of factor predictors
    factor_predictors <- data %>%
        select(predictors) %>%
        select_if(is.factor) %>%
        names()

    # output levels for each of the predictor factors
    factor_info <- list()
    for (fac in factor_predictors) {
        lvls <- levels(data[[fac]])
        ref_lvl <- lvls[1]
        factor_levels <- list(levels = lvls,
                              reference = ref_lvl)
        
        factor_info[[fac]] <- factor_levels
    }

    return(factor_info)
}

compare_cells <- function(model, higher, lower, alpha = 0.05) {

    factor_info <- get_factor_information(model)

    higher_str <- ""

    higher_intercept = FALSE
    
    for (fct in names(higher)) {
                                        # check for reference level
        
        if (factor_info[[fct]]$reference == higher[[fct]]) {
            higher_intercept = TRUE
            factor_str <- ""
        } else {
            higher_intercept = FALSE
            factor_str <- paste0(fct, higher[[fct]])
            }

        if (higher_intercept == TRUE) {
            higher_str <- "Intercept"
        } else {
            
            if (length(higher) > 1) {
                higher_str <- paste(higher_str, factor_str, sep = ":")
            } else {
                higher_str <- paste0(higher_str, factor_str)
            }       
        }
    }

    lower_str <- ""
    for (fct in names(higher)) {
        factor_str <- paste0(fct, lower[[fct]])
        if (length(lower) > 1) {
            lower_str <- paste(lower_str, factor_str, sep = ":")
        } else {
            lower_str <- paste0(lower_str, factor_str)
        }
    }


    print(paste(higher_str, ">", lower_str))
    brms::hypothesis(model, paste(higher_str, ">", lower_str), alpha = alpha)
}
