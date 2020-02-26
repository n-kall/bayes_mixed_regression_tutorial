#' Obtaining variable names from a brms model
#'
#' For a model for a factorial design, fitted with brms, this function returns the names of the independent variables.
#' For more information see \code{vignette('faintr_basics')}.
#' @param model Model fit from brms package.
#' @keywords regression, factorial design, brms
#' @import tidyverse brms
#' @export
#' @return list with names of the independent variables
#' @examples
#' library(brms)
#' m <- brm(yield ~ N * P * K, npk)
#' get_variables(m)
get_variables <- function(model) {
    # list all independent variables by parsing the brms formula

    # extract formula from fit
    formula <- model[["formula"]]

    # extract all variables from fit and remove tilde
    vars <- parse_bf(formula)[["allvars"]] %>%
        stringr::str_split(" ~ ", simplify = T)

    # split predictors and remove pluses
    predictors <- stringr::str_split(vars[[3, 1]], " \\+ ")[[1]] %>%
        str_split("\\:")
    predictors <- predictors[2:length(predictors)] %>%
        unlist() %>%
        unique()
    
    return(list(predicted = vars[[2, 1]],
                predictors = predictors))
}

#' Obtaining information about factors in regression model
#'
#' For a model for a factorial design, fitted with brms, this function returns information about the factors used, their levels, and the reference levels.
#' For more information see \code{vignette('faintr_basics')}.
#' @param model Model fit from brms package.
#' @keywords regression, factorial design, brms
#' @import tidyverse brms
#' @export
#' @return list with names of factors and their levels, including the reference levels (in dummy coding)
#' @examples
#' library(brms)
#' m <- brm(yield ~ N * P * K, npk)
#' get_factor_information(m)
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

##' Create string combining factor levels
##'
##' .. content for \details{} ..
##' @title 
##' @param factor_values named list specifying which levels of each factor to combine
##' @param factor_info list with names of factors and their levels, including the reference levels (in dummy coding)
##' @return string specifying levels of factors to be combined into a cell 
make_cell_string <- function(factor_values, factor_info) {
    # create a string for the combination of factor levels
    combination <- ""
    ref_count = 0    
    for (fct in names(factor_values)) {
        # check for reference level
        if (factor_info[[fct]]$reference == factor_values[[fct]]) {
            ref_count  <- ref_count + 1
            factor_str <- ""
        } else {
            factor_str <- paste0(fct, factor_values[[fct]])
        }
        
        if (length(factor_values) > 1) {
            combination <- paste(factor_str, combination, sep = ":")
        } else {
            combination <- paste0(factor_str, combination)
        }
    }
    # TODO: what about factors not specified? need to average over those levels rather than assuming reference level
    if (ref_count == length(factor_values)) {
        combination <- "Intercept"
    } else if (length(factor_values) > 1) {
       # remove the trailing colon
        combination  <- combination %>%
            stringr::str_remove(":*$") %>%
            stringr::str_remove("^:*")       
    }
    return(combination)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param model model of type brmsfit
##' @param higher named list specifying levels of factors that specify the cell to give a higher dependent variable value
##' @param lower named list specifying levels of factors that specify the cell to give a lower dependent variable value
##' @param alpha level of probability
##' @return 
compare_cells <- function(model, higher, lower, alpha = 0.05) {
    # create factor combination strings and run hypothesis

    factor_info <- get_factor_information(model)

    higher_str <- make_combination(higher, factor_info)
    
    lower_str <- make_combination(lower, factor_info)

    brms::hypothesis(model, paste(higher_str, ">", lower_str), alpha = alpha)
}
