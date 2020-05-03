#' Obtaining variable names from a brms model
#'
#' For a model for a factorial design, fitted with brms, this function
#' returns the names of the independent variables.  For more
#' information see \code{vignette('faintr_basics')}.
#' @param model Model fit from brms package.
#' @keywords regression, factorial design, brms
#' @import brms stringr
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
  vars <- brmsterms(formula)[["allvars"]] %>%
    stringr::str_split(" ~ ", simplify = T)

  # split predictors and remove pluses and interaction terms
  predictors <- stringr::str_split(vars[[3, 1]], " \\+ ")[[1]]


  # TODO: extracting interaction terms here to check which are actually
  # encoded in the model

  interactions <- Filter(function(x) grepl(":", x), predictors)

  predictors <- predictors[2:length(predictors)] %>%
    stringr::str_split("\\:") %>%
    unlist() %>%
    unique()

  return(
    list(
      predicted = vars[[2, 1]],
      predictors = predictors,
      interactions = interactions
    )
  )
}

#' Obtaining information about factors in regression model
#'
#' For a model for a factorial design, fitted with brms, this function
#' returns information about the factors used, their levels, and the
#' reference levels.  For more information see
#' \code{vignette('faintr_basics')}.
#' @param model Model fit from brms package.
#' @keywords regression, factorial design, brms
#' @import dplyr
#' @return list with names of factors and their levels, including the
#'   reference levels (in dummy coding)
#' @examples
#' library(brms)
#' m <- brm(pitch ~ gender * context, politeness)
#' get_factor_information(m)
get_factor_information <- function(model) {
  # return independent variables that are factors and their levels

  variables <- get_variables(model)

  predictors <- variables[["predictors"]]

  # data that the model was fit on
  d <- stats::model.frame(model)

  # get vector of names of factor predictors
  factor_predictors <- d %>%
    dplyr::select(predictors) %>%
    dplyr::select_if(is.factor) %>%
    names()

  # output levels for each of the predictor factors
  factor_info <- list()
  for (fac in factor_predictors) {
    lvls <- levels(d[[fac]])
    ref_lvl <- lvls[1]
    factor_levels <- list(
      levels = lvls,
      reference = ref_lvl
    )

    factor_info[[fac]] <- factor_levels
  }

  return(factor_info)
}

##' Create string combining factor levels
##'
##' Given a specification of factor levels, this function creates as
##' string corresponding the formula for that cell in the design
##' matrix.
##' @title
##' @import dplyr
##' @param factor_values named list specifying which levels of each
##'   factor to combine
##' @param factor_info list with names of factors and their levels,
##'   including the reference levels (in dummy coding)
##' @return string specifying levels of factors to be combined into a
##'   cell
make_cell_string <- function(factor_values, factor_info, pars) {
  # create a string for the combination of factor levels
  factor_level_strings <- c()

  for (fct in names(factor_values)) {
    # check for reference level
    if (factor_info[[fct]]$reference != factor_values[[fct]]) {
      factor_level_strings <- c(
        factor_level_strings,
        paste0(fct, factor_values[[fct]])
      )
    }
  }


  # add interaction terms
  interactions <- c()
  interaction_strings <- c()
  if (length(factor_level_strings) > 1) {
    for (i in 2:length(factor_level_strings)) {
      interactions <- c(interactions,
                        combn(factor_level_strings, m = i, simplify = F))
    }
    for (i in 1:length(interactions)) {
      new_int_str <- stringr::str_c(interactions[[i]], collapse = ":")

      # check if interaction is actually in model
      if (!(str_c("b_", new_int_str) %in% pars)) {
        # try reversing interaction
        # TODO: try different permutations for 3-way interactions
        new_int_str <- stringr::str_c(rev(interactions[[i]]), collapse = ":")

        # check if reversed is actually in model
        if (!(str_c("b_", new_int_str) %in% pars)) {
          new_int_str <- ""
        }
      }
      interaction_strings <- c(
        interaction_strings,
        new_int_str
      )
    }
  }

  interaction_strings <- interaction_strings %>%
    stringi::stri_remove_empty()
  
  cell_str <- paste(c(
    "Intercept",
    factor_level_strings,
    interaction_strings),
    collapse = " + ")
}

#' Compare means of two subsets of factorial design cells
#'
#' This function takes a brms model fit for a factorial design and a
#' specification of two groups (subsets of design cells) to compare.
#' A group is specified as a named list, specifiying the factors and
#' their levels which to include in the group.  It outputs the
#' posterior mean of the 'higher' minus the 'lower' subset of cells,
#' its 95 percent credible interval and the posterior probability that
#' the 'higher' group has a higher mean than the the 'lower' group.
#'
#' @param model a brmsfit
#' @param higher named list specifying levels of factors that specify
#'     the cell hypothesised to yield a higher dependent variable
#'     value
#' @param lower named list specifying levels of factors that specify
#'     the cell hypothesised to yield a lower dependent variable value
#' @param alpha level of probability
#' @return a brmshypothesis object corresponding to the hypothesis of
#'     "higher > lower"
#' @examples
#' m <- brm(pitch ~ gender * context, politeness)
#' compare_cells(model = m,
#'               lower = c(gender = "M", context = "inf"),
#'               higher = c(gender = "F", context = "pol"),
#'               alpha = 0.1)
#'
#' compare_cells(model = m,
#'               lower = c(gender = "M"),
#'               higher = c(gender = "F"),
#'               by = "context",
#'               alpha = 0.05)
#' @export
compare_cells <- function(model, higher, lower, by = NA, alpha = 0.05) {
  # create factor combination strings and run hypothesis

  factor_info <- get_factor_information(model)

  # needed work around to fix order of interactions (X:Y vs Y:X)
  pars <- colnames(as.data.frame(model))

  # with by factor
  if (!is.na(by)) {
    hyps <- c()
    for (lvl in factor_info[[by]]$levels) {
      # add the current level of the by-factor
      sub_lower <- c(lower, lvl)
      names(sub_lower) <- c(names(lower), by)
      lower_str <- make_cell_string(sub_lower, factor_info, pars)

      # add the current level of the by-factor
      sub_higher <- c(higher, lvl)
      names(sub_higher) <- c(names(higher), by)
      higher_str <- make_cell_string(sub_higher, factor_info, pars)

      hyps <- c(hyps, paste(lower_str, "<", higher_str))
    }
    print(paste("Hypothesis to test:", hyps))
    brms::hypothesis(x = model, hypothesis = hyps, alpha = alpha)

    # no by factor
  } else {
    lower_str <- make_cell_string(lower, factor_info, pars)
    higher_str <- make_cell_string(higher, factor_info, pars)
    hyp <- paste(lower_str, "<", higher_str)
    print(paste("Hypothesis to test:", hyp))
    brms::hypothesis(x = model, hypothesis = hyp, alpha = alpha)
  }
}