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
  vars <- parse_bf(formula)[["allvars"]] %>%
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

##' Create a vector with factor levels for a cell
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
##' @return vector specifying levels of factors that define a cell
make_cell <- function(factor_values, factor_info, pars) {
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



  c("Intercept", factor_level_strings, interaction_strings)

  ## cell_str <- paste(c(
  ##   "Intercept",
  ##   factor_level_strings,
  ##   interaction_strings),
  ##   collapse = " + ")
}

#' Compare means of two subsets of factorial design cells
#'
#' This function takes a brms model fit for a factorial design and a
#' specification of two groups (subsets of design cells) to compare.
#' A group is specified as a named list, specifiying the factors and
#' their levels which to include in the group.  It outputs the
#' posterior samples of the 'higher' and 'lower' subsets of cells,
#' and the comparison 'higher - lower'.
#'
#' @param model a brmsfit
#' @param higher named list specifying levels of factors that specify
#'     the cell hypothesised to yield a higher dependent variable
#'     value
#' @param lower named list specifying levels of factors that specify
#'     the cell hypothesised to yield a lower dependent variable value
#' @param alpha level of probability
#' @return a tibble with posterior draws from each of the groups
#' @examples
#' m <- brm(pitch ~ gender * context, politeness)
#' compare_cells(model = m,
#'               lower = c(gender = "M", context = "inf"),
#'               higher = c(gender = "F", context = "pol"))
#'
#' @export
compare_cells <- function(model, lower_group, higher_group) {
  # create factor combination strings and run hypothesis

  factor_info <- get_factor_information(model)

  # needed work around to fix order of interactions (X:Y vs Y:X)
  pars <- colnames(as.data.frame(model))


  # TODO: loop over multiple cell definitions for each group
  lower_cell <- make_cell(lower_group, factor_info, pars)
  lower_group <- posterior::as_draws_df(as.data.frame(model)) %>%
    select(all_of(str_c("b_", lower_cell))) %>%
    rowSums() %>%
    as_tibble()
  colnames(lower_group) <- c("lower")

  higher_cell <- make_cell(higher_group, factor_info, pars)
  higher_group <- posterior::as_draws_df(as.data.frame(model)) %>%
    select(all_of(str_c("b_", higher_cell))) %>%
    rowSums() %>%
    as_tibble()
  colnames(higher_group) <- c("higher")

  comparison <- lower_group %>%
    bind_cols(higher_group) %>%
    mutate(comp = higher - lower)
}
