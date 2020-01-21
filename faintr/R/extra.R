library(brms)
library(tidyverse)

all_terms <- function(x) {
  if (!length(x)) {
    return(character(0))
  }
  if (!inherits(x, "terms")) {
    x <- terms(as.formula(x))
  }
  rm_wsp(attr(x, "term.labels"))
}

get_effect.brmsterms <- function(x, target = "fe", all = TRUE, ...) {
  if (all) {
    out <- named_list(c(names(x$dpars), names(x$nlpars)))
    for (dp in names(x$dpars)) {
      out[[dp]] <- get_effect(x$dpars[[dp]], target = target)
    }
    for (nlp in names(x$nlpars)) {
      out[[nlp]] <- get_effect(x$nlpars[[nlp]], target = target)
    }
  } else {
    out <- get_effect(x$dpars[["mu"]], target = target)
  }
  unlist(out, recursive = FALSE)
}

# extract different types of effects
get_effect <- function(x, ...) {
  UseMethod("get_effect")
}


# extract names of variables treated as integers
get_int_vars <- function(x, ...) {
  UseMethod("get_int_vars")
}

#' @export
get_int_vars.brmsterms <- function(x, ...) {
  advars <- ulapply(rmNULL(x$adforms[c("trials", "cat")]), all_vars)
  unique(c(advars, get_sp_vars(x, "mo")))
}


# unlist lapply output
ulapply <- function(X, FUN, ..., recursive = TRUE, use.names = TRUE) {
  unlist(lapply(X, FUN, ...), recursive, use.names)
}


# check if an object is NULL
isNULL <- function(x) {
  is.null(x) || ifelse(is.vector(x), all(sapply(x, is.null)), FALSE)
}

# recursively removes NULL entries from an object
rmNULL <- function(x, recursive = TRUE) {
  x <- Filter(Negate(isNULL), x)
  if (recursive) {
    x <- lapply(x, function(x) if (is.list(x)) rmNULL(x) else x)
  }
  x
}


# find variables in a character string or expression
all_vars <- function(expr, ...) {
  if (is.character(expr)) {
    expr <- parse(text = expr)
  }
  all.vars(expr, ...)
}

# find names of all variables used in a special effects type
get_sp_vars <- function(x, type) {
  sp_terms <- ulapply(get_effect(x, "sp"), all_terms)
  all_vars(str2formula(get_matches_expr(regex_sp(type), sp_terms)))
}

# initialize a named list
# @param names names of the elements
# @param values optional values of the elements
named_list <- function(names, values = NULL) {
  if (!is.null(values)) {
    if (length(values) <= 1L) {
      values <- replicate(length(names), values)
    }
    values <- as.list(values)
    stopifnot(length(values) == length(names))
  } else {
    values <- vector("list", length(names))
  }
  setNames(values, names)
}

#' @export
get_effect.btl <- function(x, target = "fe", ...) {
  x[[target]]
}



# convert a string to a formula
# @param x vector of strings to be converted
# @param ... passed to formula()
str2formula <- function(x, ..., collapse = "+") {
  has_chars <- nzchar(x)
  if (length(x) && any(has_chars)) {
    out <- paste(x[has_chars], collapse = collapse) 
  } else {
    out <- "1"
  }
  out <- formula(paste("~", out), ...)
  environment(out) <- parent.frame()
  out
}

# find matches in the parse tree of an expression
# @param pattern pattern to be matched
# @param expr expression to be searched in
# @return character vector containing matches
get_matches_expr <- function(pattern, expr, ...) {
  if (is.character(expr)) {
    expr <- parse(text = expr)
  }
  out <- NULL
  for (i in seq_along(expr)) {
    sexpr <- try(expr[[i]], silent = TRUE)
    if (!is(sexpr, "try-error")) {
      sexpr_char <- deparse_combine(sexpr)
      out <- c(out, get_matches(pattern, sexpr_char, ...))
    }
    if (is.call(sexpr) || is.expression(sexpr)) {
      out <- c(out, get_matches_expr(pattern, sexpr, ...))
    }
  }
  unique(out)
}



form <- brms::bf(depvar ~ invar1 * invar2, family = "gaussian")

parsed <- brms::parse_bf(form)






 dependent_variable <- as.character(form[[1]])[[2]]
independent_variables  <- strsplit(x = gsub(pattern = "\\(.*\\|.*\\)",
                                            "",
                                            as.character(form[[1]])[[3]]),
                                   split =  "(\\*|\\+)",
                                   fixed = FALSE)[[1]] %>% trimws() %>% unique()
  independent_variables = independent_variables[which(independent_variables != "")]



iris_tibble  <- iris %>%
    mutate(Species = factor(Species),
           Sepal.Length = ordered(Sepal.Length))

test_model <- brm(Sepal.Width ~ Species + mo(Sepal.Length), data = iris_tibble)


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

get_factor_predictors <- function(model) {
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
    factor_levels <- list()
    for (fac in factor_predictors) {
        factor_levels[[fac]] <- levels(data[[fac]])
    }

    return(factor_levels)
}
