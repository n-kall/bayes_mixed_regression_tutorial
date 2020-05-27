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
#'   the cell hypothesised to yield a higher dependent variable value
#' @param lower named list specifying levels of factors that specify
#'   the cell hypothesised to yield a lower dependent variable value
#' @return a tibble with posterior draws from each of the groups and
#'   the comparison
#' @examples
#' m <- brm(pitch ~ gender * context, politeness)
#' compare_cells(model = m,
#'               lower = c("gender = M", "context = inf"),
#'               higher = c("gender = F", "context = pol"))
#'
#' @export
compare_cells <- function(model, lower_group, higher_group, cmc = FALSE) {


  #TODO: Allow for 'OR' operator when specifying levels

  lower_draws <- combined_draws(model, lower_group, cmc) %>%
    rename(lower = value)
  higher_draws <- combined_draws(model, higher_group, cmc) %>%
    rename(higher = value)
  out <- lower_draws %>%
    bind_cols(higher_draws) %>%
    mutate(comparison = higher - lower)

   colnames(out) <- c(stringr::str_c(lower_group, collapse = ","),
                     stringr::str_c(higher_group, collapse = ","),
                     "comparison")

  return(out)
}

get_cell_draws <- function(model) {

  design_matrix <- brms::standata(model)$X

  draws <- posterior::as_draws_df(as.data.frame(model))

  cell_draws <- c()

  for (c in 1:NROW(design_matrix)) {
    cell_def <- design_matrix %>%
      as_tibble() %>%
      slice(c)

    cell_def_cols <- c()

    for (col in colnames(cell_def)) {
      if ((cell_def %>%
             select(all_of(col)) %>%
             pull()) == 1) {
        cell_def_cols <- c(cell_def_cols, col)
      }
    }

    cell_draws <- cell_draws %>%
      bind_cols(draws %>%
                  select(str_c("b_", cell_def_cols)) %>%
                  rowSums() %>%
                  as_tibble())

  }

  return(cell_draws)
}

select_cells <- function(model, definition, cmc) {
  vars <- c()
  ref_vars <- c()
  ref_cols <- c()


  if (cmc == FALSE) {

    # find reference levels (dummy coding)
    contrasts <- attr(brms::standata(model)$X, "contrasts")
    for (def in definition) {
      split_def <- stringr::str_split(def, " = ", simplify = T)
      factor <- split_def[[1]]
      level <- split_def[[2]]
      if (rownames(contrasts[[factor]])[1] == level) {
        ref_vars <- c(ref_vars, factor)
      } else {
        vars <- c(vars, stringr::str_c(factor, level))
      }
    }
  } else if (cmc == TRUE) {
    for (def in definition) {
      split_def <- stringr::str_split(def, " = ", simplify = T)
      factor <- split_def[[1]]
      level <- split_def[[2]]
      vars <- c(vars, stringr::str_c(factor, level))
      }
  }
  

  design_matrix <- brms::standata(model)$X %>%
                                       tibble::as_tibble() %>%
                                       tibble::rownames_to_column("cell")

  out <- design_matrix %>%
    tibble::as_tibble()

  if (length(vars) != 0) {
    out <- out %>%
      dplyr::filter_at(.vars = vars,
                .vars_predicate = ~ . == 1)
  }

  if (length(ref_vars) != 0) {

    ref_cols <- design_matrix %>%
      dplyr::select(starts_with(ref_vars)) %>%
      colnames()

    out <- out %>%
      dplyr::filter_at(.vars = ref_cols,
                       .vars_predicate = ~ . == 0)
    }
  return(unique(out))

}


combined_draws <- function(model, cell_definition, cmc) {
  cells <- select_cells(model, cell_definition, cmc)

  combined <- get_cell_draws(model)

  out <- combined[as.numeric(cells$cell)] %>%
    rowMeans() %>%
    as_tibble()

  return(out)
}
