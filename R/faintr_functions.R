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
#' compare_cells(
#'   model = m,
#'   lower = c("gender = M", "context = inf"),
#'   higher = c("gender = F", "context = pol")
#' )
#' @export
compare_cells <- function(model, lower_group, higher_group) {
  lower_group <- dplyr::enquos(lower_group)
  higher_group <- dplyr::enquos(higher_group)

  # TODO: Allow for 'OR' operator when specifying levels

  lower_draws <- combined_draws(model, lower_group) %>%
    rename(lower = value)
  higher_draws <- filter_draws(model, higher_group) %>%
    rename(higher = value)
  out <- lower_draws %>%
    bind_cols(higher_draws) %>%
    mutate(comparison = higher - lower)

  colnames(out) <- c(
    "group_1",
    "group 2",
    "comparison"
  )

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

filter_draws <- function(model, ...) {
  cell_definition <- dplyr::enquos(...)

  cells <- get_cell_definitions(model) %>%
    filter(!!!cell_definition) %>%
    select(rowname) %>%
    pull()

  combined <- get_cell_draws(model)

  filtered_draws <- combined[as.numeric(cells)] %>%
    rowMeans() %>%
    tibble::as_tibble()

  # create a name for the group
  y <- stringr::str_c(as.character(cell_definition), collapse = ",") %>%
    stringr::str_remove_all("~") %>%
    stringr::str_remove_all('\"')

  colnames(filtered_draws) <- y

  return(filtered_draws)
}


get_cell_definitions <- function(model) {
  y <- as.character(brms::parse_bf(formula(model))$allvars[[2]])
  cell_defs <- bind_cols(
    m$data,
    as.data.frame(standata(m)$X)
  ) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-matches(match = y))
  return(cell_defs)
}
