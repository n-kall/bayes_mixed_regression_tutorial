##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param model a model of class `brmsfit`
##' @return a [tibble][tibble::tibble-package]
get_cell_draws <- function(model) {

  checkmate::assert_class(model, "brmsfit")
  
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
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param model a model of class `brmsfit`
##' @param ... specification of group
##' @return a [tibble][tibble::tibble-package]
filter_draws <- function(model, ...) {
  checkmate::assert_class(model, "brmsfit")
    
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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param model
##' @return a [tibble][tibble::tibble-package]
get_cell_definitions <- function(model) {
  checkmate::assert_class(model, "brmsfit")
  y <- as.character(brms::parse_bf(formula(model))$allvars[[2]])
  cell_defs <- bind_cols(
    m$data,
    as.data.frame(standata(m)$X)
  ) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-matches(match = y))
  return(cell_defs)
}
