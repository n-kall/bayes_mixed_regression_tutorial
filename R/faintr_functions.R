##' Calculate draws for all cells of design matrix
##' @param model a model of class `brmsfit`
##' @return a [tibble][tibble::tibble-package]
##' @export
get_cell_draws <- function(model) {

  checkmate::assert_class(model, "brmsfit")

  design_matrix <- brms::standata(model)$X

  draws <- posterior::as_draws_df(as.data.frame(model))

  cell_draws <- c()

  for (cell in 1:NROW(design_matrix)) {
    cell_def <- design_matrix %>%
      tibble::as_tibble() %>%
      dplyr::slice(cell)

    cell_def_cols <- c()

    for (col in colnames(cell_def)) {
      if ((cell_def %>%
        dplyr::select(all_of(col)) %>%
        dplyr::pull()) == 1) {
        cell_def_cols <- c(cell_def_cols, col)
      }
    }

    cell_name <- stringr::str_c("cell", cell)

    new_cell_draw <- draws %>%
      dplyr::select(stringr::str_c("b_", cell_def_cols)) %>%
      summarise(!!cell_name := pmap_dbl(., sum))

    cell_draws <- cell_draws %>%
      dplyr::bind_cols(
        new_cell_draw
      )

  }


  return(cell_draws)
}

##' Filter draws to choose a specific group of design cells
##'
##' @param model a model of class `brmsfit`
##' @param ... specification of group
##' @return a [tibble][tibble::tibble-package]
##' @export
filter_draws <- function(model, ...) {
  checkmate::assert_class(model, "brmsfit")

  cell_definition <- dplyr::enquos(...)

  cells <- get_cell_definitions(model) %>%
    dplyr::filter(!!!cell_definition) %>%
    dplyr::select(rowname) %>%
    dplyr::pull()

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

##' Get all definitions of cells
##'
##' @param model
##' @return a [tibble][tibble::tibble-package]
##' @export
get_cell_definitions <- function(model) {
  checkmate::assert_class(model, "brmsfit")
  y <- as.character(brms::brmsterms(formula(model))$allvars[[2]])
  cell_defs <- dplyr::bind_cols(
    model$data,
    as.data.frame(standata(model)$X)
  ) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-matches(match = y))
  return(cell_defs)
}
