##' Calculate draws for all cells of design matrix
##' @param model a model of class `brmsfit`
##' @return a [tibble][tibble::tibble-package]
##' @export
get_cell_draws <- function(model) {

  checkmate::assert_class(model, "brmsfit")

  # extract design matrix
  design_matrix <- brms::standata(model)$X

  # extract posterior draws
  draws <- posterior::as_draws_df(as.data.frame(model))

  cell_draws <- c()

  # loop over each row of design matrix
  for (cell in 1:NROW(design_matrix)) {
    cell_def <- design_matrix %>%
      tibble::as_tibble() %>%
      dplyr::slice(cell)

    cell_def_cols <- c()

    # get columns that specify cell
    for (col in colnames(cell_def)) {
      if ((cell_def %>%
        dplyr::select(all_of(col)) %>%
        dplyr::pull()) == 1) {
        cell_def_cols <- c(cell_def_cols, col)
      }
    }

    # create cell number
    cell_name <- stringr::str_c("cell", cell)

    # select columns in cell specification and sum the rows
    new_cell_draw <- draws %>%
      dplyr::select(stringr::str_c("b_", cell_def_cols)) %>%
      dplyr::summarise(!!cell_name := purrr::pmap_dbl(., sum))

    # bind all new cell draws
    cell_draws <- cell_draws %>%
      dplyr::bind_cols(
        new_cell_draw
      )

  }
  return(cell_draws)
}

##' Extract draws for a specified group of cells
##'
##' @param cell_draws 
##' @param cell_definitions 
##' @param ... Specification of group
##' @return tibble
##' @export

extract_draws <- function(cell_definitions, cell_draws, ...) {


  group_spec <- dplyr::enquos(...)

  
  # get cell numbers based on specification
  cell_numbers <- cell_definitions %>%
    dplyr::filter(!!!group_spec) %>%
    dplyr::select(cell) %>%
    dplyr::pull()

  # filter the draws by choosing the appropriate columns based on cell numbers
  filtered_draws <- cell_draws[as.numeric(cell_numbers)] %>%
    rowMeans() %>%
    tibble::as_tibble()

  # create a name for the group
  y <- stringr::str_c(as.character(group_spec), collapse = ", ") %>%
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

  # extract dependent variable
  y <- as.character(brms::brmsterms(formula(model))$allvars[[2]])

  # concatenate design matrix and actual data
  cell_defs <- dplyr::bind_cols(
    model$data,
    as.data.frame(brms::standata(model)$X)
  ) %>%
    # select the columns except for dependent variable
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::select(-matches(match = y))
  return(tibble::as_tibble(cell_defs))
}
