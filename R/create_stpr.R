
#'
#' @return
#'
#' @export
#'
#'
# TODO: take in path to attempt to read object
# TODO: add timestamps
create_stpr <- function() {

  dictionary <- list(
    compare_filename = ""
  )

  error_items = tibble::tibble(
    id = character(),
    item = character()
  )

  update_items = tibble::tibble(
    id = character(),
    field_name = character(),
    ref_type = character(), # expect: missing, new, changed, no_comparison
    notes = character(),
    ref_ob = list()
  )

  reference_items = tibble::tibble(
    # field_type = character(), # expect: cat(egorical) or cont(inuous)
    # todo: consider saving out the std_process field?
    id = character(),
    df_name = character(),
    from = character(),
    to = character(),
    notes = character(),
    highlight = logical(),
    issue = logical(),
    no_report = logical(),
    ref_ob = list()
  )

  filtered_items = tibble::tibble(
    id = character(),
    df_name = character(),
    logic = character(),
    notes = character(),
    highlight = logical(),
    issue = logical(),
    no_report = logical(),
    ref_ob = list()
  )

  comment_items = tibble::tibble(
    id = character(),
    item = character()
  )

  todo_items = tibble::tibble(
    id = character(),
    priority = character(),
    item = character()
  )

  return(list(
    error_items,
    update_items,
    reference_items,
    filtered_items,
    comment_items,
    todo_items)
  )

}


