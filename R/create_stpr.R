
#'
#' @return
#'
#' @export
#'
#'
# TODO: take in path to attempt to read object
# TODO: add timestamps
create_stp_ob <- function() {

  mapping_items = tibble::tibble(
    # field_type = character(), # expect: cat(egorical) or cont(inuous)
    # todo: consider saving out the std_process field
    stp_id = character(),
    df_name = character(),
    from = character(),
    to = character(),
    std_proc_na = character(), # TODO: should this be character or list of the actual functions themselves?
    mapping_notes = character(),
    update_notes = character(),
    highlight = logical(),
    issue = logical(),
    compare_path = character(),
    report = logical(), # default to true
    map_ob = list(),
    update_ob = list(),
    has_update_ob = logical(),
    timestamp = POSIXct() # todo check type
  )

  # TODO: update with reference path that was used to create the compare and possibly the date this was added/updated
  filter_items = tibble::tibble(
    stp_id = character(),
    filter_logic = character(), #
    filter_notes = character(),
    update_notes = character(),
    highlight = logical(),
    issue = logical(),
    perform_compare = logical(), # default to true
    compare_path = character(),
    report = logical(), # default to true
    filter_ob = list(),
    update_ob = list(),
    timestamp = POSIXct() # todo check type
  )

  # case_items...

  text_items = tibble::tibble(
    type = character(), # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object...
    stp_id = character(),
    item = character(),
    add_item = list(),
    priority = character(), # NA for comments
    timestamp = POSIXct() # todo check type
  )

  return(list(
    mapping_items = mapping_items,
    filter_items = filter_items,
    # case_items = case_items,
    text_items = text_items)
  )

}


