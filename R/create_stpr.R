
#'
#' @return
#'
#' @export
#'
#'
# TODO: take in path to attempt to read object
# TODO: add timestamps
create_stp_ob <- function(save_to_path = NULL,
                          reset = FALSE) {

  mapping_items = tibble::tibble(
    # field_type = character(), # expect: cat(egorical) or cont(inuous)
    # todo: consider saving out the std_process field
    stp_id = character(),
    df_name = character(),
    from = character(),
    to = character(),
    std_proc_na = character(), # TODO: should this be character or list of the actual functions themselves?
    notes = character(),
    highlight = logical(),
    issue = logical(),
    report = logical(), # default to true
    perform_compare = logical(), # default to true
    compare_path = character(),
    ref_ob = list(),
    update_ob = list(),
    has_update_ob = logical(),
    timestamp = as.POSIXct(character()) # todo check type
  )

  # TODO: update with reference path that was used to create the compare and possibly the date this was added/updated
  # TODO: add the filter_logic, etc., to the output object itself... possibly do this also with the std_proc_na ref_ob...
  filter_items = tibble::tibble(
    stp_id = character(),
    df_name = character(),
    filter_logic = character(), # Likely need to add back in the group_by_vars, when saving out the filtered cases, so the logic respect it... could have it in the filter statement...
    notes = character(),
    highlight = logical(),
    issue = logical(),
    report = logical(), # default to true
    perform_compare = logical(), # default to true
    compare_path = character(),
    ref_ob = list(),
    update_ob = list(),
    has_update_ob = logical(),
    timestamp = as.POSIXct(character()) # todo check type
  )

  # case_items...

  text_items = tibble::tibble(
    type = character(), # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object...
    stp_id = character(),
    item = character(),
    add_item = list(),
    priority = character(), # NA for comments
    timestamp = as.POSIXct(character()) # todo check type
  )

  stp_ob = list(
    mapping_items = mapping_items,
    filter_items = filter_items,
    # case_items = case_items,
    text_items = text_items)

  # TODO: add in dir.exists check...

  # TODO: consider incorporating project_dictionary globals:
  # save_metadata_gobal
  # allow_overwrite_artifacts_global

  if (file.exists(save_to_path) & reset == FALSE)
    message("Files already exists and `reset` = FALSE; no object saved at: '{save_to_path}'")
  else if (file.exists(save_to_path) & reset == TRUE) {
    if (usethis::ui_yeah("stp_ob exists at path: '{save_to_path}'.  Confirm overwrite with an empty stp_ob:")) {
      saveRDS(stp_ob, save_to_path)
      usethis::ui_done("New stp_ob created at: '{save_to_path}'")
    }
  } else {
    saveRDS(stp_ob, save_to_path)
    usethis::ui_done("New stp_ob created at: '{save_to_path}'")
  }



  return(invisible(stp_ob))

}


