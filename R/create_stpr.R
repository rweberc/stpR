#' Utility to create a new stp_ob object.
#'
#' Usually run at the beginning of processing a new data set.
#'
#' @param save_to_path If NULL, will attempt to save to the current_metadata_path in the project_dictionary.
#' @param reset If TRUE, will overwrite any existing stp_ob object at the save_to_path.
#' @param dir The directory to save the stp_ob object to, which by default is determined by the `here::here()` function.
#' @param project_dictionary The project_dictionary object, which by default is determined by the `get_project_dictionary()` function.
#'
#' @return stp_ob A list object that documents the current state of the project for evaluation and reporting.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' //create_stp_ob(reset = TRUE)
#'
#' }
#'
# TODO: take in path to attempt to read object
# TODO: add timestamps
create_stp_ob <- function(save_to_path = NULL,
                          reset = FALSE,
                          dir = here::here(),
                          project_dictionary = get_project_dictionary()) {

  mapping_items = tibble::tibble(
    stp_id = character(),
    df_name = character(),
    from = character(),
    to = character(),
    std_proc_na = list(), # TODO: should this be character or list of the actual functions themselves?
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
    df_name = character(),
    id_vars = character(),
    group_by_vars = character(),
    keep_logic = character(),
    stp_id = character(),
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

  # TODO: will add in replace case items here

  text_items = tibble::tibble(
    type = character(), # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object...
    stp_id = character(),
    item = character(), # could be "notes"
    add_item = list(), # could be "ref_ob"
    priority = character(), # NA for comments
    timestamp = as.POSIXct(character()) # todo check type
  )

  stp_ob = list(
    mapping_items = mapping_items,
    filter_items = filter_items,
    # case_items = case_items,
    text_items = text_items)

  # TODO: add in dir.exists check...

  # TODO: Incorporate project_dictionary globals:
  # allow_overwrite_artifacts_global
  # save_metadata_gobal

  # If save_to_path is NULL, try to get the write path from the current_metadata_path
  if (is.null(save_to_path)) {
    save_to_path = file.path(dir, project_dictionary$current_metadata_path) # TODO: add in data check...
  }

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


