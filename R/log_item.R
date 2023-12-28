

#'
#' @return
#'
#' @export
#'
log_item <- function(type,
                     item,
                     add_item = NULL, # This is to encompass the assert_distinct, assert_cases, assert_order output
                     priority = NULL,
                     stp_id = "99999",
                     project_dictionary = get_project_dictionary(),
                     project_directory = here::here()
                     )
{
  type = match.arg(type, c("ALERT", "TODO", "COMMENT"))

  # TODO: add in error-checking on item + add_item

  # Try to read current mapping
  stp_ob = get_stp_object(project_dictionary,
                          dir = project_directory,
                          file_type = "current")


  # Right now, will have global parameter for logging.  If TRUE, all instances will be logged (including duplicate items, etc.)
  if (project_dictionary$save_metadata_global == TRUE &
      project_dictionary$include_metadata_log_items == TRUE) {

    stp_ob$text_items = stp_ob$text_items %>%
      dplyr::add_row(
        type = type,
        stp_id = stp_id,
        item = item,
        add_item = ifelse(is.null(add_item), list(), add_item),
        priority = ifelse(is.null(priority), character(), priority),
        timestamp = lubridate::now()
      )

    # Path verified in get_stp_object step
    saveRDS(stp_ob, file.path(project_directory, project_dictionary$current_metadata_path))

  }

}

