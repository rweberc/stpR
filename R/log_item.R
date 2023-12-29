#' Primary utility for logging items to text metadata items for the stpR package.
#'
#' Logs assert failures, comments, and todos for review.  Both save_metadata_global and include_metadata_log_items must be set to TRUE in the project_dictionary object for items to be logged.
#' Expected that during initial setup steps, may want to avoid logging all log_items values.
#' To avoid overhead of maintaining meaningful, individual ids for each log item, the default stp_id is set to "99999".  All instances will be logged (including duplicate items, etc.).
#' Function will be created to remove individual or all log items. For a final run of this documentation per iteratio of data set processed.
#'
#' @param type String value indicating type of item to log.  Either "ALERT", "TODO", or "COMMENT".
#' @param item String value of the item to log.
#' @param add_item Additional item to log.  This is used for the assert_distinct, assert_cases, assert_order output.
#' @param priority String value indicating priority of item to log.  TBD if this is still a useful field to include
#' @param stp_id Id of the stp_ob entity. Defaults to "99999".  Not as significant in usage as with other strR logging functions, such as eval_map(), etc.
#' @param project_dictionary The project_dictionary object, which by default is output by the `get_project_dictionary()` function.
#' @param project_directory The project_directory object, which by default is output by the `here::here()` function.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' log_item(
#'   type = "TODO",
#'   item = "Create function to reset all log_item metadata",
#'   priority = "1"
#'  )
#'
#' # Example 2 ----------------------------------
#' # Called by assert_cases.R function
#' log_item(type = "ALERT",
#'     item = eval_logic_assert_string,
#'     add_item = NULL,
#'     priority = "1",
#'     stp_id = stp_id,
#'     project_dictionary = project_dictionary,
#'     project_directory = project_directory
#'  )
#' }
log_item <- function(type,
                     item,
                     add_item = NULL,
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

