#' Integrates asserts of uniqueness by key variables into the stpR object.
#'
#' Asserts that rows in the data set are unique by one or more grouping variables. If not, prints a warning and logs an alert.
#'
#' If `throw_assert_errors` in the project_dictionary is `TRUE`, then an error is thrown instead of a warning.
#' If `throw_assert_errors` is `FALSE` and `console_output_global` is `TRUE` in the project_dictionary, then a warning is printed to the console.
#'
#' @param data_df A data set to be evaluated for uniqueness.
#' @param group_by_vars A string or vector of strings representing fields within the data set to group by.
#' @param stp_id A character ID of the stp_ob entity. Defaults to "99999".
#' @param project_dictionary Project dictionary. Defaults to the global dictionary.
#' @param project_directory Project directory. Defaults to the global directory based on `here::here()` call.
#'
#' @export
#'
#' @examples
#' \donttest{
#' temp_dup_tbl <- test_tbl %>%
#'   bind_rows(test_tbl %>%
#'             slice(1))
#'
#' assert_distinct(
#'   data_df = temp_dup_tbl,
#'   group_by_vars = "col_id"
#' )
#'
#' }

assert_distinct <- function(data_df,
                            group_by_vars = NULL,
                            stp_id = "99999",
                            project_dictionary = get_project_dictionary(),
                            project_directory = here::here()
                            )
{

  # check first for any duplicated
  dupDf <- data_df[duplicated(data_df),]

  if (nrow(dupDf) > 0)
    dupDf$errorType <- "Duplicate Rows"

  # check for multiple by group id
  multDf <- data_df %>%
    dplyr::ungroup() %>%
    unique() %>%
    dplyr::group_by_at(c(group_by_vars)) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange_at(group_by_vars) %>%
    dplyr::mutate(errorType = "Multiple Rows Per Identifier")

  unionDf <- dplyr::bind_rows(dupDf, multDf) %>%
    dplyr::arrange(errorType)

  if (nrow(unionDf) > 0) {

    error_print <- paste0("Assert distinct failed.")

    if (project_dictionary$save_metadata_global) {

      log_item(type = "ALERT",
               item = error_print,
               add_item = unionDf, # for now, this is to encompass the assert_distinct, assert_cases scenarios... but right now only do the duplicate checks by "item"... some other work needs to be done here
               priority = "1",
               stp_id = stp_id, # TODO: is it a concern if id's in different stp_ob entities are the same
               project_dictionary = project_dictionary,
               project_directory = project_directory)
    }

    if (project_dictionary$console_output_global) {
      output_data_to_console(unionDf, header_text = "Cases failing distinct:")
    }

    if (project_dictionary$throw_assert_errors) {
      stop(error_print,
           call. = FALSE)
    } else if (project_dictionary$console_output_global) {
      warning(error_print,
              call. = FALSE)
    }

  }

  return(invisible(unionDf))

}
