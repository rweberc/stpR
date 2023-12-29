#' Integrates logical statement asserts into the stpR object.
#'
#' Asserts that a statement is TRUE. If not, prints a warning and logs an alert.
#'
#' If `throw_assert_errors` in the project_dictionary is `TRUE`, then an error is thrown instead of a warning.
#' If `throw_assert_errors` is `FALSE` and `console_output_global` is `TRUE` in the project_dictionary, then a warning is printed to the console.
#'
#' @param logic Statement that evaluates to a TRUE/FALSE.
#' @param stp_id Id of the stp_ob entity. Defaults to "99999".
#' @param project_dictionary Project dictionary. Defaults to the global dictionary.
#' @param project_directory Project directory. Defaults to the global directory based on `here::here()` call.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' assert_cases(
#'   logic = qa_tbl %>%
#'     filter(trt == "Drug A" & stage == "T3") %>%
#'     nrow() == 0,
#'   stp_id = 'assert_trt_vs_stage'
#' )
#' }
assert_cases <- function(logic, # expecting statement that evaluates to a TRUE/FALSE
                         stp_id = "99999",
                         project_dictionary = get_project_dictionary(),
                         project_directory = here::here()
                         )
{

  eval_logic_assert_string = deparse(substitute(logic)) # TODO: there are probably a lot of cases here to look out for when trying to turn this into a character

  if (!logic) {

    error_print <- "{paste0(stringr::str_trim(eval_logic_assert_string), collapse = ' ') }  -  FAILED."%>% glue::glue()

    if (project_dictionary$save_metadata_global) {

      log_item(type = "ALERT",
               item = eval_logic_assert_string, # TODO: if statement is not a string but R code... will as.character() just give a "print" of it?
               add_item = NULL,
               priority = "1",
               stp_id = stp_id, # TODO: is it a concern if id's in different stp_ob entities are the same
               project_dictionary = project_dictionary,
               project_directory = project_directory)
    }

    if (project_dictionary$throw_assert_errors) {
      stop(error_print,
           call. = FALSE)
    } else if (project_dictionary$console_output_global) {
      warning(error_print,
              call. = FALSE)
    }

  }
}
