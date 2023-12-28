


#'
#' @export
#'
assert_cases <- function(logic, # expecting statement that evaluates to a TRUE/FALSE
                         stp_id = 99999,
                         project_dictionary = get_project_dictionary(),
                         project_directory = here::here()
                         )
{

  eval_logic_assert_string = deparse(substitute(logic)) # TODO: there are probably a lot of cases here to look out for when trying to turn this into a character

  if (!logic) {

    error_print <- "{paste0(stringr::str_trim(eval_logic_assert_string), collapse = ' ') }  FAILED."%>% glue::glue()

    if (project_dictionary$save_metadata_global) {

      log_item(type = "ALERT",
               item = eval_logic_assert_string, # TODO: if statement is not a string but R code... will as.character() just give a "print" of it?
               add_item = NULL,
               priority = "1",
               stp_id = stp_id, # TODO: is it a concern if id's in different stp_ob entities are the same
               project_dictionary = project_dictionary,
               project_directory = project_directory)
    }

    if (project_dictionary$console_output_global) {
      warning(error_print)
    }

    # TODO: isn't there an error call?
    # TODO: error call occurs regardless of whether console_output_global is true or not
    error(error_print)
  }
}
