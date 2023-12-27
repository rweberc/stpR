


#'
#' @export
#'
assert_cases <- function(logic, # expecting statement that evaluates to a TRUE/FALSE
                         stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                         project_dictionary = get_project_dictionary(),
                         project_directory = here::here()
                         )
{
  # TODO: input checks...
  # TODO: does this really need a stp_id?

  # eval_logic_assert = eval(parse(text = logic))
  # eval_logic_assert_string = as.character(eval(parse(text = logic)))

  eval_logic_assert_string = deparse(substitute(logic))

  if (!logic) { # TODO: in this case, I think we don't need to expect a string.. so this should be updated to just eval()?

    error_print <- "{paste0(stringr::str_trim(eval_logic_assert_string), collapse = ' ') }  FAILED."%>% glue::glue() # TODO: there are probably a lot of cases here to look out for when trying to turn this into a character

    ### TODO: save mult
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
      print(error_print)
    }

    # TODO: isn't there an error call?
    # TODO: error call occurs regardless of whether console_output_global is true or not
    stop(error_print,
         call. = FALSE)
  }

  # return(invisible(eval_logic_assert)) # return cases that fail the assert...

}
