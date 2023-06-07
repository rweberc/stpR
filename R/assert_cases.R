

#'
#' @return
#'
#' @export
#'
assert_cases <- function(logic,
                         stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                         project_dictionary = get_project_dictionary(),
                         project_directory = here::here()
                         )
{
  # TODO: input checks...

  eval_logic_assert = eval(parse(text = logic))

  eval_logic_assert_string = as.character(eval(parse(text = logic)))

  if (!eval_logic_assert) { # TODO: in this case, I think we don't need to expect a string.. so this should be updated to just eval()?

    error_print <- paste0(eval_logic_assert_string, "' FAILED.")

    ### TODO: save mult
    if (stp_project_dictionary$save_metadata_global) {

      log_item(type = "ALERT",
               item = eval_logic_assert_string, # TODO: if statement is not a string but R code... will as.character() just give a "print" of it?
               add_item = NULL,
               priority = 1,
               stp_id = stp_id, # TODO: is it a concern if id's in different stp_ob entities are the same
               project_dictionary = project_dictionary,
               project_directory = project_directory)
    }

    if (stp_project_dictionary$console_output_global) {
      print(multDf)
    }

    # TODO: isn't there an error call?
    # TODO: error call occurs regardless of whether console_output_global is true or not
    stop(error_print,
         call. = FALSE)
  }

  return(invisible(eval_logic_assert)) # return cases that fail the assert...

}
