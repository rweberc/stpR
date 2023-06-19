
#'
#' @return
#'
#' @export
#'
assert_distinct <- function(data_df,
                            group_by_vars = NULL,
                            stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                            project_dictionary = get_project_dictionary(),
                            project_directory = here::here()
                            )
{

  # TODO: input checks...

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

    ### TODO: save mult
    if (project_dictionary$save_metadata_global) {

      log_item(type = "ALERT",
               item = error_print,
               add_item = unionDf, # for now, this is to encompass the assert_distinct, assert_cases scenarios... but right now only do the duplicate checks by "item"... some other work needs to be done here
               priority = 1,
               stp_id = stp_id, # TODO: is it a concern if id's in different stp_ob entities are the same
               project_dictionary = project_dictionary,
               project_directory = project_directory)
    }

    if (project_dictionary$console_output_global) {
      print(multDf)
    }

    # TODO: isn't there an error call?
    # TODO: error call occurs regardless of whether console_output_global is true or not
    stop(error_print,
         call. = FALSE)
  }

  return(invisible(unionDf))

}
