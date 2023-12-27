#' Description
#'
#'
#' @param data_df
#' @param from
#' @param to
#' @param std_proc_na
#' @param highlight
#' @param issue
#' @param id
#' @param notes
#' @param save_artifact
#' @param report
#' @param perform_compare
#' @param stpr_ob
#'
#' @return
#'
#' @export
#'


eval_summary <- function(summary_df = NULL,
                         key,
                         highlight = FALSE, # similar with issue... this would initially apply the mapping and not the update items
                         issue = FALSE, # TODO: if you say issue = 1... start with just the mapping itself... but there should be a way to deal with to highlight the update artifact for issue tracking...
                         notes = NULL,
                         stp_id,
                         save_artifact = TRUE, # create an artifact to be saved out (almost always true... only false if you just wanted to use this function to do the count() type check with benefit of console output control)
                         report = TRUE,
                         perform_compare = TRUE, # case where you just want to highlight something, but didn't want to "enforce" any particular relationship
                         project_dictionary = get_project_dictionary(),
                         project_directory = here::here()) {


  # Differs from eval_map based on the following parameters:
  #   summary_df is the artifact itself... it doesn't not to be creating via a count() call on the to/from variables
  #   The 'from' variable here is called 'key', since the variables are not necessarily being derived from one or another (but may be in some cases)
  #       In some scenarios, deciding on the key variable will be arbitrary... but without it, would have to change how the "compare" logic works, so just keeping it here
  summary_df = summary_df

  eval_map(
    data_df = summary_df,
    from = key, # TODO: should update to be required when perform_compare = TRUE
    highlight = highlight,
    issue = issue,
    notes = notes,
    stp_id = stp_id,
    save_artifact = save_artifact,
    report = report,
    perform_compare = perform_compare,
    project_dictionary = project_dictionary,
    project_directory = project_directory,
    summary = TRUE
  )
}

