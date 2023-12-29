#' For use with stpR setup cases where a pre-defined mapping of values is of interest to be tracked, but where variables are strictly associated in a `from` and `to` relationship.  Most likely to be utilized
#'
#' This function calls `eval_map()` with the `summary` parameter set to TRUE.  See `eval_map()` for additional details.
#'
#' @param summary_df The data frame, pre-summarized, to be associated with this documentation item.
#' @param key String value of the column name to be used as the key for the summary_df.  Will be utilized similarlly to the `from` column in the `eval_map()` function.  In some scenarios, deciding on the key variable will be arbitrary, but choosing it here to support additional comparison functionality associated with these artifact items.
#' @param highlight A boolean tag to denote whether this processing is of interest for highlighting in the reporting of the current state of the data.  Generally, this would be a type of mapping or field processing that would be of interest to routinely review for each data set processing.
#' @param issue A boolean tag to denote whether this processing is of interest for noting for inspection as an issue in the reporting of the current state of the data.
#' @param notes A string value to be included in the reporting of the current state of the data.
#' @param stp_id A string value to be used as the identifier for this processing in the reporting of the current state of the data. By default, this is the `to` column.
#' @param save_artifact A boolean tag to denote whether this processing should be saved as an artifact.  Only false if you just wanted to use this function to do the count() type check with benefit of console output control.
#' @param report A boolean tag to denote whether this processing should be included in the reporting of the current state of the data.  May be false in scenarios where you didn't want to routinely look at the output for processing, but did want to be alerted when there were updates of values.
#' @param perform_compare A boolean tag to denote whether this processing should be compared against the previous data artifact. to be used in the comparison of the current data against previous data artifacts.  This should be true when you want to "enforce" a given relationship
#' @param project_dictionary The project_dictionary object, which by default is output by the `get_project_dictionary()` function.
#' @param project_directory The project_directory object, which by default is output by the `here::here()` function.
#'
#' @return a data frame with the current mapping and any updates to the mapping, if relevant.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   eval_summary(
#'     summary_df = count(test_tbl, approach, asa),
#'     key = "proc_name",
#'     notes = "summary proc, ASA notes",
#'     stp_id = "comp_approach_asa"
#'   )
#'
#' }


eval_summary <- function(summary_df = NULL,
                         key,
                         highlight = FALSE,
                         issue = FALSE,
                         notes = NULL,
                         stp_id,
                         save_artifact = TRUE,
                         report = TRUE,
                         perform_compare = TRUE,
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

