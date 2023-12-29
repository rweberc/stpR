#' Return cases relevant for reporting
#'
#' Based on the current state of the stpR object.  Currently, only returns those values that are flagged for reporting (i.e., report = TRUE).
#' In the future, this function will also allow returning "non-report" items that have updates associated with them as compared with the previous data set.
#'
#' @param stp_ob stpR artifact object. Default is the current state of the stpR object.
#' @param limit_to_highlight boolean to limit returned values to those that are highlighted
#' @param limit_to_issue boolean to limit returned values to those that are flagged as issues
#'
#' @return a dataframe of the cases that are flagged for reporting
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#'   get_reporting_cases()
#'
#' }
get_reporting_cases = function(stp_ob = get_stp_object(),
                               limit_to_highlight = FALSE,
                               limit_to_issue = FALSE) {

  output_ob = stp_ob$mapping_items %>%
    dplyr::filter(report == TRUE) %>%
    dplyr::select(stp_id,
           notes,
           highlight,
           issue,
           report,
           ref_ob,
           update_ob,
           has_update_ob) %>%
    dplyr::mutate(reference_object_row = TRUE) %>%
    dplyr::bind_rows(stp_ob$filter_items %>%
                       dplyr::filter(report == TRUE) %>%
                       dplyr::select(stp_id,
                                     notes,
                                     highlight,
                                     issue,
                                     report,
                                     ref_ob,
                                     update_ob,
                                     has_update_ob) %>%
                       dplyr::mutate(filter_object_row = TRUE))


  if (limit_to_highlight)
    output_ob = output_ob %>%
      dplyr::filter(highlight == 1)

  if (limit_to_issue)
    output_ob = output_ob %>%
      dplyr::filter(issue == 1)

  return(output_ob)
}
