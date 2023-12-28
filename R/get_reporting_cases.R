get_reporting_cases = function(stp_ob,
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
