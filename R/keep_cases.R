
#'
#' @return
#'
#' @export
#'
keep_cases <- function(data_df = NULL,
                       stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                       id_vars, # TODO: is this reasonable to assume that you always have?
                       non_id_vars,
                       group_by_vars = NULL, # possibly can stuff into the logic
                       keep_logic,
                       highlight = FALSE, # similar with issue... this would initially apply the mapping and not the update items
                       issue = FALSE, # TODO: if you say issue = 1... start with just the mapping itself... but there should be a way to deal with to highlight the update artifact for issue tracking...
                       notes = NULL,
                       save_artifact = TRUE, # create an artifact to be saved out (almost always true... only false if you just wanted to use this function to do the count() type check with benefit of console output control)
                       report = TRUE,
                       perform_compare = TRUE, # case where you just want to highlight something, but didn't want to "enforce" any particular relationship
                       project_dictionary = get_project_dictionary(),
                       project_directory = here::here())
{

  df_name = deparse(substitute(data_df)) # TODO: likely need checks added to this kind of call

  # initialize internal objects

  stp_ob = NULL
  stp_comp_ob = NULL

  new_filter = NULL
  compare_filter = NULL

  filter_new = NULL
  filter_missing = NULL # Note: don't track 'filter_changed' (that is where the non_id_var values changed for a given filter id set)
  update_ob = NULL

  # TODO: what if id_vars and non_id_vars do not include group_at_vars... will be included if df already grouped, right?  ... add something explicit?

  data_df <- data_df  %>%
    dplyr::group_by_at(c(group_by_vars)) # TODO: check that if no group_by_vars are provided, does this ungroup()... does it throw an error?



  # Get cases to filter
  remove_cases <- data_df %>%
    dplyr::filter((!eval(parse(text = keep_logic))) %>%
                    tidyr::replace_na(FALSE)) %>% # TODO: add in option with default to either keep or remove the NA cases
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(id_vars, non_id_vars)))


  # Get remaining cases to keep
  keep_cases <- data_df %>%
    dplyr::filter(eval(parse(text = keep_logic)) %>%
                    tidyr::replace_na(TRUE)) %>% # TODO: add in option with default to either keep or remove the NA cases
    dplyr::ungroup() # TODO: might not be expected that a grouping is removed...


  ## If writing or comparing, determined document id (only need if going to save out)
  if (save_artifact) {

    if (project_dictionary$compare_artifacts_global & perform_compare) {

      stp_comp_ob = get_stp_object(project_dictionary,
                                   dir = project_directory,
                                   file_type = "compare")

      if (is.null(stp_comp_ob))
        stop("Cannot perform compare of data artifacts when no compare object could be found.")

      # get compare mapping
      compare_cases = stp_comp_ob$filter_items %>%
        dplyr::filter(id == stp_id) %>%
        dplyr::pull(ref_ob) # TODO: have to update this syntax... [[.]]?

      ## [ new ]
      filter_new <- remove_cases %>%
        dplyr::anti_join(compare_cases %>%
                           dplyr::select(dplyr::all_of(id_vars)),
                         by = id_vars) %>%
        dplyr::mutate(.stp_compare_order_num = 1,
                      compare_results = "missing in old")

      ## [ missing ]
      filter_missing <- compare_cases %>%
        dplyr::anti_join(remove_cases %>%
                           dplyr::select(dplyr::all_of(id_vars)),
                         by = id_vars) %>%
        dplyr::mutate(.stp_compare_order_num = 2,
                      compare_results = "new in compare")

      update_ob <- filter_missing %>%
        dplyr::bind_rows(filter_new) %>%
        dplyr::arrange(.stp_compare_order_num, dplyr::all_of(id_vars, non_id_vars)) %>%
        dplyr::select(-.stp_compare_order_num) # TODO: maybe keep and only remove when printing/returning?

    }


    #   Save out mapping, update artifact, and update issues
    if (project_dictionary$save_metadata_global) {

      # Need to send project dictionary... need to send compare? ... only check if the remove_cases is not null?
      # Need to send the notes

      update_stp_filtered(
        df_name = df_name,
        ref_ob = remove_cases,
        update_ob = update_ob,
        stp_id_var = stp_id,
        id_vars = id_vars,
        group_by_vars = group_by_vars,
        keep_logic = keep_logic,
        highlight = highlight,
        issue = issue,
        notes = notes,
        report = report,
        perform_compare = perform_compare,
        project_dictionary = project_dictionary,
        project_directory = project_directory
      )

    }
  }


  if (project_dictionary$console_output_global) {

    # TODO: update function used to output messages... what's the recommended approach? message("") for text.. but for dataframes?
    message("Filtered cases:")
    print(remove_cases)

    if (!is.null(update_ob)){

      if (nrow(update_ob) > 0) {
        message("Updated filtered:")
        print(update_ob)
      }
    }

    message("See returned object for further details")

  }


  return(invisible(keep_cases))

  # Returning removed cases
  # return(invisible(
  #   remove_cases %>%
  #     dplyr::mutate(current_filtering = 1) %>%
  #     dplyr::bind_rows(update_ob)
  # ))

}
