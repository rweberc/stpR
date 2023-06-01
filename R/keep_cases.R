
#'
#' @return
#'
#' @export
#'
keep_cases <- function(data_df = NULL,
                       stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                       id_vars,
                       non_id_vars,
                       group_by_vars = NULL, # possibly can stuff into the logic
                       keep_logic,
                       highlight = FALSE, # similar with issue... this would initially apply the mapping and not the update items
                       issue = FALSE, # TODO: if you say issue = 1... start with just the mapping itself... but there should be a way to deal with to highlight the update artifact for issue tracking...
                       filter_notes = NULL,
                       update_notes = NULL,
                       artifact = TRUE, # create an artifact to be saved out (almost always true... only false if you just wanted to use this function to do the count() type check with benefit of console output control)
                       perform_compare = TRUE, # case where you just want to highlight something, but didn't want to "enforce" any particular relationship
                       project_dictionary = get_project_dictionary(),
                       project_directory = here::here())
{


  # Next to do...
  # figure out internal objects
  # do filter
  # do check on compare (compare just based on ids... forget about the non-id vars for the compare... right?)
  # create save function
  # work up output


  # TODO: input checks (including figuring out if fields are in dataframes of interest)

  # initialize internal objects

  stp_ob = NULL
  stp_comp_ob = NULL

  new_filter = NULL
  compare_filter = NULL

  filter_new = NULL
  filter_missing = NULL # Note: don't track 'filter_changed' (that is where the non_id_var values changed for a given filter id set)
  filter_diff = NULL


  data_df <- data_df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(id_vars, non_id_vars))) %>%
    dplyr::group_by_at(c(group_by_vars)) # TODO: check that if no group_by_vars are provided, does this ungroup()... does it throw an error?


  # Get cases to filter
  remove_cases <- data_df %>%
    dplyr::filter((!eval(parse(text = filter_logic))) %>%
                    tidyr::replace_na(FALSE)) # TODO: add in option with default to either keep or remove the NA cases


  # Get remaining cases to keep
  keep_cases <- data_df %>%
    dplyr::filter(eval(parse(text = logic)) %>%
                    tidyr::replace_na(TRUE)) # TODO: add in option with default to either keep or remove the NA cases


  ## If writing or comparing, determined document id (only need if going to save out)
  if (artifact) {

    if (stp_project_dictionary$compare_artifacts_global & perform_compare) { # TODO: update the stp_project_dictionary piece to a get() function that can give a warning when there's not a TRUE/FALSE value for this field

      stp_comp_ob = get_stp_object(stp_project_dictionary,
                                   dir = project_directory,
                                   file_type = "compare")

      if (is.null(stp_comp_ob))
        stop("Cannot perform compare of data artifacts when no compare object could be found.") # could put in details of path, etc.)
      # TODO: if true, update to a break out of compare enclosure and add warning

      # TODO: figure in a mapping option for use against old names fields in previous data (that object would say specify, if comparing this date vs that date for x field, use this mapping, else, look for the current mapping)
      # TODO: possibly have that mapping say, ignore comparing this for a particular date...

      # get compare mapping
      compare_cases = stp_comp_ob$filter_items %>%
        dplyr::filter(id == stp_id) %>%
        dplyr::pull(filter_ob) # todo... have to update this syntax... [[.]]?

      # TODO: confirm there only one such of these items... confirm existence
      # TODO: compare the column field names... might need to include this in the optional traversing name updates

      # TODO: have warning about the same columns existing in previous and current
      # TODO: have warning about classes of columns being the same (add in check that columns have to be atomic type... not checking values of lists, etc.)

      # TODO: decide how to deal with the warning related to many-to-many joins.... just keep it?... always avoid it (since you're outputting the results?)

      ## [ new ]
      filter_new <- filter_cases %>%
        dplyr::anti_join(compare_cases %>%
                           dplyr::select(dplyr::all_of(id_vars)),
                         by = id_vars) %>%
        dplyr::mutate(.stp_compare_order_num = 1,
                      compare_results = "missing in old")

      ## [ missing ]
      filter_missing <- compare_cases %>%
        dplyr::anti_join(filter_cases %>%
                           dplyr::select(dplyr::all_of(id_vars)),
                         by = id_vars) %>%
        dplyr::mutate(.stp_compare_order_num = 2,
                      compare_results = "new in compare")



      filter_diff <- filter_missing %>%
        dplyr::bind_rows(filter_new) %>%
        dplyr::arrange(.stp_compare_order_num, dplyr::all_of(id_vars, non_id_vars)) %>%
        dplyr::select(-.stp_compare_order_num) # TODO: maybe keep and only remove when printing/returning?

    }


    #   - save out mapping, update artifact, and update issues
    if (stp_project_dictionary$save_metadata_global) {

      df_name = deparse(substitute(data_df)) # TODO: likely need checks added to this kind of call

      # Need to send project dictionary... need to send compare? ... only check if the map_diff is not null?
      # Need to send the notes

      update_stp_filtered(
        df_name,
        stp_id,
        id_vars,
        group_by_vars,
        keep_logic,
        highlight,
        issue,
        filter_notes,
        update_notes,
        project_dictionary,
        project_directory
      )

    }
  }


  if (stp_project_dictionary$console_output_global) {

    # TODO: update how these are formatted/what text accompanies

    # TODO: update function used to output messages... what's the recommended approach? message("") for text.. but for dataframes?
    message("New Mapping:")
    print(new_map)

    if (!is.null(map_diff)){

      if (nrow(map_diff) > 0) {
        message("Updated Mappings:") # TODO: add exclusion statement if std_proc_na is not null
        print(map_diff)
      }
    }

    message("See returned object for further details")

  }

  return(invisible(
    new_map %>%
      dplyr::mutate(current_mapping = 1) %>%
      bind_rows(map_diff)
  ))

  # TODO: return new mapping and updated mappings in a single object...

}
