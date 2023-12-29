#' Utility to filter cases from a data set that do not meet a specified condition.
#'
#' This utility also creates an artifact of removed cases for reporting and comparison against separate data set processing.
#'
#' @param data_df A data set to be evaluated and possibly filtered.
#' @param stp_id A unique identifier for this filter instance.
#' @param id_vars A string or vector of strings representing fields within the data set to use as identifiers.
#' @param non_id_vars A string or vector of strings representing fields within the data set to use as non-identifiers.
#' @param group_by_vars A string or vector of strings representing fields within the data set to group by.
#' @param keep_logic A string representing the logic to be used to filter the data set. Default is to keep cases where the logic evaluates to TRUE.
#' @param highlight A logical value indicating whether or not to highlight the filtered cases. Default is FALSE.
#' @param issue A logical value indicating whether or not to flag the filtered cases as an issue. Default is FALSE.
#' @param notes A string representing notes to be associated with the update artifact.
#' @param save_artifact A logical value indicating whether or not to save the update artifact. Default is TRUE.
#' @param report A logical value indicating whether or not to include the update artifact in the report. Default is TRUE.
#' @param perform_compare A logical value indicating whether or not to perform a comparison of the update artifact to the compare artifact. Default is TRUE.
#' @param return_only_filtered A logical value indicating whether or not to return only the filtered cases or the data set with these cases removed.  Default is FALSE
#' @param project_dictionary The project_dictionary object, which by default is output by the `get_project_dictionary()` function.
#' @param project_directory The project_directory object, which by default is output by the `here::here()` function.
#'
#' @return data frame. Depending on the return_only_filtered argument, either the data set with the filtered cases (default), or the filtered data itself.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' qa_tbl <- keep_cases(
#'   data_df = qa_tbl,
#'   stp_id = "age_filter",
#'   id_vars = "col_id",
#'   non_id_vars = "age",
#'   keep_logic = "age >= 18",
#'   notes = "Remove patients younger than 18 years old"
#' )
#'
#' }
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
                       return_only_filtered = FALSE,
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
  filter_missing = NULL # Note: utility doesn't track 'filter_changed' (that is where the non_id_var values changed for a given filter id set)
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


  ## If writing or comparing, determine document id (only need if going to save out)
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

    output_data_to_console(remove_cases, header_text = "Filtered cases:")

  }

  if (return_only_filtered)
    return(invisible(remove_cases))
  else
    return(invisible(keep_cases))

}
