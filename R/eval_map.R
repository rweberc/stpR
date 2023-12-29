#' Primary setup utility for stpR.  This function is used to create the initial metadata objects that are used by the other setup utilities in stpR.
#' Processes stp_ob artifacts for multiple scenarios:
#'
#'  - Initial processing of raw data for both categorical and "continuous" fields (depending on whether std_proc_na is NULL)
#'  - General summary of value combinations for several columns: As a utility, `eval_summary()` also utilizes the functionality of this utility, as well: When `summary = TRUE`, the `data_df` is assumed to be a summary table rather than a processing of `from` and `to` values as is commonly the case for the use of eval_map().
#'  - Comparison of current data against previous data artifacts
#'  - Additional creation of tags of items to note as "issues" or "highlights" for reporting of the current state of the data
#'
#' @param data_df The data frame to be evaluated.
#' @param from string value of the column(s) in the data frame that represent the columns being processed in the current data set
#' @param to string value of the column(s) in the data frame that represent the columns being processed in the current data set.
#' @param std_proc_na A function that will be applied to the `from` column(s) to determine which values should be considered "standard processing" NA values.  This field is used in cases when processing values such as numbers, where it's not necessary to know all the "typically" processed values, but it is helpful to know the values that aren't readily processed by this standard processing as well as how they are handled in the current data set.
#' @param highlight A boolean tag to denote whether this processing is of interest for highlighting in the reporting of the current state of the data.  Generally, this would be a type of mapping or field processing that would be of interest to routinely review for each data set processing.
#' @param issue A boolean tag to denote whether this processing is of interest for noting for inspection as an issue in the reporting of the current state of the data.
#' @param notes A string value to be included in the reporting of the current state of the data.
#' @param stp_id A string value to be used as the identifier for this processing in the reporting of the current state of the data. By default, this is the `to` column.
#' @param save_artifact A boolean tag to denote whether this processing should be saved as an artifact.  Only false if you just wanted to use this function to do the count() type check with benefit of console output control.
#' @param report A boolean tag to denote whether this processing should be included in the reporting of the current state of the data.  May be false in scenarios where you didn't want to routinely look at the output for processing, but did want to be alerted when there were updates of values.
#' @param perform_compare A boolean tag to denote whether this processing should be compared against the previous data artifact. to be used in the comparison of the current data against previous data artifacts.  This should be true when you want to "enforce" a given relationship
#' @param project_dictionary The project_dictionary object, which by default is output by the `get_project_dictionary()` function.
#' @param project_directory The project_directory object, which by default is output by the `here::here()` function.
#' @param summary A boolean tag to denote whether this processing is a summary of the data rather than a mapping of values.  Primarily only true when this function is called from `eval_summary()`.
#'
#' @return a data frame with the current mapping and any updates to the mapping, if relevant.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   # mapping with issue
#'   eval_map(
#'     data_df = test_tbl,
#'     from = "TRT_RAW",
#'     to = "trt",
#'     notes = "Note multiple raw processed values associated with each treatment type"
#'   )
#'
#'   test_tbl$age <- as.numeric(test_tbl$AGE_RAW)
#'   test_tbl$age[test_tbl$AGE_RAW == "twenty"] <- 20
#'
#'   # 'continuous' field with issue
#'   eval_map(
#'     data_df = test_tbl,
#'     from = "AGE_RAW",
#'     to = "age",
#'     std_proc_na = as.numeric,
#'     notes = "Note processing of value of 'twenty' from AGE_RAW"
#'   )
#'
#' }

eval_map <- function(data_df = NULL,
                     from = NULL,
                     to = NULL,
                     std_proc_na = NULL,
                     highlight = FALSE, # similar with issue... this would initially apply the mapping and not the update items
                     issue = FALSE, # TODO: if you say issue = 1... start with just the mapping itself... but there should be a way to deal with to highlight the update artifact for issue tracking...
                     notes = NULL,
                     stp_id = NULL,
                     save_artifact = TRUE,
                     report = TRUE,
                     perform_compare = TRUE, # case where you just want to highlight something, but didn't want to "enforce" any particular relationship
                     project_dictionary = get_project_dictionary(),
                     project_directory = here::here(),
                     summary = FALSE)
{

  # input checks
  # TODO: make sure variables are expected type (dataframe, function, etc.)
  # TODO: id is null and to is not length = 1
  # TODO: can't have document be true and artifact be false
  if (!is.null(std_proc_na) & (length(to) > 1 | length(from) > 1))
    stop("Cannot remove 'standard processing' values if from and to parameters contain more than one field.")

  # initialize internal objects
  df_name = NULL
  var_list = NULL

  stp_ob = NULL
  stp_comp_ob = NULL

  new_map = NULL
  compare_map = NULL

  map_new = NULL
  map_changed = NULL
  map_missing = NULL

  ref_ob = NULL
  update_ob = NULL


  # This is the default cases where the from and to columns are used to define the dataframe being evaluated
  # Summary == TRUE when this function, eval_map(), is being called from eval_summary()
  if (!summary) {
    df_name = deparse(substitute(data_df)) # TODO: likely need checks added to this kind of call - consider removing, since may be dependent on type of data frame passed in

    var_list = c(from, to)

    # limit to fields of interest
    data_df <- data_df %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(from, to))) %>%
      dplyr::arrange(dplyr::arrange(dplyr::across(dplyr::all_of(c(to, from)))))


    # if std_proc_na, only keep those cases that resultin NA for the standard processing
    if (!is.null(std_proc_na))
      data_df <- data_df %>%
        dplyr::filter_at(from, ~ is.na(suppressWarnings(std_proc_na(.))))


    # create current mapping object
    new_map <- data_df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(to, from)))) %>%
      dplyr::count() # TODO: consider adding in % for each row

  } else {

    # To set up variables for the scenario when eval_map() is being called from eval_summary() (i.e., when summary = TRUE)
    df_name = NULL

    var_list = names(data_df)

    new_map <- data_df %>%
      dplyr::ungroup()

  }

  ## If writing or comparing, determine document id (only need if going to save out)
  if (save_artifact) {

    if (is.null(stp_id)) {
      if (length(to) == 1) {
        stp_id = to
      } else {
        stop("When creating an artifact or comparing current data against previous, either need 'to' of length 1 or stp_id explicitly specified.") # TODO: possibly update to a warning
      }
    }

    if (project_dictionary$compare_artifacts_global & perform_compare) { # TODO: update the project_dictionary piece to a get() function that can give a warning when there's not a TRUE/FALSE value for this field

      stp_comp_ob = get_stp_object(project_dictionary,
                                   dir = project_directory,
                                   file_type = "compare")

      if (is.null(stp_comp_ob))
        stop("Cannot perform compare of data artifacts when no compare object could be found.") # could put in details of path, etc.)
        # TODO: if true, update to a break out of compare enclosure and add warning

      # Get compare mapping
      compare_map = stp_comp_ob$mapping_items %>%
        dplyr::filter(stp_id == stp_id) %>%
        dplyr::pull(ref_ob) # todo... have to update this syntax... [[.]]?

      ## [ new ]
      map_new <- new_map %>%
        dplyr::anti_join(old_map,
                         by = from) %>%
        dplyr::mutate(.stp_compare_order_num = 1,
                      compare_results = "missing in old")

      ## [ missing ]
      map_missing <- compare_map %>%
        dplyr::anti_join(new_map,
                         by = from) %>%
        dplyr::mutate(.stp_compare_order_num = 2,
                      compare_results = "new in compare")


      ## [ changed ]
      # inner_join is to get the relevant from and two rows and columns from old_map; anti_join is to limit these to where there are differences in the `to` columns with respect to the `from` columns existing in the
      old_map_anti <- old_map %>%
        dplyr::inner_join(new_map %>%
                            dplyr::select_at(from),
                          by = from) %>%
        unique() %>% #
        dplyr::anti_join(new_map,
                         by = var_list) %>%
        unique() %>%
        dplyr::mutate(.stp_compare_order_num = 2,
                      compare_results = "changed `to` [old]")

      # do same as above but in the other direction
      new_map_anti <- new_map %>%
        dplyr::inner_join(old_map %>%
                            dplyr::select_at(from),
                          by = from) %>%
        unique() %>%
        dplyr::anti_join(old_map,
                         by = var_list) %>%
        unique() %>%
        dplyr::mutate(.stp_compare_order_num = 2,
                      compare_results = "changed `to` [new]")


      # join the two above by shared "from" values
      # TODO: for cases where there's no -to-many or multiple `to` columns... format of one row per `to` with columns in wide format best, but for other cases, long format likely better...
      #         going to just do all in long format for now
      map_changed <- old_map_anti %>%
        dplyr::bind_rows(new_map_anti)


      update_ob <- map_missing %>%
        dplyr::bind_rows(map_new) %>%
        dplyr::bind_rows(map_changed)%>%
        dplyr::arrange(.stp_compare_order_num, dplyr::all_of(var_list)) %>%
        dplyr::select(-.stp_compare_order_num) # TODO: maybe keep and only remove when printing/returning?

    }


    # Save out mapping, update artifact, and update issues (when relevant based on below conditions)
    if (project_dictionary$save_metadata_global) {

      # TODO: can update to remove several of the unnecessary variables sent here (perform_compare, etc.)
      update_stp_mappings(
        df_name = df_name,
        ref_ob = new_map,
        update_ob = update_ob,
        from = from,
        to = to,
        std_proc_na = std_proc_na,
        highlight = highlight,
        issue = issue,
        notes = notes,
        report = report,
        perform_compare = perform_compare,
        stp_id_var = stp_id,
        project_dictionary = project_dictionary,
        project_directory = project_directory
      )

    }
  }


  # Send relevant output to console depending on below conditions
  if (project_dictionary$console_output_global) {

    if (!is.null(std_proc_na))
      output_data_to_console(new_map, header_text = "Current Mapping of Standard Processing NA Values:")
    else
      output_data_to_console(new_map, header_text = "Current Mapping:")


    output_data_to_console(update_ob, header_text = "Updated Mappings:")

  }

  return(invisible(
    new_map %>%
      dplyr::mutate(current_mapping = 1) %>%
      dplyr::bind_rows(update_ob)
  ))

}



