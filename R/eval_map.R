#' Description
#'
#'
#' @param data_df a dataframe that contains the underlying data to be evaluated
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
#' @param stp_ob
#'
#' @return
#'
#' @export
#'

eval_map <- function(data_df = NULL,
                     from = NULL,
                     to = NULL,
                     std_proc_na = NULL,
                     highlight = FALSE, # similar with issue... this would initially apply the mapping and not the update items
                     issue = FALSE, # TODO: if you say issue = 1... start with just the mapping itself... but there should be a way to deal with to highlight the update artifact for issue tracking...
                     notes = NULL,
                     stp_id = NULL,
                     save_artifact = TRUE, # only false if you just wanted to use this function to do the count() type check with benefit of console output control
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


# example type cases to unit test
# # only get two rows of change... I think as expected
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 2, 1,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 2, 11,
#   "b", 2, 12,
#   "c", 3, 3
# )
#
# # Will get four rows of change for b... one for the change from the perspective of each of the "to" columns
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 1, 11,
#   "b", 2, 12,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 1, 12,
#   "b", 2, 11,
#   "c", 3, 3
# )
#
# # ~ four rows again...
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 9, 20,
#   "b", 10, 21,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 7, 11,
#   "b", 8, 12,
#   "c", 3, 3
# )
#
# # what about when one of these to values is an NA
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 9, 20,
#   "b", 10, 21,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 7, 11,
#   "b", 8, NA,
#   "c", 3, 3
# )
## Looking at case with multiple values but only one 'to' column
# old_map <- tribble(
#   ~ id, ~ col1,
#   "a", 1,
#   "b", 9,
#   "b", 10,
#   "c", 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1,
#   "a", 1,
#   "b", 100,
#   "b", 200,
#   "c", 3
# )
#
# from <-  "id"
# to <- c("col1", "col2")


# TODO: have init_stp() that would suggest and create the needed metadata objects...


# Cases to check... when you have an NA in the from column:
# tibble(id = c(NA, 1, 2), col1 = c(0, 1, NA)) %>% anti_join(tibble(id = 1:3, col1 = c(0, 1, NA)), by = "id")

# Similarly, when you have NA in the to columns (either in old or new)
# tibble(id = c(NA, 1, 2), col1 = c(0, 1, NA)) %>% anti_join(tibble(id = 1:3, col1 = c(0, 1, NA)), by = "id")



# Cases when you have a single value mapped in multiple ways (either in new or old)

