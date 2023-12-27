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

eval_map <- function(data_df = NULL,
                     from = NULL,
                     to = NULL,
                     std_proc_na = as.numeric,
                     highlight = FALSE, # similar with issue... this would initially apply the mapping and not the update items
                     issue = FALSE, # TODO: if you say issue = 1... start with just the mapping itself... but there should be a way to deal with to highlight the update artifact for issue tracking...
                     notes = NULL,
                     stp_id = NULL,
                     save_artifact = TRUE, # create an artifact to be saved out (almost always true... only false if you just wanted to use this function to do the count() type check with benefit of console output control)
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
      dplyr::group_by(dplyr::across(dplyr::all_of(c(from, to)))) %>%
      dplyr::count() # TODO: consider adding in % for each row

  } else {

    df_name = NULL

    var_list = names(data_df)

    new_map <- data_df %>%
      dplyr::ungroup()

  }

  ## If writing or comparing, determined document id (only need if going to save out)
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

      # TODO: figure in a mapping option for use against old names fields in previous data (that object would say specify, if comparing this date vs that date for x field, use this mapping, else, look for the current mapping)
      # TODO: possibly have that mapping say, ignore comparing this for a particular date...

      # get compare mapping
      compare_map = stp_comp_ob$mapping_items %>%
        dplyr::filter(stp_id == stp_id) %>%
        dplyr::pull(ref_ob) # todo... have to update this syntax... [[.]]?

      # TODO: confirm there only one such of these items... confirm existence
      # TODO: compare the column field names... might need to include this in the optional traversing name updates

      # TODO: have warning about the same columns existing in previous and current
      # TODO: have warning about classes of columns being the same (add in check that columns have to be atomic type... not checking values of lists, etc.)

      # TODO: decide how to deal with the warning related to many-to-many joins.... just keep it?... always avoid it (since you're outputting the results?)

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


    #   - save out mapping, update artifact, and update issues
    if (project_dictionary$save_metadata_global) {

      # Need to send project dictionary... need to send compare? ... only check if the update_ob is not null?
      # Need to send the notes

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


  if (project_dictionary$console_output_global) {

    # TODO: update how these are formatted/what text accompanies

    # TODO: update function used to output messages... what's the recommended approach? message("") for text.. but for dataframes?
    message("Current Mapping:")
    print(new_map)

    if (!is.null(update_ob)){

      if (nrow(update_ob) > 0) {
        message("Updated Mappings:") # TODO: add exclusion statement if std_proc_na is not null
        print(update_ob)
      }
    }

    if (nrow(new_map) > 9 |
        (!is.null(update_ob) && nrow(update_ob) > 0))
      message("See returned object for further details")

  }

  return(invisible(
    new_map %>%
      dplyr::mutate(current_mapping = 1) %>%
      dplyr::bind_rows(update_ob)
  ))

  # TODO: return new mapping and updated mappings in a single object...

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

