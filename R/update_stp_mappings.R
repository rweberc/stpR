#' Utility function to update the stp_ob "mapping" items artifacts with a new case.
#'
#' Used primarily internally by the stpR package.
#'
#' @param df_name String name of the data frame that was being filtered.
#' @param ref_ob Reference object that was used to filter the data frame.
#' @param update_ob Update object that was used to filter the data frame.
#' @param from string value of the column(s) in the data frame that represent the columns being processed in the current data set
#' @param to string value of the column(s) in the data frame that represent the columns being processed in the current data set.
#' @param std_proc_na A function that will be applied to the `from` column(s) to determine which values should be considered "standard processing" NA values.  This field is used in cases when processing values such as numbers, where it's not necessary to know all the "typically" processed values, but it is helpful to know the values that aren't readily processed by this standard processing as well as how they are handled in the current data set.
#' @param highlight Boolean to indicate if the case should be highlighted. Defaults to FALSE.
#' @param issue Boolean to indicate if the case should be flagged as an issue. Defaults to FALSE.
#' @param notes String notes associated with the case.
#' @param report Boolean to indicate if the case should be flagged for reporting. Defaults to TRUE.
#' @param perform_compare Boolean to indicate if the case should be flagged for comparison. Defaults to TRUE.
#' @param project_dictionary The project_dictionary object.
#' @param project_directory The project_directory object.
#'
#'
update_stp_mappings <- function(df_name,
                                ref_ob,
                                update_ob,
                                from,
                                to,
                                std_proc_na,
                                highlight,
                                issue,
                                notes,
                                report,
                                perform_compare,
                                stp_id_var,
                                project_dictionary,
                                project_directory) {

  # Try to read current mapping
  stp_ob = get_stp_object(project_dictionary,
                          dir = project_directory,
                          file_type = "current")

  # Check if stp_ob currently has a row for this id
  existing_rows = stp_ob$mapping_items %>%
    dplyr::filter(stp_id == stp_id_var) %>%
    nrow()

  if (existing_rows > 0) {

    if (project_dictionary$allow_updates_to_existing_individual_artifacts == FALSE) { # TODO: again, consider get() function

      warning("Rows exist for ... but allow_updates_to_existing_individual_artifacts is set to FALSE.",
              call. = FALSE)

      return() # TODO: likely need to update return statement

    } else {

      # if overwrite allows, removed existing row before making update
      stp_ob$mapping_items = stp_ob$mapping_items %>%
        dplyr::filter(stp_id != stp_id_var)

    }
  }

  compare_path = project_dictionary$compare_metadata_path
  has_update_ob = as.numeric(!is.null(update_ob))

  stp_ob$mapping_items = stp_ob$mapping_items %>% # TODO: add "|" that if strings are "" function... is.null.. is.na or "", then they are saved as empty character
    dplyr::add_row(
      stp_id = stp_id_var,
      df_name = ifelse(is.null(df_name), character(), df_name),
      from = ifelse(is.null(from), character(), from),
      to = ifelse(is.null(to), character(), to),
      std_proc_na = ifelse(is.null(std_proc_na), list(), list(std_proc_na)),
      notes = ifelse(is.null(notes), character(), notes),
      highlight = ifelse(is.null(highlight), FALSE, highlight),
      issue = ifelse(is.null(issue), FALSE, issue),
      report = ifelse(is.null(report), FALSE, report),
      perform_compare = ifelse(is.null(perform_compare), FALSE, perform_compare),
      compare_path = ifelse(is.null(compare_path), character(), compare_path),
      ref_ob = ifelse(is.null(ref_ob), list(), list(ref_ob)),
      update_ob = ifelse(is.null(update_ob), list(), list(update_ob)),
      has_update_ob = has_update_ob,
      timestamp = lubridate::now()
    )

  # TODO: need to add path check
  saveRDS(stp_ob, file.path(project_directory, project_dictionary$current_metadata_path))

}

