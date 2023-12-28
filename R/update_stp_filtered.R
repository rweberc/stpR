#'
#' @return
#'
#' @export
#'
update_stp_filtered <- function(df_name,
                                ref_ob,
                                update_ob,
                                stp_id_var,
                                id_vars,
                                group_by_vars,
                                keep_logic,
                                highlight,
                                issue,
                                notes,
                                report = report,
                                perform_compare,
                                project_dictionary,
                                project_directory) {

  # Try to read current mapping
  stp_ob = get_stp_object(project_dictionary,
                          dir = project_directory,
                          file_type = "current")

  # check if stp_ob currently has a row for this id
  existing_rows = stp_ob$filter_items %>%
    dplyr::filter(stp_id == stp_id_var) %>%
    nrow()

  if (existing_rows > 0) {

    if (project_dictionary$allow_updates_to_existing_individual_artifacts == FALSE) { # TODO: again, consider get() function

      warning("Rows exist for ... but allow_updates_to_existing_individual_artifacts is set to FALSE.")
      return() # TODO: likely need to update return statement

    } else {

      # if overwrite allows, removed existing row before making update
      stp_ob$filter_items = stp_ob$filter_items %>%
        dplyr::filter(stp_id != stp_id_var)

    }
  }

  compare_path = project_dictionary$compare_metadata_path
  has_update_ob = as.numeric(!is.null(update_ob))

  stp_ob$filter_items = stp_ob$filter_items %>% # TODO: add "|" that if strings are "" function... is.null.. is.na or "", then they are saved as empty character
    dplyr::add_row(
      df_name = ifelse(!is.null(df_name), character(), df_name),
      id_vars = ifelse(is.null(id_vars), character(), id_vars),
      group_by_vars = ifelse(is.null(group_by_vars), character(), group_by_vars),
      keep_logic = ifelse(is.null(keep_logic), character(), as.character(keep_logic)),
      stp_id = stp_id_var,
      notes = ifelse(is.null(notes), character(), notes),
      highlight = ifelse(is.null(highlight), FALSE, highlight),
      issue = ifelse(is.null(issue), FALSE, issue),
      report = ifelse(is.null(report), FALSE, report),
      perform_compare = ifelse(is.null(perform_compare), FALSE, perform_compare),
      compare_path = ifelse(is.null(compare_path), character(), compare_path),
      ref_ob = ifelse(is.null(ref_ob), list(), ref_ob),
      update_ob = ifelse(is.null(update_ob), list(), update_ob),
      has_update_ob = has_update_ob,
      timestamp = lubridate::now()
    )


  # TODO: need to add path check
  saveRDS(stp_ob, file.path(project_directory, project_dictionary$current_metadata_path))

}


