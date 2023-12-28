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

  # you know it's to save a mapping if ref_ob isn't null.... you know if there's an update to save if ref_ob isn't null (are there exception cases?)

  # TODO: have option to read this, then make update and write it to directory, then return the copy to work from...

  # TODO: check for existence of stp_ob (when update = TRUE)

  # TODO: check stp_ob is well-formed (no NAs for ids)

  # TODO: check for existence of row (only allow for complete overwrite or addition?)

  # TODO: update when row id may also respect the data frame

  # TODO: likely should update this function so that it just receives a list of values for add_item... then depending on the entity in the stp_ob,
  #         creates the row and does some checking...

  # Try to read current mapping
  stp_ob = get_stp_object(project_dictionary,
                          dir = project_directory,
                          file_type = "current")

  # check if stp_ob currently has a row for this id
  existing_rows = stp_ob$filter_items %>%
    dplyr::filter(stp_id == stp_id_var) %>%
    nrow()

  # TODO: perhaps have function to do all things like checking for duplicates by id, etc.

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

  # TODO: add data-type checks for the input data?  must have stp_id_var... must have a ref_ob?

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


# maybe update this so every time it is reading it from the path.expand

# first tries to read it from the path

# if not there, attempts to read from the parent environment (+ warning)

# if reads from the path, then necessarily writes to the path

# if reads from the environment, then gives warning, and does not write to the path ... a lot more

# have dictionary object to suppress warnings about modifying the object in the parent environment? (this would be to limit all the read/write tot he disk for each function call...)



