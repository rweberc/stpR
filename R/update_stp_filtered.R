#'
#' @return
#'
#' @export
#'
update_stp_mappings <- function(df_name,
                                stp_id,
                                id_vars,
                                group_by_vars,
                                keep_logic,
                                highlight,
                                issue,
                                filter_notes,
                                update_notes,
                                project_dictionary,
                                project_directory) {

  # you know it's to save a mapping if map_ob isn't null.... you know if there's an update to save if map_diff isn't null (are there exception cases?)

  # TODO: have option to read this, then make update and write it to directory, then return the copy to work from...

  # TODO: check for existence of stp_ob (when update = TRUE)

  # TODO: check stp_ob is well-formed (no NAs for ids)

  # TODO: check for existence of row (only allow for complete overwrite or addition?)

  # TODO: update when row id may also respect the data frame

  # TODO: likely should update this function so that it just receives a list of values for add_item... then depending on the entity in the stp_ob,
  #         creates the row and does some checking...

  # Try to read current mapping
  stp_ob = get_stp_object(stp_project_dictionary,
                          dir = project_directory,
                          file_type = "current")

  # check if stp_ob currently has a row for this id
  existing_rows = stp_ob$filter_items %>%
    dplyr::filter(stp_id == stp_id) %>%
    dplyr::nrow()

  # TODO: perhaps have function to do all things like checking for duplicates by id, etc.

  if (existing_rows > 0) {

    if (stp_project_dictionary$allow_overwrite_artifacts_global == FALSE) { # TODO: again, consider get() function

      warning("Rows exist for ... but allow_overwrite_artifacts_global is set to FALSE.")
      return() # TODO: likely need to update return statement

    } else {

      # if overwrite allows, removed existing row before making update
      stp_ob$filter_items = stp_ob$filter_items %>%
        dplyr::filter(stp_id != stp_id)

    }
  }

  # TODO: add data-type checks for the input data?  must have stp_id... must have a map_ob?

  compare_path = stp_project_dictionary$compare_metadata_path
  has_update_ob = as.numeric(!is.null(update_ob))

  stp_ob$filter_items = stp_ob$filter_items %>% # TODO: add "|" that if strings are "" function... is.null.. is.na or "", then they are saved as empty character
    add_row(
      df_name = if_else(is.null(df_name), character(), df_name),
      id_vars = if_else(is.null(id_vars), list(), id_vars),
      group_by_vars = if_else(is.null(group_by_vars), list(), group_by_vars),
      keep_logic = if_else(is.null(keep_logic), character(), keep_logic),
      stp_id = stp_id,
      filter_notes = if_else(is.null(mapping_notes), character(), mapping_notes),
      update_notes = if_else(is.null(update_notes), character(), update_notes),
      highlight = if_else(is.null(highlight), FALSE, highlight),
      issue = if_else(is.null(issue), FALSE, issue),
      compare_path = if_else(is.null(compare_path), character(), compare_path),
      report = if_else(is.null(report), TRUE, report), # default to true
      filter_ob = if_else(is.null(map_ob), list(), map_ob),
      update_ob = if_else(is.null(map_diff), list(), map_diff),
      has_update_ob = has_update_ob,
      timestamp = lubridate::now()
    )

  # TODO: need to add path check
  saveRDS(stp_ob, file.path(project_directory, stp_project_dictionary$current_metadata_path))

}


# maybe update this so every time it is reading it from the path.expand

# first tries to read it from the path

# if not there, attempts to read from the parent environment (+ warning)

# if reads from the path, then necessarily writes to the path

# if reads from the environment, then gives warning, and does not write to the path ... a lot more

# have dictionary object to suppress warnings about modifying the object in the parent environment? (this would be to limit all the read/write tot he disk for each function call...)



