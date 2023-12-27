#'
#' @return
#'
#' @export
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

  # TODO: have option to read this, then make update and write it to directory, then return the copy to work from...

  # TODO: Possibly consider having default options for this... if default = TRUE... will only just fill in those values...
  #         and can have default set to TRUE by default, but in the update_xyz_object calls... would be set to FALSE to prevent writing without sufficient values...

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
  existing_rows = stp_ob$mapping_items %>%
    dplyr::filter(stp_id == stp_id_var) %>%
    nrow()

  # TODO: perhaps have function to do all things like checking for duplicates by id, etc.

  if (existing_rows > 0) {

    if (project_dictionary$allow_overwrite_artifacts_global == FALSE) { # TODO: again, consider get() function

      warning("Rows exist for ... but allow_overwrite_artifacts_global is set to FALSE.")
      return() # TODO: likely need to update return statement

    } else {

      # if overwrite allows, removed existing row before making update
      stp_ob$mapping_items = stp_ob$mapping_items %>%
        dplyr::filter(stp_id != stp_id_var)

    }
  }

  # TODO: add data-type checks for the input data?  must have stp_id_var must have a ref_ob?

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



