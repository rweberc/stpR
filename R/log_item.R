

#'
#' @return
#'
#' @export
#'
log_item <- function(type, # TODO: I think there is a way to specify limited set of acceptable values for function arguments, right?
                     # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object... do id's need to be unique among these?
                     item,
                     add_item = NULL, # for now, this is to encompass the assert_distinct, assert_cases scenarios... but right now only do the duplicate checks by "item"... some other work needs to be done here
                     priority = NULL,
                     stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                     project_dictionary = get_project_dictionary(),
                     project_directory = here::here()
                     )
{

  # TODO: need to confirm existence of fields like, type and item

  # TODO: check for existence of stp_ob (when update = TRUE)

  # TODO: check stp_ob is well-formed (no NAs for ids)

  # TODO: check for existence of row (only allow for complete overwrite or addition?)

  # Try to read current mapping
  stp_ob = get_stp_object(project_dictionary,
                          dir = project_directory,
                          file_type = "current")

  type <- stringr::str_to_upper(type)

  if (!(type %in% c("COMMENT", "TODO", "ALERT")))
    stop("Invalid type.")

  if (is.null(stp_id))
    stp_id = format(Sys.time(), "%H%M%OS3")

  # check if stp_ob currently has a row for this id
  existing_rows = stp_ob$text_items %>%
    dplyr::filter(stp_id == stp_id | item == item) %>% # currently... id is read regardless of type...
    dplyr::nrow()

  # TODO: perhaps have function to do all things like checking for duplicates by id, etc.

  if (existing_rows > 0) {

    if (project_dictionary$allow_overwrite_artifacts_global == FALSE) { # TODO: again, consider get() function

      warning("Rows exist for ... but allow_overwrite_artifacts_global is set to FALSE.")

      # Have to say if existing row is due to id (unlikely if automated) or if duplicated item...
      #  very possible that could have a "duplicated" item that was just lazily the same todo/comment... in this case, might want to give a different warning about the overwrite

      return() # TODO: likely need to update return statement

    } else {

      # if overwrite allows, removed existing row before making update
      stp_ob$text_items = stp_ob$text_items %>%
        dplyr::filter(stp_id != stp_id & item != item) # TODO: again, doing these two compares at once and not respecting item types

    }
  }

  stp_ob$text_items = stp_ob$text_items %>% # TODO: add "|" that if strings are "" function... is.null.. is.na or "", then they are saved as empty character
    add_row(
      type = type, # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object...
      stp_id = stp_id,
      item = item,
      add_item = if_else(is.null(add_item), character(), add_item),
      priority = if_else(is.null(priority), character(), priority), # NA for comments
      timestamp = lubridate::now()
    )

  # TODO: need to add path check
  saveRDS(stp_ob, file.path(project_directory, project_dictionary$current_metadata_path))

}

