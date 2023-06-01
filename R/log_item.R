

#'
#' @return
#'
#' @export
#'
log_item <- function(type, # TODO: I think there is a way to specify limited set of acceptable values for function arguments, right?
                     # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object... do id's need to be unique among these?
                     item,
                     priority = NULL,
                     stp_id = NULL, # TODO: is it a concern if id's in different stp_ob entities are the same
                     artifact = TRUE, # create an artifact to be saved out (almost always true... only false if you just wanted to use this function to do the count() type check with benefit of console output control)
                     project_dictionary = get_project_dictionary(),
                     project_directory = here::here()
                     )
{

  # TODO: need

  # TODO: check for existence of stp_ob (when update = TRUE)

  # TODO: check stp_ob is well-formed (no NAs for ids)

  # TODO: check for existence of row (only allow for complete overwrite or addition?)

  # Try to read current mapping
  stp_ob = get_stp_object(stp_project_dictionary,
                          dir = project_directory,
                          file_type = "current")


  time <-

  type <- stringr::str_to_upper(type)

  if (!(type %in% c("COMMENT", "TODO", "ALERT")))
    stop("Invalid type.")


  if (is.null(stp_id))
    stp_id = format(Sys.time(), "%H%M%OS3")

  # check if stp_ob currently has a row for this id
  existing_rows = stp_ob$text_items %>%
    dplyr::filter(id == stp_id | item == item) %>% # currently... id is read regardless of type...
    dplyr::nrow()

  # TODO: perhaps have function to do all things like checking for duplicates by id, etc.

  if (existing_rows > 0) {

    if (stp_project_dictionary$allow_overwrite_artifacts_global == FALSE) { # TODO: again, consider get() function

      warning("Rows exist for ... but allow_overwrite_artifacts_global is set to FALSE.")
      return() # TODO: likely need to update return statement

    } else {

      # if overwrite allows, removed existing row before making update
      stp_ob$filter_items = stp_ob$filter_items %>%
        dplyr::filter(id != stp_id)

    }
  }

  stp_ob$text_items = stp_ob$text_items %>% # TODO: add "|" that if strings are "" function... is.null.. is.na or "", then they are saved as empty character
    add_row(
      type = character(), # comment, todo, alert # TODO: consier if "errors/alerts" should be in a separate data object...
      id = character(),
      item = character(),
      priority = character(), # NA for comments
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



