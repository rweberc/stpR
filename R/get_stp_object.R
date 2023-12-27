#'
#' @return
#'
#' @export
#'
get_stp_object <- function(project_dictionary = get_project_dictionary(),
                           dir = here::here(),
                           file_type = "current") {

  # TODO: if not available, warning?  add in suggestion for creating... populate with suggested path?

  # TODO: add in additional validation for files found or not...

  # TODO: consider the cases where you would and wouldn't want to handle the items not being there and what type of response should be given in each of these cases.

  if (file_type == "current")
    path_to_read = file.path(dir, project_dictionary$current_metadata_path)
  else if (file_type == "compare")
    path_to_read = file.path(dir, project_dictionary$compare_metadata_path)
  else
    stop("Error: file_type '{file_type}' not valid." %>% glue::glue())

  stp_object = NULL

  if (file.exists(path_to_read))
    stp_object = readRDS(path_to_read)
  else {
    message("stp_ob doesn't exist at: '{path_to_read}'.  Returning empty stp_ob...") # TODO: will say this even if it's the directory that doesn't exist...
    if (usethis::ui_yeah("Save empty stp_ob at this path?" %>% glue::glue())) # TODO: add in warning that this makes a lot less sense for "compare" file types...
      stp_object = create_stp_ob(save_to_path = path_to_read)
  }

  # TODO: add in validation for compare object

  return(stp_object)

}
