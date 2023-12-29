#' Returns stpR object.
#'
#' Utility to quickly pull stpR object from file.  Utilized by many of the strR functions for this purpose.
#'
#' desc
#'
#' details
#'
#' @param file_type The type of stpR object to return, either "current" or "compare".  Default is "current".
#' @param dir The directory to save the stp_ob object to, which by default is determined by the `here::here()` function.
#' @param project_dictionary The project_dictionary object, which by default is determined by the `get_project_dictionary()` function.
#'
#' @return stp_ob A list object that documents the current state of the project for evaluation and reporting.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' get_stp_object()
#'
#' }
get_stp_object <- function(file_type = "current",
                           dir = here::here(),
                           project_dictionary = get_project_dictionary()) {


  if (file_type == "current")
    path_to_read = file.path(dir, project_dictionary$current_metadata_path)
  else if (file_type == "compare")
    path_to_read = file.path(dir, project_dictionary$compare_metadata_path)
  else
    stop("Error: file_type '{file_type}' not valid for file_type argument." %>% glue::glue())

  stp_object = NULL

  if (file.exists(path_to_read))
    stp_object = readRDS(path_to_read)
  else {
    message("stp_ob doesn't exist at: '{path_to_read}'.  Returning empty stp_ob...") # TODO: will say this even if it's the directory that doesn't exist...
    if (usethis::ui_yeah("Save empty stp_ob at this path?" %>% glue::glue())) # TODO: add in warning that this makes a lot less sense for "compare" file types...
      stp_object = create_stp_ob(save_to_path = path_to_read)
  }

  return(stp_object)

}
