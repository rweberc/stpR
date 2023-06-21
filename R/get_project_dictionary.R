#' First attempts to read the project_dictionary file from the current parent environment;
#' failing that will attempt to read the file from a path, with the default path being at the root level of a project for a file specifically named stpr_project_dictionary
#'
#'
#' @return
#'
#' @export
#'
get_project_dictionary <- function(filename = "project_dictionary.yaml", dir = here::here()) {

  # TODO: Add warning that it's needed and suggestion, if not available, use x() to create it

  # Attempt to read the dictionary from the path
  project_dictionary = yaml::read_yaml(file.path(dir, filename))

  # TODO: add in validation for dictionary object

  return(project_dictionary)

}
