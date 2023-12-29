#' Read stpR project_dictionary from environment.
#'
#' By default, attempts to read the project_dictionary file from the current parent environment.
#'
#' @param filename The name of the project_dictionary file to read.  Default and standard is "project_dictionary.yaml".
#' @param dir The directory where the project_dictionary file is located.  Default is the current working directory.
#'
#' @return project_dictionary A list object that contains the project_dictionary.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#'   get_project_dictionary()
#'
#' }
get_project_dictionary <- function(filename = "project_dictionary.yaml",
                                   dir = here::here()) {

  # TODO: Add check for existence of project_dictionary.yaml file in the current environment
  # TODO: Create function to create a dictionary object, if not already available.
  # TODO: Add in validation for dictionary object

  # Attempt to read the dictionary from the path
  project_dictionary = yaml::read_yaml(file.path(dir, filename))

  return(project_dictionary)

}
