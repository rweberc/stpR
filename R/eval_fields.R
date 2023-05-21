#' Description
#'
#'
#' @param data_df
#' @param from
#' @param to
#' @param rm_std_proc
#' @param highlight
#' @param issue
#' @param summary_by
#' @param id
#' @param notes
#' @param artifact
#' @param no_report
#' @param compare
#' @param stpr_ob
#'
#' @return
#'
#' @export
#'

eval_fields <- function(data_df = NULL,
                        from = NULL,
                        to = NULL,
                        rm_std_proc = NULL,
                        highlight = FALSE,
                        issue = FALSE,
                        summary_by = NULL,
                        notes = NULL,
                        id = NULL, # default to to if singular= to,
                        artifact = TRUE,
                        no_report = FALSE,
                        compare = TRUE,
                        stpr_project_dictionary = get_project_dictionary(stpr_project_dictionary),
                        stpr_compare_object = get_compare_object(stpr_compare_object, stpr_project_dictionary) # if null and compare = TRUE, expects dictionary object to make it possible to read the compare file, if neither true, throws error
                        # implicitly, reads the stpr object from the parent environment if writing or reading from it
                        )
{

  #  select to from/to fields



  # if rm_std_proc, do "std" processing
  #  only show cases that don't follow the standard processing

  # create mapping object
  #   - group by from and to + count
  # if outputting... output this here separately from compare?

  ## If writing or comparing: DETERMINE FIELD ID (only need if going to save out)

  # decide on id
  #  - take to fields and id field
  #  - if id field is specified, use that
  #  - if id field is not specified and to is length 1, use that
  #  - else error

  # - write relevant ref_ob to object... (have to decide if it should be overwriting based on existing id
  #   - dataframe name
  #   - highlight
  #   - issue
  #   - notes
  #   - timestamp
  #   - compare
  #   - no_report
  #   - mapping
  #   - this is what gets saved out

  # - the compare:
  #     - if doing - read in old
  #     - get field of interest... need to check on existence, etc.
  #   - group by from and to + count
  #   - consider the bridge here for names if relevant...
  #   - map missing, changed, new
  #   - save out update issues
  #   - the console/output piece

}


get_project_dictionary <- function(stpr_project_dictionary, path = file.path(here::here(), "stpr_project_dictionary")) {

  if (is.null(stpr_project_dictionary)) {
    # try to read the dictionary from the path:
    stpr_project_dictionary = yaml.load_file(path)
  }

  return(stpr_project_dictionary)

}


get_compare_object <- function(stpr_compare_object, stpr_project_dictionary, dir = here::here()) {

  if (is.null(stpr_compare_object)) {
    # try to go to stpr_project_dictionary to get compare path to read this object
    stpr_compare_object = readRDS(file.path(dir, stpr_project_dictionary$compare_metadata_filenames))
  }

  return(stpr_compare_object)

}


