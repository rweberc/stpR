#'
#' User is calling this function create rmarkdown
#'
#' @export
#'
create_stp_markdown <- function(include_reference_object = TRUE,
                                include_update_object = FALSE,
                                limit_to_highlight = FALSE,
                                limit_to_issue = FALSE,
                                rmarkdown_path,  # if null, stored in same directory as current file
                                project_dictionary = get_project_dictionary(),
                                project_directory = here::here()) {


  # Create output file path:
  if (is.null(rmarkdown_path))
    rmarkdown_path = file.path(project_directory,
                               str_extract(project_dictionary$current_metadata_path, "(?<=/)[^/]*$")) # This part is just taking the path of the current stp_ob.rds object and writing there... this should be re-thought, but see comment above on decision to expect file structure or not


  # Sections needed in the rmarkdown

  headerTemplate <- '
  ---
  output: html_document
  ---

  ```{{r, include = FALSE}}
  library(tidyverse)

  options(htmlwidgets.TOJSON_ARGS = list(na = "string")) # option_chunk$set did not work, ideas why?

  stp_object = readRDS(file.path({{project_directory}}, {{project_dictionary$current_metadata_filenames}}))

  reporting_object <- get_reporting_cases(stp_object,
                                          limit_to_highlight,
                                          limit_to_issue)
  ```
  '



  iterate_reference_strings <- ""

  # include_reference_object and include_update_object
  # for-loop
  # Section that outputs header/desc/dataframe

  for (i in seq_len(nrow(reporting_object))) {

    item_reference_string <- ""

    reporting_object_row <- reporting_object %>%
      slice(i)

    # TODO: need to add in value checks for null items
    item_header <- reporting_object_row %>%
      pull(stp_id)

    item_desciption <- reporting_object_row %>%
      pull(notes)

    has_update_ob <- reporting_object_row %>%
      pull(has_update_ob)


    # item header text
    item_reference_string <- '
    ## {item_header}
    ' %>% glue::glue()

    # item description text
    if (!is.null(item_desciption) &&
        item_desciption != "") {

      item_reference_string <- paste0(item_reference_string, '
      item_desciption
      ')
    }


    # ref ob
    # TODO: add in conditions: nrow(data_rds) == 0 ["*No issues* (column names pasted...)"]; all(is.na(data_rds)) ["*All values NA* (column names pasted)"]
    if (include_reference_object) {
      ref_text <- '
      ```{{r}}
      ref_ob <- reporting_object %>%
      slice(i) %>%
      pull(ref_ob)

      if (nrow(ref_ob) <= 10) {{

        ref_ob %>%
          DT::datatable(rownames = FALSE,
                        options = list(dom = "t"))

      }} else {{

        ref_ob %>%
          DT::datatable(rownames = FALSE)

      }}
      ```'

      item_reference_string <- paste0(item_reference_string, ref_text)
    }

    # update ob
    if (include_update_object) {

      update_text <- '
      ```{{r}}
      update_ob <- reporting_object %>%
      slice(i) %>%
      pull(update_ob)

      if (nrow(update_ob) <= 10) {{

        update_ob %>%
          DT::datatable(rownames = FALSE,
                        options = list(dom = "t"))

      }} else {{

        update_ob %>%
          DT::datatable(rownames = FALSE)

      }}
      ```'

      item_reference_string <- paste0(item_reference_string, update_text)
    }

    # cat the file to the rmarkdown_path
    cat(item_reference_string, rmarkdown_path)
  }

}



get_reporting_cases = function(stp_ob,
                               limit_to_highlight = FALSE,
                               limit_to_issue = FALSE) {

  output_ob = stp_ob$mapping_items %>%
    filter(report == 1) %>%
    select(stp_id,
           notes,
           highlight,
           issue,
           report,
           ref_ob,
           update_ob,
           has_update_ob) %>%
    mutate(reference_object_row = 1) %>%
    bind_rows(stp_ob$filter_items %>%
                filter(report == 1) %>%
                select(stp_id,
                       notes,
                       highlight,
                       issue,
                       report,
                       ref_ob,
                       update_ob,
                       has_update_ob) %>%
                mutate(filter_object_row = 1))


  if (limit_to_highlight)
    output_ob = output_ob %>%
    filter(highlight == 1)

  if (limit_to_issue)
    output_ob = output_ob %>%
    filter(issue == 1)

  return(output_ob)
}






