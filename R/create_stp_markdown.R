#' Utility to create an Rmarkdown file that can be used to generate a report of the current state of the data
#'
#' Data summary created according to the current state of the stp_object.  The stp_object is read from the `current_metadata_path` in the project_dictionary.
#' The stp_object is then filtered according to the `limit_to_highlight` and `limit_to_issue` arguments, depending on what is of interest for the current report.
#' The output is also filtered to include the reference and update objects if `include_reference_object` and `include_update_object` are set to TRUE.
#' Only stp_ids that have a report = TRUE are included in the output.
#'
#' The output is a .rmd file that can be rendered to html.  If `render_markdown` is set to TRUE, an html markdown will also be rendered.
#' If rmarkdown_path is NULL, the output file is saved to the `current_report_path` from the project_dictionary.
#'
#'
#' @param include_reference_object If TRUE, the reference object is included in the output.  Default is TRUE.  The reference object is the object represents the current state of the data as output by the last run of the stpR setup function, such as `eval_map()`, `eval_summary()`, `assert_distinct()`, etc.
#' @param include_update_object If TRUE, the update object is included in the output. Default is FALSE. the update object represents differences recorded between the current state of the stp_object and the comparison object (`compare_metadata_path` in the project_dictionary), as determined by the last run of the stpR setup functions on the data setup.
#' @param limit_to_highlight If TRUE, the output is filtered to only include stp_ids that have highlight = TRUE. Default is FALSE.
#' @param limit_to_issue If TRUE, the output is filtered to only include stp_ids that have issue = TRUE. Default is FALSE.
#' @param rmarkdown_path The path to save the output rmarkdown file. If NULL, the output file is saved to the `current_report_path` from the project_dictionary.
#' @param report_filename The name of the output file, without the extension. Default is "summary_report".
#' @param render_markdown If TRUE, the output rmarkdown file is rendered to html in the same path as the .rmd file is output.
#' @param project_dictionary The project_dictionary object, which by default is output by the `get_project_dictionary()` function.
#' @param project_directory The project_directory object, which by default is output by the `here::here()` function.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' create_stp_markdown()
#'
#' }
#'
#'
create_stp_markdown <- function(include_reference_object = TRUE,
                                include_update_object = FALSE,
                                limit_to_highlight = FALSE,
                                limit_to_issue = FALSE,
                                rmarkdown_path = NULL,
                                report_filename = "summary_report",
                                render_markdown = TRUE,
                                project_dictionary = get_project_dictionary(),
                                project_directory = here::here()) {

  # Check rmarkdown_path
  # Check current_report_path..

  # Create output file path:
  if (is.null(rmarkdown_path)) {

    rmarkdown_path = file.path(project_directory,
                               project_dictionary$current_report_path,
                               glue::glue("{report_filename}.rmd"))
  }

  # Need to iterate over the reporting object in current state here as well as having that object exist within the Rmarkdown file when it is rendering
  stp_object = readRDS(file.path(project_directory, project_dictionary$current_metadata_path))

  reporting_object = get_reporting_cases(stp_object,
                                          limit_to_highlight,
                                          limit_to_issue)

  # Sections needed in the rmarkdown

  headerTemplate <- '
---
output: html_document
---

```{{r, include = FALSE}}
library(tidyverse)
library(stpR)

options(htmlwidgets.TOJSON_ARGS = list(na = "string")) # option_chunk$set did not work, ideas why?

stp_object <- readRDS(file.path("{project_directory}", "{project_dictionary$current_metadata_path}"))

reporting_object <- get_reporting_cases(stp_object,
                                        limit_to_highlight = {limit_to_highlight},
                                        limit_to_issue = {limit_to_issue})
```

' %>% glue::glue()



  iterate_reference_strings <- headerTemplate

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

    item_description <- reporting_object_row %>%
      pull(notes)

    has_update_ob <- reporting_object_row %>%
      pull(has_update_ob)


    # item header text
    item_reference_string <- '

    ## {item_header}

    ' %>% glue::glue()

    # item description text
    if (!is.null(item_description) &&
        item_description != "") {

      item_reference_string <- '{item_reference_string}
      {item_description}

      ' %>% glue::glue()
    }


    # ref ob
    # TODO: add in conditions: nrow(data_rds) == 0 ["*No issues* (column names pasted...)"]; all(is.na(data_rds)) ["*All values NA* (column names pasted)"]
    if (include_reference_object) {
      ref_text <- '

      ```{{r, echo = FALSE}}
      ref_ob <- reporting_object %>%
      slice({i}) %>%
      .[["ref_ob"]] %>%
      .[[1]]

      if (!is.null(ref_ob)) {{
        if (nrow(ref_ob) <= 10) {{

          ref_ob %>%
            DT::datatable(rownames = FALSE,
                          options = list(dom = "t"))

        }} else {{

          ref_ob %>%
            DT::datatable(rownames = FALSE)

        }}
      }}
      ```

      ' %>% glue::glue()

      item_reference_string <- paste0(item_reference_string, ref_text)
    }

    # update ob
    if (include_update_object) {

      update_text <- '

      ```{{r, echo = FALSE}}
      update_ob <- reporting_object %>%
      slice({i}) %>%
      .[["update_ob"]] %>%
      .[[1]]

      if (!is.null(update_ob)) {{
        if (nrow(update_ob) <= 10) {{

          update_ob %>%
            DT::datatable(rownames = FALSE,
                          options = list(dom = "t"))

        }} else {{

          update_ob %>%
            DT::datatable(rownames = FALSE)

        }}
      }}
      ```

      ' %>% glue::glue()

      item_reference_string <- paste0(item_reference_string, update_text)
    }

    iterate_reference_strings <- paste0(iterate_reference_strings, item_reference_string)

  }

  # cat the file to the rmarkdown_path
  cat(iterate_reference_strings, file = rmarkdown_path)

  if (render_markdown) {
    rmarkdown::render(rmarkdown_path)
  }

}








