#'
#' User is calling this function create rmarkdown
#'
#' @export
#'
create_stp_markdown <- function(include_reference_object = TRUE,
                                include_update_object = FALSE,
                                limit_to_highlight = FALSE,
                                limit_to_issue = FALSE,
                                rmarkdown_path = NULL,  # if null, stored in same directory as current file
                                report_filename = "summary_report",
                                project_dictionary = get_project_dictionary(),
                                project_directory = here::here()) {


  # Create output file path:
  if (is.null(rmarkdown_path)) {

    # Regex to remove everything after the last slash from the variable project_dictionary$current_metadata_path

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

}








