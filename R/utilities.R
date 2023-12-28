
#'
#' @return
#'
#'

# Function to output dataframe to the console similar to using message() for test.  Do not use print() because it doesn't work well with tibbles.
output_data_to_console <- function(df,
                                   header_text = NULL,
                                   cases_text = NULL,
                                   no_cases_text = "No cases to report",
                                   see_returned_object_text = "See returned object for further details") {

  if (!is.null(df)) {
    if (!is.null(header_text))
      message(header_text)

    if (nrow(df) > 0) {

      if (!is.null(cases_text))
        message(cases_text)

      print(df)

      # Add in when kableExtra is added to the project
      # df %>%
      #   knitr::kable() %>%
      #   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

      # Tibble limits print to first 9 rows, so this is for that case
      if (nrow(df) > 9 & !is.null(see_returned_object_text))
        message(see_returned_object_text)

    } else {
      if (!is.null(no_cases_text))
        message(no_cases_text)
    }
  }

}
