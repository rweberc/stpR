#' Internal utility to output data frame and messages to the console based on several conditions.
#'
#'
#' @param df The data frame to be output to the console.
#' @param header_text The text to be printed before the data frame.
#' @param cases_text The text to be printed before the data frame when there are cases to report.
#' @param no_cases_text The text to be printed when there are no cases to report.
#' @param see_returned_object_text The text to be printed when there are more than 9 rows in the data frame.

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
