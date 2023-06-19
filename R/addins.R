#' exec_view
#'
#' Execute and view highlighted text.  Wrapper for View().
#'
#' rstudio.prefs::use_rstudio_keyboard_shortcut("Ctrl+Shift+W" = "AmplioHelpers::exec_view")
#'
#' @export
#'

exec_view <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  # for (con in rev(context$selection[[1]]$text)) { # Could try to make it possible to execute/view multiple cursors... here will only take the first one.
  # text_to_send <- paste0("View(", con, ")")
  text_to_send <- paste0("View(", context$selection[[1]]$text, ")")
  rstudioapi::sendToConsole(text_to_send, execute = TRUE) # Should add try-catch
  # }
}
