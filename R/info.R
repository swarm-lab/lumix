#' @export
get_info <- function(ip = "192.168.54.1", setting, timeout = 5) {
  out <- tryCatch(
    httr::GET(paste0("http://", ip, "/cam.cgi?mode=getinfo&type=", setting),
              httr::timeout(timeout)),
    error = function(e) NA
  )

  if (inherits(out, "response")) {
    test <- grepl("<result>ok</result>", suppressMessages(httr::content(out)))
    if (!test) {
      message("Unknown setting.")
      test
    } else {
      out <- xml2::as_list(xml2::read_xml(out))
      out # attr(out$camrply$settingvalue, setting)
    }
  } else {
    message("The setting value could not be retrieved. Connection timeout.")
    FALSE
  }
}
