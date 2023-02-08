#' @export
get_setting <- function(ip = "192.168.54.1", setting, timeout = 5) {
  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=getsetting&type=", setting),
                  timeout = timeout),
    error = function(e) NA
  )

  if (is.na(out)) {
    stop("No camera could be found at this address.")
  } else {
    test <- grepl("<result>ok</result>", out)

    if (!test) {
      stop("Unknown setting.")
    } else {
      out <- xml2::as_list(xml2::read_xml(out))
      attr(out$camrply$settingvalue, setting)
    }
  }
}


#' @export
set_setting <- function(ip = "192.168.54.1", setting, value, timeout = 5) {
  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=setsetting&type=", setting, "&value=", value),
                  timeout = timeout),
    error = function(e) NA
  )

  if (is.na(out)) {
    stop("No camera could be found at this address.")
  } else {
    test <- grepl("<result>ok</result>", out)

    if (!test) {
      stop("Unknown setting or incompatible setting value.")
    } else {
      message("Setting successfully modified.")
      TRUE
    }
  }
}
