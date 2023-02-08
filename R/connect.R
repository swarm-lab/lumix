#' @export
connect <- function(ip = "192.168.54.1", timeout = 5) {
  message("Looking for camera. Please wait.")

  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=recmode"),
                  timeout = timeout),
    error = function(e) NA
  )

  if (is.na(out)) {
    stop("No camera could be found at this address.")
  } else {
    test <- grepl("<result>ok</result>", out)

    if (!test) {
      stop("Connection error.")
    } else {
      message("Camera detected and connected.")
      TRUE
    }
  }
}
