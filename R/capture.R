#' @export
snap <- function(ip = "192.168.54.1", timeout = 5) {
  message("Snapping a picture. Please wait.")

  out <- tryCatch(
    httr::GET(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=capture"),
              httr::timeout(timeout)),
    error = function(e) NA
  )

  if (inherits(out, "response")) {
    test <- grepl("<result>ok</result>", suppressMessages(httr::content(out)))
    if (!test) {
      message("A picture could not be snapped. Connection error.")
    } else {
      message("Picture snapped.")
    }
    test
  } else {
    message("A picture could not be snapped. Connection timeout.")
    FALSE
  }
}


#' @export
start_recording <- function(ip = "192.168.54.1", timeout = 5) {
  message("Starting recording. Please wait.")

  out <- tryCatch(
    httr::GET(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=video_recstart"),
              httr::timeout(timeout)),
    error = function(e) NA
  )

  if (inherits(out, "response")) {
    test <- grepl("<result>ok</result>", suppressMessages(httr::content(out)))
    if (!test) {
      message("The recording could not be started. Connection error.")
    } else {
      message("Recording started.")
    }
    test
  } else {
    message("The recording could not be started. Connection timeout.")
    FALSE
  }
}


#' @export
stop_recording <- function(ip = "192.168.54.1", timeout = 5) {
  message("Stopping recording. Please wait.")

  out <- tryCatch(
    httr::GET(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=video_recstop"),
              httr::timeout(timeout)),
    error = function(e) NA
  )

  if (inherits(out, "response")) {
    test <- grepl("<result>ok</result>", suppressMessages(httr::content(out)))
    if (!test) {
      message("The recording could not be stopped. Connection error.")
    } else {
      message("Recording stopped.")
    }
    test
  } else {
    message("The recording could not be stopped. Connection timeout.")
    FALSE
  }
}


#' @export
start_streaming <- function(ip = "192.168.54.1", udp = 49152, timeout = 5) {
  message("Starting streaming. Please wait.")

  out <- tryCatch(
    httr::GET(paste0("http://", ip, "/cam.cgi?mode=startstream&value=", udp),
              httr::timeout(timeout)),
    error = function(e) NA
  )

  if (inherits(out, "response")) {
    test <- grepl("<result>ok</result>", suppressMessages(httr::content(out)))
    if (!test) {
      message("The stream could not be started. Connection error.")
    } else {
      message("Stream started.")
    }
    test
  } else {
    message("The stream could not be started. Connection timeout.")
    FALSE
  }
}


#' @export
stop_streaming <- function(ip = "192.168.54.1", timeout = 5) {
  message("Stopping streaming. Please wait.")

  out <- tryCatch(
    httr::GET(paste0("http://", ip, "/cam.cgi?mode=stopstream"),
              httr::timeout(timeout)),
    error = function(e) NA
  )

  if (inherits(out, "response")) {
    test <- grepl("<result>ok</result>", suppressMessages(httr::content(out)))
    if (!test) {
      message("The stream could not be stopped. Connection error.")
    } else {
      message("Stream started.")
    }
    test
  } else {
    message("The stream could not be stopped. Connection timeout.")
    FALSE
  }
}
