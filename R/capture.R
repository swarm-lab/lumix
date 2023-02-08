#' @export
snap <- function(ip = "192.168.54.1", timeout = 5) {
  message("Snapping a picture. Please wait.")

  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=capture"),
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
      message("Picture snapped.")
      TRUE
    }
  }
}


#' @export
start_recording <- function(ip = "192.168.54.1", timeout = 5) {
  message("Starting recording. Please wait.")

  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=video_recstart"),
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
      message("Recording started.")
      TRUE
    }
  }
}


#' @export
stop_recording <- function(ip = "192.168.54.1", timeout = 5) {
  message("Stopping recording. Please wait.")

  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=video_recstop"),
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
      message("Recording stopped.")
      TRUE
    }
  }
}


#' @export
start_streaming <- function(ip = "192.168.54.1", udp = 49152, timeout = 5) {
  message("Starting streaming. Please wait.")

  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=startstream&value=", udp),
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
      message("Streaming started.")
      TRUE
    }
  }
}


#' @export
stop_streaming <- function(ip = "192.168.54.1", timeout = 5) {
  message("Stopping streaming. Please wait.")

  out <- tryCatch(
    RCurl::getURL(paste0("http://", ip, "/cam.cgi?mode=stopstream"),
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
      message("Streaming stopped.")
      TRUE
    }
  }
}


