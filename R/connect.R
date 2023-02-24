#' @export
discover <- function(timeout = 5) {
  message("Scanning for connected devices. Please wait.")
  res <- system("arp -an", intern = TRUE)
  router <- stringr::str_extract(res[1], "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")

  if (suppressWarnings(length(system("which fping", intern = TRUE))) > 0) {
    fping <- suppressWarnings(
      system(paste0("fping -g -r 1 ", router, "/24"), intern = TRUE,
             ignore.stderr = TRUE)
    )
    alive <- grepl("alive", fping)
    ips <- gsub(" is alive", "", fping[alive])
  } else if (suppressWarnings(length(system("which fping", intern = TRUE))) > 0) {
    nmap <- system(paste0("nmap -sn ", router, "-255"), intern = TRUE)
    alive <- grepl("Nmap scan report for ", nmap)
    ips <- gsub("Nmap scan report for ", "", nmap[alive])
  } else {
    root <- stringr::str_extract(router, "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.")
    ips <- parallel::mclapply(1:255, function(i) {
      ping <- system(paste0("ping -c 1 -t 1 ", root, i), intern = TRUE)
      if (any(grepl("1 packets received", ping))) {
        paste0(root, i)
      }
    }, mc.cores = if (.Platform$OS.type == "windows") 1 else parallel::detectCores())
    ips <- unlist(ips)
  }

  ips <- ips[ips != router]
  success <- connect(ip = ips, timeout = timeout)
  message("Done.")

  as.list(ips[success])
}


#' @title Connection to Lumix Camera
#'
#' @description This function establishes a connection to a Lumix camera over a
#'  WiFi network.
#'
#' @param ip
#'
#' @param timeout
#'
#' @return
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{discover}}
#'
#' @examples
#'
#' @export
connect <- function(ip = "192.168.54.1", timeout = 5) {
  message("Looking for Lumix cameras. Please wait.")

  sapply(ip, function(ip) {
    cam <- tryCatch(
      httr::GET(paste0("http://", ip, "/cam.cgi?mode=camcmd&value=recmode"),
                httr::timeout(timeout)),
      error = function(e) NA
    )

    if (inherits(cam, "response")) {
      grepl("<result>ok</result>", suppressMessages(httr::content(cam)))
    } else {
      FALSE
    }
  })
}
