
GISAID_URL = "https://www.epicov.org/epi3/frontend"

headers = c(accept = "application/json, text/javascript, */*; q=0.01",
            "content-type" = "application/x-www-form-urlencoded; charset=UTF-8")

timestamp <- function() {
  return( as.character(as.integer(Sys.time()) * 1000))
}

createCommand <-
  function(wid, pid, cid, cmd, params, equiv = NULL) {
    ev = list(
      wid = wid,
      pid = pid,
      cid = cid,
      cmd = cmd,
      params = params,
      equiv = equiv
    )
    return(ev)
  }

createUrlData <- function(sid, wid, pid, queue, timestamp, mode = 'ajax') {
  data <- paste0(
    "sid=",
    sid,
    "&wid=",
    wid,
    "&pid=",
    pid,
    "&data=",
    utils::URLencode(rjson::toJSON(queue), reserved = TRUE),
    "&ts=",
    timestamp,
    "&mode=",
    mode
  )
  return(data)
}

parseResponse <- function(res) {
  if (res$status_code >= 500) {
    stop("Server error")
  }
  j = httr::content(res, as = 'parsed')
  if (isTRUE(grep('Error', j$responses[[1]]$data) == 1)) {
    # make a better check
    warning(j$responses[[1]]$data)
    stop("Login failed")
  }
  if (isTRUE(grep('expired', j$responses[[1]]$data) == 1)) {
    warning(j$responses[[1]]$data)
    stop("The session has expired. Please login again.")
  }
  if (isTRUE(grep('showMessage', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop("Username or password wrong!")
  }

  return(j)

}

