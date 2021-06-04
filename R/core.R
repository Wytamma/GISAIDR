


GISAID_URL = "https://www.epicov.org/epi3/frontend"

headers = c(accept = "application/json, text/javascript, */*; q=0.01",
            "content-type" = "application/x-www-form-urlencoded; charset=UTF-8")

timestamp <- function() {
  return(as.character(as.integer(Sys.time()) * 1000))
}

createCommand <-
  function(wid,
           pid,
           cid,
           cmd,
           params = setNames(list(), character(0)),
           equiv = NULL) {
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

createUrlData <-
  function(sid, wid, pid, queue, timestamp, mode = 'ajax') {
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
  if (length(j$responses) == 0 & length(j) == 2) {
    print(j)
    stop("incorrect PID?")
  }
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

resetQuery <- function(credentials) {
  queue = list()
  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = "Reset"
  )
  queue <- append(queue, list(command))
  command_queue <- list(queue = queue)
  data <-
    createUrlData(
      sid = credentials$sid,
      wid = credentials$wid,
      pid = credentials$pid,
      queue = command_queue,
      timestamp = timestamp()
    )
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
}
