


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

formatDataForRequest <-
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
  j = httr::content(res, as = 'parsed')
  if (length(j$responses) == 0 & length(j) == 2) {
    warning("There was an error please see: https://github.com/Wytamma/GISAIDR/issues/1")
    stop("Error! Please login again.")
  }
  if (isTRUE(grep('Error', j$responses[[1]]$data) == 1)) {
    warning(utils::URLdecode(strsplit(j$responses[[1]]$data, '"')[[1]][2]))
    stop("Internal server Error.")
  }
  if (isTRUE(grep('expired', j$responses[[1]]$data) == 1)) {
    stop("The session has expired. Please login again.")
  }
  if (isTRUE(grep('password', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop("Username or password wrong!")
  }
  if (isTRUE(grep('No data.', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop("No data found.")
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
    formatDataForRequest(
      sid = credentials$sid,
      wid = credentials$wid,
      pid = credentials$pid,
      queue = command_queue,
      timestamp = timestamp()
    )
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
}

extract_search_ceid <- function(identifier, t) {
  ceid <-
    regmatches(t,
               regexpr(
                 paste0(".createFI\\('(.*)','.*Widget','", identifier),
                 t,
                 perl = TRUE
               ))
  ceid <- strsplit(ceid, "'")
  ceid <- ceid[[1]][length(ceid[[1]]) - 4]
  return(ceid)
}

log.debug <- function(msg) {
  if (Sys.getenv("GISAIDR_DEBUG") == 1) {
    message(msg, appendLF=FALSE)
  }
  invisible()
}

send_request <-
  function(parameter_string = "",
           data = NULL,
           method = 'GET') {
    URL <- paste0(GISAIDR::GISAID_URL, '?', parameter_string)
    if (is.null(data)) {
      data <- ""
    }
    log.debug(sprintf("Sending request:\n Method -> %s\n URL -> %s\n data -> %s", method, URL, data))
    if (method == 'GET') {
      response <- httr::GET(URL)
    } else if (method == 'POST') {
      response <-
        httr::POST(URL, httr::add_headers(.headers = GISAIDR::headers), body = data)
    } else {
      stop(sprintf("Method '%s' not allowed", method))
    }
    if (response$status_code >= 500) {
      warning(sprintf("An error occurred while trying to %s %s", method, URL))
      stop("Server error!")
    }
    response
  }

extract_first_match <- function(regex, text) {
  log.debug(sprintf("Extracting '%s' from '%s' [...trimmed to 30 chars]", regex, substr(text, 0, 30)))
  matches <- regmatches(text, regexec(regex, text))
  return(matches[[1]][[2]])
}
