


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

get_selection_panel <- function(session_id, WID, customSearch_page_ID, query_cid) {
  # selection changes every time you open it
  selection_command <- createCommand(
    wid = WID,
    pid = customSearch_page_ID,
    cid = query_cid,
    cmd = 'Selection',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(selection_command))

  data <-
    formatDataForRequest(session_id, WID, customSearch_page_ID, queue, timestamp())

  response <-
    send_request(method='POST', data=data)

  response_data <- parseResponse(response)
  # extract PID
  # selection changes every time
  selection_pid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][4]
  selection_wid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][2]
  log.debug(sprintf("get_selection_panel (response_data): %s", response_data))
  list(pid=selection_pid, wid=selection_wid)
}

get_download_panel <- function(session_id, WID, customSearch_page_ID, query_cid) {
  # selection changes every time you open it
  selection_command <- createCommand(
    wid = WID,
    pid = customSearch_page_ID,
    cid = query_cid,
    cmd = 'DownloadAllSequences',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(selection_command))

  data <-
    formatDataForRequest(session_id, WID, customSearch_page_ID, queue, timestamp())

  response <-
    send_request(method='POST', data=data)

  response_data <- parseResponse(response)
  log.debug(sprintf("get_download_panel_pid_wid (response_data): %s", response_data))
  # extract PID
  # selection changes every time
  download_pid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][4]
  download_wid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][2]

  list(pid=download_pid, wid=download_wid)
}

send_back_cmd <- function(session_id, WID, PID, CID ) {
  # send back command to get back to page
  # {"queue":[{"wid":"wid_r8fuui_7jgp","pid":"pid_r8fuui_7jgq","cid":"c_r8fuui_3uj","cmd":"Back","params":{},"equiv":null}]}
  selection_command <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'Back',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(selection_command))

  data <-
    formatDataForRequest(session_id, WID, PID, queue, timestamp())

  response <-
    send_request(method='POST', data=data)

  response_data <- parseResponse(response)
}

select_entries <- function(credentials, list_of_accession_ids) {
  accession_ids_string <- paste(list_of_accession_ids, collapse=", ")

  selection_pid_wid <- get_selection_panel(credentials$sid, credentials$wid, credentials$pid, credentials$query_cid)

  #load panel
  selection_page <-
    send_request(paste0('sid=', credentials$sid, '&pid=', selection_pid_wid$pid))

  ev1 <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'setTarget',
    params = list(cvalue=accession_ids_string, ceid=credentials$selection_ceid), #hack for empty {}
    equiv = paste0("ST", credentials$selection_ceid)
  )

  ev2 <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'ChangeValue',
    params = list(cvalue=accession_ids_string, ceid=credentials$selection_ceid), #hack for empty {}
    equiv = paste0("CV", credentials$selection_ceid)
  )
  ev3 <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'OK',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev1, ev2, ev3))
  data <- formatDataForRequest(credentials$sid, selection_pid_wid$wid, selection_pid_wid$pid, json_queue, timestamp())
  response <-
    send_request(method='POST', data=data)
  response_data <-parseResponse(response)
  log.debug(response_data)
  if (isTRUE(grep('Back', response_data$responses[[2]]$data) == 1)) {
    send_back_cmd(credentials$sid, selection_pid_wid$wid, selection_pid_wid$pid, credentials$selection_panel_cid)
  }

  return(response)
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
  regex <- paste0(".createFI\\('(.*)','.*Widget','", identifier)
  log.debug(sprintf("Extracting '%s' from '%s'", regex, substr(t, 0, 30)))
  ceid <-
    regmatches(t,
               regexpr(
                 regex,
                 t,
                 perl = TRUE
               ))
  ceid <- strsplit(ceid, "'")
  ceid <- ceid[[1]][length(ceid[[1]]) - 4]
  return(ceid)
}

log.debug <- function(msg) {
  if (Sys.getenv("GISAIDR_DEBUG") == 1) {
    message(msg)
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
  log.debug(sprintf("Extracting '%s' from '%s'", regex, substr(text, 0, 30)))
  matches <- regmatches(text, regexec(regex, text))
  return(matches[[1]][[2]])
}


go_to_page <- function(session_id, WID, PID, CID, link) {
  go_command <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'Go',
    params = list(link = link)
  )

  queue <- list(queue = list(go_command))

  data <-
    formatDataForRequest(session_id, WID, PID, queue, timestamp())

  response <- send_request(data)
  response_data <- parseResponse(response)
  return(response_data)
}


Variants <-
  list(
    alpha = 'B.1.1.7 / Q.*',
    beta = 'B.1.351 / B.1.351.2 / B.1.351.3',
    gamma = 'P.1 / P.1.*',
    delta = 'B.1.617.2 / AY.*',
    epsilon = 'B.1.427 / B.1.429',
    eta = 'B.1.525',
    iota = 'B.1.526',
    kappa = 'B.1.617.1',
    lambda = 'C.37 / C.37.1',
    mu = 'B.1.621 / B.1.621.1',
    omicron = 'B.1.1.529 / BA.*',
    GH_490R = 'B.1.640 / B.1.640.*'
  )
