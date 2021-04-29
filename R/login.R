#' Login to GISAID
GISAID_URL = "https://www.epicov.org/epi3/frontend"

create_command <-
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

create_url_data <- function(sid, wid, pid, queue, ts, mode = 'ajax') {
  data <- paste0(
    "sid=",
    sid,
    "&wid=",
    wid,
    "&pid=",
    pid,
    "&data=",
    URLencode(rjson::toJSON(queue), reserved = TRUE),
    "&ts=",
    ts,
    "&mode=",
    mode
  )
  return(data)
}

extract_PID_from_res <- function(res) {
  j = httr::content(res, as = 'parsed')
  j$responses[[1]]$data
  PID <-
    substr(j$responses[[1]]$data, 13, nchar(j$responses[[1]]$data) - 2)
  return(PID)
}

login <- function(username, password) {
  # get a session ID
  res <- httr::GET(GISAID_URL)
  t = httr::content(res, as = 'text')
  SID <- regmatches(t, regexpr("name=\"sid\" value='([^']*)", t))
  SID <- strsplit(SID, "='")[[1]][[2]]
  res <- httr::GET(paste0(GISAID_URL, '?sid=', SID))
  t = httr::content(res, as = 'text')
  # extract the other IDs for log in stage 1
  WID <- regmatches(t, regexpr('WID"] = "([^"]*)', t))
  WID <- strsplit(WID, '= "')[[1]][[2]]
  PID <- regmatches(t, regexpr('PID"] = "([^"]*)', t))
  PID <- strsplit(PID, '= "')[[1]][[2]]
  CID <- regmatches(t, gregexpr('<div cid="([^"]*)', t))
  CID <- strsplit(CID[[1]][[2]], '="')[[1]][[2]]
  # create doLogin command
  ev <- create_command(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'doLogin',
    params = list(login = username, hash = openssl::md5(password))
  )

  json_queue <- list(queue = list(ev))

  ts = as.character(as.integer(Sys.time()) * 1000)

  data <- create_url_data(SID, WID, PID, json_queue, ts)

  headers = c(accept = "application/json, text/javascript, */*; q=0.01",
              "content-type" = "application/x-www-form-urlencoded; charset=UTF-8")
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  PID <- extract_PID_from_res(res)

  ev <- create_command(
    wid = WID,
    pid = PID,
    cid = 'c_qs8mrs_p5',
    cmd = 'Go',
    params = list(link = 'page_corona2020.Corona2020BrowsePage')
  )

  json_queue <- list(queue = list(ev))

  data <- create_url_data(SID, WID, PID, json_queue, ts)
  res <- httr::GET(paste0(GISAID_URL, '?', data))
  PID <- extract_PID_from_res(res)
  credentials <- list(pid = PID, sid = SID, wid = WID)
  if (!all(unlist(sapply(credentials, function(x) isTRUE(nchar(x) != 0))))) {
    stop("Login failed")
  }
  return(credentials)
}
