
extract_PID_from_res <- function(res) {
  j = httr::content(res, as = 'parsed')
  j$responses[[1]]$data
  PID <-
    substr(j$responses[[1]]$data, 13, nchar(j$responses[[1]]$data) - 2)
  return(PID)
}


#' Login to GISAID
#'
#' @param username GISAID username.
#' @param password GISAID password.
#' @return credentials used to query GISAID.
#' @examples
#' username = Sys.getenv("GISAIDR_USERNAME")
#' password = Sys.getenv("GISAIDR_PASSWORD")
#' login(username, password)
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
  ev <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'doLogin',
    params = list(login = username, hash = openssl::md5(password))
  )

  json_queue <- list(queue = list(ev))

  ts = as.character(as.integer(Sys.time()) * 1000)

  data <- createUrlData(SID, WID, PID, json_queue, ts)

  headers = c(accept = "application/json, text/javascript, */*; q=0.01",
              "content-type" = "application/x-www-form-urlencoded; charset=UTF-8")
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  PID <- extract_PID_from_res(res)

  ev <- createCommand(
    wid = WID,
    pid = PID,
    cid = 'c_qs8mrs_p5',
    cmd = 'Go',
    params = list(link = 'page_corona2020.Corona2020BrowsePage')
  )

  json_queue <- list(queue = list(ev))

  data <- createUrlData(SID, WID, PID, json_queue, ts)
  res <- httr::GET(paste0(GISAID_URL, '?', data))
  PID <- extract_PID_from_res(res)
  credentials <- list(pid = PID, sid = SID, wid = WID)
  if (!all(unlist(sapply(credentials, function(x) isTRUE(nchar(x) != 0))))) {
    stop("Login failed")
  }
  return(credentials)
}
