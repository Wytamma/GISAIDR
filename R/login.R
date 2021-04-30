parseResponse <- function(res) {
  j = httr::content(res, as = 'parsed')
  if (isTRUE(grep('Error', j$responses[[1]]$data) == 1)) {
    # make a better check
    warning(j$responses[[1]]$data)
    stop("Login failed")
  }
  if (isTRUE(grep('showMessage', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop("Username or password wrong!")
  }
  return(j)

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
  j <- parseResponse(res)
  PID <-
    substr(j$responses[[1]]$data, 13, nchar(j$responses[[1]]$data) - 2)

  # get CID
  res <- httr::GET(paste0(GISAID_URL, '?sid=', SID, '&pid=', PID))
  t = httr::content(res, as = 'text')
  CID <-
    regmatches(t,
               regexpr(
                 "sys-actionbar-action\" onclick=\"sys.getC\\('([^']*)",
                 t,
                 perl = TRUE
               ))
  CID <- strsplit(CID, "sys.getC\\(\'")[[1]][[2]]

  ev <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'Go',
    params = list(link = 'page_corona2020.PartnerDownloadsPage')
  )

  json_queue <- list(queue = list(ev))

  data <- createUrlData(SID, WID, PID, json_queue, ts)
  res <- httr::GET(paste0(GISAID_URL, '?', data))
  j <- parseResponse(res)
  PID <-
    strsplit(j$responses[[1]]$data, "'")[[1]][4]

  # get genmoic epi cid
  res <- httr::GET(paste0(GISAID_URL, '?sid=', SID, '&pid=', PID))
  t = httr::content(res, as = 'text')
  CID <-
    regmatches(t,
               regexpr("sys.call\\('(.*)','GoAugur", t, perl = TRUE))
  CID <- strsplit(CID, "'")[[1]][2]

  # data: {"queue":[{"wid":"wid_qsbxxp_1r0u","pid":"pid_qsbxxp_1r0v","cid":"c_qsbxxp_vf","cmd":"GoAugur","params":{},"equiv":null}]}
  ev <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'GoAugur',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev))
  data <- createUrlData(SID, WID, PID, json_queue, ts)

  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  j <- parseResponse(res)
  PID <-
    strsplit(j$responses[[3]]$data, "'")[[1]][4]

  # get search CID
  res <- httr::GET(paste0(GISAID_URL, '?sid=', SID, '&pid=', PID))
  t = httr::content(res, as = 'text')
  CID <-
    regmatches(t,
               regexpr("div class=\"sys-datatable\" id=\"(.*)_table", t, perl = TRUE))
  CID <- strsplit(CID, " id=\"")[[1]][[2]]
  search_cid <- substr(CID, 0, nchar(CID) - 6)

  # get download cid


  credentials <-
    list(
      pid = PID,
      sid = SID,
      wid = WID,
      search_cid = search_cid,
      download_cid = search_cid
    )
  if (!all(unlist(sapply(credentials, function(x)
    isTRUE(nchar(x) != 0))))) {
    stop("Login failed")
  }
  return(credentials)
}
