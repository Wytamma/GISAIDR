
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

  data <- createUrlData(SID, WID, PID, json_queue, timestamp())

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

  data <- createUrlData(SID, WID, PID, json_queue, timestamp())
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

  # go to custom search
  ev <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'GoAugur',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev))
  data <- createUrlData(SID, WID, PID, json_queue, timestamp())

  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  j <- parseResponse(res)
  PID <-
    strsplit(j$responses[[3]]$data, "'")[[1]][4]

  # get query CID
  res <- httr::GET(paste0(GISAID_URL, '?sid=', SID, '&pid=', PID))
  t = httr::content(res, as = 'text')
  CID <-
    regmatches(t,
               regexpr("div class=\"sys-datatable\" id=\"(.*)_table", t, perl = TRUE))
  CID <- strsplit(CID, " id=\"")[[1]][[2]]
  CID <- substr(CID, 0, nchar(CID) - 6)
  query_cid <- CID

  # Search
  search_CID <- regmatches(t, regexpr("sys.createComponent\\('(.*)','Corona2020ToolSearchComponent'", t, perl = TRUE))
  search_CID <- strsplit(search_CID, "'")
  search_CID <- search_CID[[1]][2]


  # Location
  location_CID <- regmatches(t, regexpr(".createFI\\('(.*)','EntryWidget','covv_location',", t, perl = TRUE))
  location_CID <- strsplit(location_CID, "'")
  location_CID <- location_CID[[1]][length(location_CID[[1]]) - 5]


  # send selection command
  ev <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'Selection',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev))
  data <- createUrlData(SID, WID, PID, json_queue, timestamp())

  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  j <- parseResponse(res)

  # extract PID
  selection_PID <-
    strsplit(j$responses[[1]]$data, "'")[[1]][4]

  #load panel
  res <- httr::GET(paste0(GISAID_URL, '?sid=', SID, '&pid=', selection_PID))
  t = httr::content(res, as = 'text')

  # extract cids
  CID <-
    regmatches(t,
               regexpr("onselect=\"sys.getC\\('([^']*)'\\).getFI\\('([^']*)'\\).onSelect()", t, perl = TRUE))
  panel_CID <- strsplit(CID, "'")[[1]][[2]]
  selection_CID <- strsplit(CID, "'")[[1]][[4]]

  credentials <-
    list(
      pid = PID,
      sid = SID,
      wid = WID,
      query_cid = query_cid,
      panel_CID = panel_CID,
      selection_PID=selection_PID,
      selection_CID = selection_CID,
      download_cid = query_cid,
      location_CID = location_CID,
      search_CID = search_CID
    )
  if (!all(unlist(sapply(credentials, function(x)
    isTRUE(nchar(x) != 0))))) {
    stop("Login failed")
  }
  return(credentials)
}
