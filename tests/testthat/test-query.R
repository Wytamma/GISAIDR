test_that("basic query works", {
  # will break if log in fails...
  credentials <- login(username=Sys.getenv("GISAIDR_USERNAME"), password=Sys.getenv("GISAIDR_PASSWORD"))

  ev <- create_command(wid = credentials$WID,
                       pid = credentials$PID,
                       cid = 'c_qs8mrs_pj',
                       cmd = 'GetData',
                       params = setNames(list(), character(0))) #hack for empty {}

  json_queue <- list(queue=list(ev))
  ts = as.character(as.integer(Sys.time()) * 1000)
  data <- create_url_data(credentials$SID, credentials$WID, credentials$PID, json_queue, ts)
  res <- httr::GET(paste0(GISAID_URL, '?',data))
  j = httr::content(res, as='parsed')
  print(j$responses[[1]]$data)
})
