username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)
# will break if log in fails...

test_that("basic query works", {
  ev <- create_command(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = 'c_qs8mrs_pj',
    cmd = 'GetData',
    params = setNames(list(), character(0))
  ) #hack for empty {}

  json_queue <- list(queue = list(ev))
  ts = as.character(as.integer(Sys.time()) * 1000)
  data <-
    create_url_data(
      sid = credentials$sid,
      wid = credentials$wid,
      pid = credentials$pid,
      queue = json_queue,
      ts = ts
    )
  res <- httr::GET(paste0(GISAID_URL, '?', data))
  j = httr::content(res, as = 'parsed')
  df <- data.frame(do.call(rbind, j$records))
  expect_true(is.data.frame(df))

})
