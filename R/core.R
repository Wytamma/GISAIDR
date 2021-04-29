
GISAID_URL = "https://www.epicov.org/epi3/frontend"

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

createUrlData <- function(sid, wid, pid, queue, ts, mode = 'ajax') {
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
    ts,
    "&mode=",
    mode
  )
  return(data)
}
