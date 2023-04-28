test_that("login returns credentials", {
  username = Sys.getenv("GISAIDR_USERNAME")
  password = Sys.getenv("GISAIDR_PASSWORD")
  credentials <- login(username = username, password = password)
  expect_true(is.list(credentials))

})

test_that("invalid login fails", {
  username = 'NOT_A_REAL_PERSON'
  password = Sys.getenv("GISAIDR_PASSWORD")
  expect_error(login(username = username, password = password),
               "Username or password wrong!")
})

test_that("EpiPox login works", {
  username = Sys.getenv("GISAIDR_USERNAME")
  password = Sys.getenv("GISAIDR_PASSWORD")
  credentials <- login(username = username, password = password, database="EpiPox")
  expect_true(is.list(credentials))
})


test_that("EpiRSV login works", {
  username = Sys.getenv("GISAIDR_USERNAME")
  password = Sys.getenv("GISAIDR_PASSWORD")
  credentials <- login(username = username, password = password, database="EpiRSV")
  expect_true(is.list(credentials))
})
