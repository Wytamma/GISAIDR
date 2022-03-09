test_that("createCommand returns list", {
  expect_true(is.list(createCommand(1,2,3,"Go", list())))
})


test_that("formatDataForRequest returns str", {
  expect_true(is.character(formatDataForRequest(1,2,3, list(), 1)))
})
