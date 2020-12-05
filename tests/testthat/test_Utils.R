testthat::context(desc = "Testing the functions inside Utils.R...")



testthat::test_that(desc = "testing function bIsScalarOfType...", code = {
  testthat::expect_true(
    object = bIsScalarOfType(objIn = "asdf", cTypeName = "character"))
  testthat::expect_false(
    object = bIsScalarOfType(objIn = 123L, cTypeName = "character"))
  testthat::expect_error(object = bIsScalarOfType(objIn = 123L, cTypeName = 123))
  testthat::expect_false(
    object = bIsScalarOfType(objIn = rnorm(n = 5L), cTypeName = "double"))
})



testthat::test_that(desc = "testing function bIsScalarOfClass...", code = {
  expect_true(object = bIsScalarOfClass(objIn = 123.2, cClassName = "numeric"))
  expect_false(object = bIsScalarOfClass(objIn = 123.2, cClassName = "character"))
  expect_false(
    object = bIsScalarOfClass(objIn = rnorm(n = 3), cClassName = "numeric"))
  expect_error(
    object = bIsScalarOfClass(objIn = 123.33, cClassName = c("asdf", "qwery")))
})



test_that(desc = "testing function dtGenerateHourlyGridForTimeRange...", code = {
  posixctMyStartTime <-
    ISOdatetime(year = 2020, month = 9, day = 10, hour = 10, min = 0, sec = 0, tz = "Europe/Warsaw")
  posixctMyEndTime <-
    ISOdatetime(year = 2020, month = 9, day = 25, hour = 17, min = 0, sec = 0, tz = "Europe/Warsaw")
  dtRes <- dtGenerateHourlyGridForTimeRange(
    posixctStartTime = posixctMyStartTime, posixctEndTime = posixctMyEndTime)
  expect_is(object = dtRes, class = "data.table")
  expect_equal(object = nrow(x = dtRes), expected = 367L)
  expect_equal(object = ncol(x = dtRes), expected = 2L)
})



test_that(desc = "testing function posixctChangeToNewTimeZone...", code = {
  # posixctChangeToNewTimeZone(posixctIn, cNewTimeZone = "Europe/London")
  posixctTestIn <-
    ISOdatetime(year = 2020, month = 4, day = 3, hour = 16, min = 0, sec = 0, tz = "Europe/Moscow")
  expect_error(object = posixctChangeToNewTimeZone(
    posixctIn = posixctTestIn, cNewTimeZone = "qwerty_aasdf"))
  expect_error(object = posixctChangeToNewTimeZone(
    posixctIn = "asdfqwerty", cNewTimeZone = "Europe/Berlin"))
  expect_equal(
    object = posixctChangeToNewTimeZone(posixctIn = posixctTestIn,
                                        cNewTimeZone = "GMT"),
    expected = ISOdatetime(year = 2020, month = 4, day = 3, hour = 13, min = 0,
                           sec = 0, tz = "GMT"))
  expect_false(
    object = posixctChangeToNewTimeZone(posixctIn = posixctTestIn, cNewTimeZone = "GMT") ==
      ISOdatetime(year = 2020, month = 5, day = 3, hour = 13, min = 0, sec = 0, tz = "GMT"))
})



test_that(desc = "testing function lGeneralPurposeTimeHandler...", code = {
  lGeneralPurposeTimeHandler(dateStartDate, dateEndDate)

  # 1. testing for errors on incorrect input -----------------------------------
  expect_error(object = lGeneralPurposeTimeHandler(dateStartDate, dateEndDate))

  # 2. testing both dates in the summer time -----------------------------------

  # 3. testing both dates out of summer time -----------------------------------

  # 4. testing former date not in summer time, latter in summer time -----------

  # 5. testing former date in summer time, latter not in summer time -----------

})


