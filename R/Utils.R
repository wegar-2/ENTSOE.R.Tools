bIsScalarOfType <- function(objIn, cTypeName) {
  # 1. validate the input
  if (!is.character(x = cTypeName) | length(x = cTypeName) != 1L) {
    stop("Error inside bIsScalarOfType function call, the cTypeName parameter is either ",
         " not of character type or not of length one! ")
  }
  # 2. check the objIn
  if (typeof(x = objIn) == cTypeName & length(objIn) == 1L)  {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



bIsScalarOfClass <- function(objIn, cClassName) {
  # 1. validate the input
  if (!is.character(x = cClassName) | length(x = cClassName) != 1L) {
    stop("Error inside bIsScalarOfType function call, the cClassName parameter is either ",
         " not of character type or not of length one! ")
  }
  # 2. check the objIn
  if (class(x = objIn) == cClassName & length(objIn) == 1L)  {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



dtGenerateHourlyGridForTimeRange <- function(posixctStartTime, posixctEndTime) {
  cSequenceOfHourlyBuckets <- seq(posixctStartTime, posixctEndTime, by = "hour")
  return(data.table::data.table(
    HourlyBucketTimeStart = cSequenceOfHourlyBuckets[1:(length(cSequenceOfHourlyBuckets) - 1L)],
    HourlyBucketTimeEnd = cSequenceOfHourlyBuckets[2:length(cSequenceOfHourlyBuckets)]))
}



posixctChangeToNewTimeZone <- function(posixctIn, cNewTimeZone = "Europe/London") {

  # 0. input validation --------------------------------------------------------
  # 0.1. validate posixctIn
  if (!("POSIXct" %in% class(x = posixctIn))) {
    stop("The posixctIn parameter of the posixctChangeToNewTimeZone function is invalid ",
         "; not of posixct class! ")
  }
  # 0.2. validate new time zone
  if (!bIsScalarOfType(objIn = cNewTimeZone, cTypeName = "character")) {
    stop("The cNewTimeZone parameter of the posixctChangeToNewTimeZone function is invalid ",
         "; not a character scalar! ")
  }
  if (!(cNewTimeZone %in% OlsonNames())) {
    stop("The cNewTimeZone parameter of the posixctChangeToNewTimeZone function is invalid ",
         "; not in the vector of time zones returned by OlsonNames()! ")
  }

  # 1. convert to character scalar in the target time zone ---------------------
  cStringTargetTimeZone <- format(x = posixctIn, tz = cNewTimeZone, usetz = TRUE)

  # 2. cast the character as posixct -------------------------------------------
  posixctOut <- as.POSIXct(x = cStringTargetTimeZone, tz = cNewTimeZone)

  return(posixctOut)
}



lGeneralPurposeTimeHandler <- function(dateStartDate, dateEndDate) {

  # 1. input validation --------------------------------------------------------
  # 1.1. validate start date and end date classes
  if (!bIsScalarOfClass(objIn = dateStartDate, cClassName = "Date") |
      !bIsScalarOfClass(objIn = dateEndDate, cClassName = "Date")) {
    stop("One of the parameters :  dateStartDate or dateEndDate is not of Date class! ")
  }
  if (dateStartDate > dateEndDate) {
    stop("The function parameter dateStartDate precedes dateEndDate! ")
  }
  # 1.2. check if the time zone name is valid
  if (!(cPosixTimeZoneName %in% OlsonNames())) {
    stop("The cPosixTimeZoneName parameter of the dtFetchLoadDataForBiddingZone function is invalid ",
         "; not in the vector of time zones returned by OlsonNames()! ")
  }

  # ----------------------------------------------------------------------------
  # 2. prepare the expected grid of hours for the range of dates in scope ------
  # 2.1. prepare charPeriodStart and charPeriodEnd parameters to the query
  # 2.1.1. charPeriodStart
  posixctStartTime <- ISOdatetime(
    year = lubridate::year(x = dateStartDate),
    month = lubridate::month(x = dateStartDate),
    day = lubridate::day(x = dateStartDate), hour = 0L, min = 0L, sec = 0L,
    tz = cPosixTimeZoneName)
  posixctStartTimeGMT <- posixctChangeToNewTimeZone(posixctIn = posixctStartTime,
                                                    cNewTimeZone = "GMT")
  charPeriodStart <- strftime(x = posixctStartTimeGMT, format = "%Y%m%d%H%M",
                              usetz = FALSE, tz = "GMT")
  # 2.1.2. charPeriodEnd
  posixctEndTime <- ISOdatetime(
    year = lubridate::year(x = dateEndDate), month = lubridate::month(x = dateEndDate),
    day = lubridate::day(x = dateEndDate) + 1, hour = 0L, min = 0L, sec = 0L,
    tz = cPosixTimeZoneName)
  posixctEndTimeGMT <- posixctChangeToNewTimeZone(posixctIn = posixctEndTime,
                                                  cNewTimeZone = "GMT")
  charPeriodEnd <- strftime(x = posixctEndTimeGMT, format = "%Y%m%d%H%M",
                            usetz = FALSE, tz = "GMT")
  # 2.2. make the grid of hourly buckets in under the target time zone (!)
  dtHourlyGrid <- dtGenerateHourlyGridForTimeRange(
    posixctStartTime = posixctStartTime, posixctEndTime = posixctEndTime)

  return(list(
    dtHourlyGrid = dtHourlyGrid,
    charPeriodStart = charPeriodStart,
    posixctStartTime = posixctStartTime,
    posixctStartTimeGMT = posixctStartTimeGMT,
    charPeriodEnd = charPeriodEnd,
    posixctEndTime = posixctEndTime,
    posixctEndTimeGMT = posixctEndTimeGMT
  ))
}



lGeneralPurposeEntsoeQueryRunner <- function(cQueryUrl) {

  # 1. run the query -----------------------------------------------------------
  message("Fetching data from the URL: ", charQueryUrl)
  res <- RCurl::getURL(url = charQueryUrl) %>% XML::xmlParse(asText = TRUE) %>%
    XML::xmlToList()
  if (methods::is(object = res, class2 = "try-erro")) {
    stop("Error occured when trying to fetch the data from URL: ", charQueryUrl,
         " the error: ", res)
  }
  message("Successfully fetched data from the URL: ", charQueryUrl)

  # 2. process the output ------------------------------------------------------
  # 2.1. extract the only relevant piece of data
  res <- res$TimeSeries$Period
  if (is.null(x = res)) {
    stop("The output is corrupt: cannot extract the res$TimeSeries$Period member! ")
  }
  # 2.2. check if data granularity is hourly
  if (res$resolution != "PT60M") {
    stop("Incorrect, unhandled data granularity encountered: ", res$resolution)
  }
  # 3.3. extract the relevant data
  # 3.3.1. get ordering
  iOrderingVector <- sapply(X = res, FUN = function(x) {
    if (all(c("position", "quantity") %in% names(x))) {
      return(x[["position"]])
    } else {
      return(NA)
    }
  })
  iOrderingVector <- as.integer(iOrderingVector)
  dQuantityVector <- sapply(X = res, FUN = function(x) {
    if (all(c("position", "quantity") %in% names(x))) {
      return(x[["quantity"]])
    } else {
      return(NA)
    }
  })
  dQuantityVector <- as.double(dQuantityVector)
  if (any(is.na(iOrderingVector) != is.na(x = dQuantityVector))) {
    stop("Inconsistent data fetched...")
  }
  iOrderingVector <- iOrderingVector[!is.na(iOrderingVector)]
  dQuantityVector <- dQuantityVector[!is.na(dQuantityVector)]
  # 3.4. put that data into ordered table
  dtOut <- data.table::data.table(
    IntervalNumber = iOrderingVector,
    QuantityValue = dQuantityVector)
  dtOut <- dtOut[order(iOrderingVector, decreasing = FALSE), ]

}

