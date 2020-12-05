source("R/InternalVariables.R")
source("R/Utils.R")



lScrapeTablesFromUrlByXPaths <- function(cUrl, lXPaths) {

  # 1. load the content of the table pointed at by cUrl
  res <- try(expr = {
    xml2::read_html(x = cUrl)
  }, silent = TRUE)
  if (methods::is(object = res, class2 = "try-error")) {
    message("Failed to load the website from URL - skipping: ", cIterWebpageUrl)
    next
  }

  # 2. iterate over the XPaths
  # cIterXpath <- names(lXPaths)[[1]]
  lIterOut <- vector(mode = "list", length = length(lXPaths))
  names(lIterOut) <- names(lXPaths)
  for (cIterXpath in names(lXPaths)) {
    message("\t\tFetching table - XPath's name: ", cIterXpath)
    # 2.1. fetch the XPath
    objIterXpathRes <- try(expr = {
        rvest::html_nodes(x = res, xpath = lXPaths[[cIterXpath]])
    }, silent = TRUE)
    # 2.2. if error, skip iteration
    if (methods::is(object = res, class2 = "try-error")) {
      message("\t\tFailed to load the website from URL - skipping: ",
              cIterWebpageUrl, cIterXpath)
      next
    }
    # 2.3. parse the list res into data.frame
    try(expr = {
      dfIterRes <- rvest::html_table(x = objIterXpathRes)
    }, silent = TRUE)
    # 2.4.  if error, skip iteration
    if (methods::is(object = res, class2 = "try-error")) {
      message("\t\tFailed to parse the output retrieved from the website - skipping: ",
              cIterWebpageUrl, cIterXpath)
      next
    }
    # 2.5. if successful, save the table to the output list
    lIterOut[[cIterXpath]] <- dfIterRes[[1]]
  }

  return(lIterOut)
}



# charWhichLoad <- "actualTotal"
# charBiddingZoneCode <- lDictionaryBiddingZones$Poland
# charDocumentType <- lDictionaryDocumentType$systemTotalLoad
# dateStartDate <- as.Date("2020-10-03")
# dateEndDate <- as.Date("2020-10-06")


# res1 <- dtFetchDailyLoadDataForBiddingZone(charWhichLoad = "actualTotal",
#                               charSecurityToken = charSecurityToken,
#                               charBiddingZoneCode = lDictionaryBiddingZones$Poland,
#                               dateStartDate = as.Date("2020-10-24"),
#                               dateEndDate = as.Date("2020-10-26"),
#                               cPosixTimeZoneName = "Europe/Warsaw")
dtFetchDailyLoadDataForBiddingZone <- function(
  charWhichLoad, charSecurityToken, charBiddingZoneCode,
  dateStartDate, dateEndDate, cPosixTimeZoneName = "Europe/Warsaw") {

  # ----------------------------------------------------------------------------
  # 0. validate function parameters --------------------------------------------
  # 0.1. charWhichLoad
  if (!(charWhichLoad %in% c("actualTotal", "foracastDam"))) {
    stop("The 'charWhichLoad' parameter of dtFetchLoadDataForBiddingZone function is incorrect! ")
  }
  if (charWhichLoad == "actualTotal") {
    charProcessType <- "A16"
  } else {
    charProcessType <- "A01"
  }
  # 0.2. start date and end date
  if (!bIsScalarOfClass(objIn = dateStartDate, cClassName = "Date") |
      !bIsScalarOfClass(objIn = dateEndDate, cClassName = "Date")) {
    stop("One of the parameters :  dateStartDate or dateEndDate is not of Date class! ")
  }
  if (dateStartDate > dateEndDate) {
    stop("The function parameter dateStartDate precedes dateEndDate! ")
  }
  # 0.3. check if the time zone name is valid
  if (!(cPosixTimeZoneName %in% OlsonNames())) {
    stop("The cPosixTimeZoneName parameter of the dtFetchLoadDataForBiddingZone function is invalid ",
         "; not in the vector of time zones returned by OlsonNames()! ")
  }

  # ----------------------------------------------------------------------------
  # 1. prepare the expected grid of hours for the range of dates in scope ------
  # 1.1. prepare charPeriodStart and charPeriodEnd parameters to the query
  # 1.1.1. charPeriodStart
  posixctStartTime <- ISOdatetime(
    year = lubridate::year(x = dateStartDate),
    month = lubridate::month(x = dateStartDate),
    day = lubridate::day(x = dateStartDate), hour = 0L, min = 0L, sec = 0L,
    tz = cPosixTimeZoneName)
  posixctStartTimeGMT <- posixctChangeToNewTimeZone(posixctIn = posixctStartTime,
                                                  cNewTimeZone = "GMT")
  charPeriodStart <- strftime(x = posixctStartTimeGMT, format = "%Y%m%d%H%M",
                              usetz = FALSE, tz = "GMT")
  # 1.1.2. charPeriodEnd
  posixctEndTime <- ISOdatetime(
    year = lubridate::year(x = dateEndDate), month = lubridate::month(x = dateEndDate),
    day = lubridate::day(x = dateEndDate) + 1, hour = 0L, min = 0L, sec = 0L,
    tz = cPosixTimeZoneName)
  posixctEndTimeGMT <- posixctChangeToNewTimeZone(posixctIn = posixctEndTime,
                                                 cNewTimeZone = "GMT")
  charPeriodEnd <- strftime(x = posixctEndTimeGMT, format = "%Y%m%d%H%M",
                            usetz = FALSE, tz = "GMT")
  # 1.2. make the grid of hourly buckets in under the target time zone (!)
  dtHourlyGrid <- dtGenerateHourlyGridForTimeRange(
    posixctStartTime = posixctStartTime, posixctEndTime = posixctEndTime)

  # 2. concatenate the query ---------------------------------------------------
  charQueryUrl <- paste0("https://transparency.entsoe.eu/api?securityToken=", charSecurityToken,
                         "&documentType=", "A65",
                         "&processType=", charProcessType,
                         "&outBiddingZone_Domain=", charBiddingZoneCode,
                         "&periodStart=", charPeriodStart,
                         "&periodEnd=", charPeriodEnd)

  # 3. run the query -----------------------------------------------------------
  message("Fetching data from the URL: ", charQueryUrl)
  res <- RCurl::getURL(url = charQueryUrl) %>% XML::xmlParse(asText = TRUE) %>%
    XML::xmlToList()
  if (methods::is(object = res, class2 = "try-erro")) {
    stop("Error occured when trying to fetch the data from URL: ", charQueryUrl,
         " the error: ", res)
  }
  message("Successfully fetched data from the URL: ", charQueryUrl)

  # 3. process the output ------------------------------------------------------
  # 3.1. extract the only relevant piece of data
  res <- res$TimeSeries$Period
  if (is.null(x = res)) {
    stop("The output is corrupt: cannot extract the res$TimeSeries$Period member! ")
  }
  # 3.2. check if data granularity is hourly
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

  # 4. further parse the table before outputting -------------------------------
  # 4.1. check if the dimension of table with grid of dates and the table with data are consistent
  if (nrow(dtOut) != nrow(dtHourlyGrid)) {
    stop("The table with the data and the table with the hourly grid have inconsistent numbers of rows; ",
         "#rows hourly grid table: ", nrow(dtHourlyGrid), "; #rows table with data: ", nrow(dtOut))
  }
  # 4.2. prepare final output data table
  dtDataOut <- data.table::copy(x = dtHourlyGrid)
  dtDataOut[, var_value := dtOut$QuantityValue]
  dtDataOut[, TimeZone := cPosixTimeZoneName]
  data.table::setnames(x = dtDataOut, old = "var_value", new = charWhichLoad)

  return(dtDataOut)
}



