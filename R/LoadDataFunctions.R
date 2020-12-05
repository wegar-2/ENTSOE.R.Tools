

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
  # 0.2. check if the time zone name is valid
  if (!(cPosixTimeZoneName %in% OlsonNames())) {
    stop("The cPosixTimeZoneName parameter of the dtFetchLoadDataForBiddingZone function is invalid ",
         "; not in the vector of time zones returned by OlsonNames()! ")
  }

  # 1. parse the time range ----------------------------------------------------
  lTimeInfo <- lGeneralPurposeTimeHandler(dateStartDate = dateStartDate,
                                          dateEndDate = dateEndDate)
  charPeriodStart <- lTimeInfo
  charPeriodEnd <- lTimeInfo

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

