library(stringr)
#' mining request build
#'
#' @param logId
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
miningRequest <- function(logId, traceFilter){
miningRequestData <- list(
  includeHeader = TRUE,
  includeLogId = TRUE,
  logId = logId,
  edgeThreshold = 1,
  traceFilterSequence = traceFilter,
  runConformance = FALSE
)
return(miningRequestData)
}

#' #' trace filter sequence build
#' #'
#' #' @param type
#' #' @param min
#' #' @param max
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#'
#' traceFilter <- function(type, min, max){
#'   if (is.null(type) | is.null(min) | is.null(max)) {
#'     traceFilterSequence = list()
#'   } else {
#'     traceFilterSequence = list(list(
#'       max = max,
#'       min = min,
#'       type = "variantSliderFilter"))
#'   }
#'   return(traceFilterSequence)
#' }


#' request data build
#'
#' @param dateType
#' @param grouping
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param miningRequestData
#'
#' @return
#' @export
#'
#' @examples
requestData <- function(dateType, grouping, valuesFrom, maxAmountAttributes, miningRequestData){
requestData <- list(
  #check null
  #check attribute grouping type
  grouping = list(
    dateType = dateType,
    timeZone = "Europe/Berlin",
    type = grouping
  ),
  metric = list(
    type = "frequency"
  ),
  valuesFrom = list(
    type = valuesFrom
  ),
  options =  list(
    maxAmountAttributes = maxAmountAttributes
  ),
  miningRequest = miningRequestData
)
return(requestData)
}

#' post aggregate api call to lana
#'
#' @param lanaUrl
#' @param requestData
#' @param applicationKey
#'
#' @return
#' @export
#'
#' @examples
aggregateApiCall <- function(lanaUrl, requestData, applicationKey){
response <- httr::POST(
  paste0("https://", lanaUrl, "/api/v2/aggregate-data"),
  body = list(request = jsonlite::toJSON(requestData, auto_unbox = TRUE)),
  encode = "multipart",
  httr::add_headers( c(
    Authorization = applicationKey
  ))
)
c <- httr::content(response)
groupingLabel <- c[["groupingAxisLabel"]]
groupingLabel <- str_replace_all(groupingLabel, " ", "_")
metrikLabel <- c[["metricAxisLabel"]]
metrikLabel <- str_replace_all(metrikLabel, " ", "_")
content <- c$chartValues
content <- do.call(rbind.data.frame, content)
content$X.type <- NULL
names(content)[names(content) == "xAxis"] <- groupingLabel
names(content)[names(content) == "yAxis"] <- metrikLabel


#content <- do.call(rbind.data.frame, c)
#content <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

return(content)
}

#' aggregated frequencies API call
#'
#' @param lanaUrl default = "cloud-backend.lanalabs.com"
#' @param applicationKey API Key in Lana (Export Filter Settings)
#' @param logId id of uploaded log in lana
#' @param dateType "startDate" or "endDate"
#' @param valuesFrom "allCases" or "allEvents"
#' @param grouping "byYear", "byQuarter", "byMonth", "byDayOfYear", "byDayOfWeek" or "byHourOfDay"
#' @param maxAmountAttributes default = 10
#' @param traceFilter
#'
#' @return frequency counts
#'
#' @examples
getFreq <- function(lanaUrl, applicationKey, logId, dateType, valuesFrom,
                     grouping, maxAmountAttributes, traceFilter){

  miningRequestData <- miningRequest(logId, traceFilter)
  requestData <- requestData(dateType, grouping, valuesFrom, maxAmountAttributes, miningRequestData)
  content <- aggregateApiCall(lanaUrl, requestData, applicationKey)
  return(content)
}

#' aggregated frequencies per year
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqYear <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                        valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

  grouping <- "byYear"
  freqYear <- getFreq(lanaUrl, applicationKey, logId, dateType, valuesFrom, grouping,
                        maxAmountAttributes, traceFilter)
  return(freqYear)

}

#' aggregated frequencies per quarter
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqQuarter <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                           valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

  grouping <- "byQuarter"
  freqQuarter <- getFreq(lanaUrl, applicationKey, logId, dateType, valuesFrom, grouping,
                        maxAmountAttributes, traceFilter)
  return(freqQuarter)

}

#' aggregated frequencies per month
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqMonth <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                         valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

  grouping <- "byMonth"
  freqMonth <- getFreq(lanaUrl, applicationKey, logId, dateType, valuesFrom, grouping,
                        maxAmountAttributes, traceFilter)
  return(freqMonth)

}

#' aggregated frequencies per day of year
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqDayofYear <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

  grouping <- "byDayOfYear"
  freqDayofYear <- getFreq(lanaUrl, applicationKey, logId, dateType, valuesFrom, grouping,
                        maxAmountAttributes, traceFilter)
  return(freqDayofYear)

}

#' aggregated frequencies per day of week
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqDayofWeek <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

  grouping <- "byDayOfWeek"
  freqDayofWeek <- getFreq(lanaUrl, applicationKey, logId, dateType, valuesFrom, grouping,
                             maxAmountAttributes, traceFilter)
  return(freqDayofWeek)

}

#' aggregated frequencies per hour of the day
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqHourofDay <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

  grouping <- "byHourOfDay"
  freqHourofDay <- getFreq(lanaUrl, applicationKey, logId, dateType, valuesFrom, grouping,
                             maxAmountAttributes, traceFilter)
  return(freqHourofDay)

}

getFreqAttribute <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, traceFilter = list()){

}

