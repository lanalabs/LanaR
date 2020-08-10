#installed.packages("stringr")
library(stringr)
source('R/aggregateUtils.R')
#source(here::here('/Users/goegges/Development/lanar'))

#' requestDataBuilder
#'
#' @param groupingType
#' @param dateType
#' @param attribute
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param miningRequestData
#'
#' @return
#' @export
#'
#' @examples
requestDataBuilder <- function(groupingType, dateType, attribute, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData){
  requestData <- list(
    metric = metricFreqBuilder(),
    valuesFrom = list(
      type = valuesFrom
    ),
    options =  optionsBuilder(maxAmountAttributes, valueSorting, sortingOrder),
    miningRequest = miningRequestData
  )
  if (groupingType != "null") {
    requestData[["grouping"]] <- groupingBuilder(groupingType, dateType, attribute)
  }
  return(requestData)
}

#' aggregated frequencies API call
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param attribute
#' @param valuesFrom
#' @param groupingType
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
contentBuilder <- function(lanaUrl, applicationKey, logId, dateType, attribute, valuesFrom,
                           groupingType, maxAmountAttributes, valueSorting, sortingOrder, traceFilter){

  miningRequestData <- miningRequestBuilder(logId, traceFilter)
  requestData <- requestDataBuilder(groupingType, dateType, attribute, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData)
  content <- aggregateApiCall(lanaUrl, requestData, applicationKey)
  return(content)
}

#' aggregated frequencies without grouping
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
getFreq <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "null",
                    valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                    sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "null"
  freq <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                      maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freq)

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
                        valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                        sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byYear"
  freqYear <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                          maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
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
                           valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                           sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byQuarter"
  freqQuarter <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                             maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
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
                         valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                         sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byMonth"
  freqMonth <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                           maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
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
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byDayOfYear"
  freqDayofYear <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
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
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byDayOfWeek"
  freqDayofWeek <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqDayofWeek)

}

#' aggregated frequencies per hour of day
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
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byHourOfDay"
  freqHourofDay <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqHourofDay)

}

#' aggregated frequencies per attribute
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param attribute
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqAttribute <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, attribute,
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){
  groupingType <- "byAttribute"
  freqAttribute <- contentBuilder(lanaUrl, applicationKey, logId, dateType = NULL, attribute, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)

  return(freqAttribute)
}


