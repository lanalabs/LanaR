#' freqRequestDataBuilder
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
freqRequestDataBuilder <- function(groupingType, dateType, attribute, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData){
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
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param attribute
#' @param valuesFrom
#' @param groupingType
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
freqContentBuilder <- function(lanaUrl, lanaToken, logId, dateType, attribute, valuesFrom,
                           groupingType, maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence){

  miningRequestData <- miningRequestBuilder(logId, traceFilterSequence)
  requestData <- freqRequestDataBuilder(groupingType, dateType, attribute, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData)
  content <- aggregateApiCall(lanaUrl, requestData, lanaToken)
  return(content)
}

#' aggregated frequencies without grouping
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreq <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "null",
                    valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                    sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "null"
  freq <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                      maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freq)

}

#' aggregated frequencies per year
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqYear <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "startDate",
                        valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                        sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byYear"
  freqYear <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                          maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqYear)

}

#' aggregated frequencies per quarter
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqQuarter <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "startDate",
                           valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                           sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byQuarter"
  freqQuarter <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                             maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqQuarter)

}

#' aggregated frequencies per month
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqMonth <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "startDate",
                         valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                         sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byMonth"
  freqMonth <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                           maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqMonth)

}

#' aggregated frequencies per day of year
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqDayofYear <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byDayOfYear"
  freqDayofYear <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqDayofYear)

}

#' aggregated frequencies per day of week
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqDayofWeek <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byDayOfWeek"
  freqDayofWeek <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqDayofWeek)

}

#' aggregated frequencies per hour of day
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqHourofDay <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byHourOfDay"
  freqHourofDay <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqHourofDay)

}

#' aggregated frequencies per attribute
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param attribute
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
getFreqAttribute <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, attribute,
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilterSequence = list()){
  groupingType <- "byAttribute"
  freqAttribute <- freqContentBuilder(lanaUrl, lanaToken, logId, dateType = NULL, attribute, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)

  return(freqAttribute)
}


