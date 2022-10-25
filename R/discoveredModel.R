library(plyr)

# build aggregation settings for the API call
#' Make authorisation headers for lana api
#' @param applicationKey the application key with or without starting 'API-Key'
#' @export
makeAuthorisationHeader <- function(applicationKey) {
  headerFields <-
    c(
      Authorization = applicationKey
    )
  return(headerFields)
}

#' Programmatically remove filters from a trace filter sequence by type
#' @param traceFilter the trace filter as character or R list
#' @param removeFilters filter types to remove
#' @export
handleTraceFilterArgument <- function(traceFilter, removeFilters = list()) {

  res <- if (typeof(traceFilter) == 'character' )
    jsonlite::fromJSON(traceFilter, simplifyVector = FALSE) else
      traceFilter

  return(
    rlist::list.filter(res, !(type %in% removeFilters) )
  )
}


# Below attributes mostly do not change the result of the call
#1. computeAttributecounts: If set to "true", the categorical attribute values are counted across the event log. Can lead to considerable processing overhead for data-sets with many distinct attribute values.
#2. includeHeader: A flag used during export of data. If set to "true", the file will contain a header describing the function of each column in the CSV file.
#3. logName: Can be used to override the name of the event log inserted into the output CSV, for external control over the naming. Used in combination with the inlcudeLogID flag.
#4. exportCaseAttributes: A flag used during export of data. If set to "true", the case attributes of the event log will be exported for every event in the event output, helping with 3rd party analysis of the data.
#5. exportDuration: A flag used during export of data. If set to "true", the duration of events and cases will be exported as a column in the CSV output.
#6. onlyColumns: During export of a CSV this allows to control which columns should be exported. Helpful to reduce the amount of exported data when only specific information is required for use in external tooling.
#7. timestampFormatter: Defines how the timestamps are exported to CSV. Can be set to value that is understood by Java's DateTimeFormatter
#8. limit: The number of items that will be shown on each page when results are paginated.
#9. page: The selected page of the paginated results.
#10. sort: Defines how results are sorted.
#11. computeNumericAttributeRanges: If set to "true", the numerical attributes ranges are computed across the event log.Does not make any difference in the result.
#12. computeCaseCounts: If set to "true", the case counts are added to the results instead of just the raw frequency that is often based on the number of events in the event log.


#' @title Get discovered model data
#' @description Get the discovered model data, which includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param logName Full name of the uploaded csv file in Lana
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation (optional, use func. handleTraceFilterArgument to pass this argument when passed as a string or just an R nested list otherwise)
#' @param runConformance Decide whether you want to include conformance data with a R booelan value (optional)
#' @param modelId Provide the ID of Taret model for conformance checking (optional)
#' @param edgeThreshold A value between 0.0 and 1.0 that guides the heuristic for edge removal from the discovered graph. Lower values mean that "unimportant" edges are removed from the generated graph. The heuristic aims to remove edges that have low frequency and which most likely correspond to parallel activities in the underlying process. Care is taken to not have dangling activities after removal of edges.
#' @param includeHeader A flag used during export of data. If set to "true", the file will contain a header describing the function of each column in the CSV file.
#' @param exportCaseAttributes A flag used during export of data. If set to "true", the case attributes of the event log will be exported for every event in the event output, helping with 3rd party analysis of the data.
#' @param onlyColumns During export of a CSV this allows to control which columns should be exported. Helpful to reduce the amount of exported data when only specific information is required for use in external tooling.
#' @param timestampFormatter Defines how the timestamps are exported to CSV. Can be set to value that is understood by Java's DateTimeFormatter
#' @param computeAttributeCounts If set to "true", the categorical attribute values are counted across the event log. Can lead to considerable processing overhead for data-sets with many distinct attribute values.
#'
#' @name discoveredModel

discoveredModel <- function(lanaUrl, lanaToken, logId,...){

  # Check the variables being tramsitted from LANA and receiving the user ID.


  rqBody <- append(list(logId = logId), list(...))

  discoveredModelRequestData <- httr::POST(
    paste0("https://", lanaUrl, "/api/v2/mining/discover-model"),
    body = list(request = jsonlite::toJSON(rqBody, auto_unbox = TRUE)),
    encode = "multipart",
    httr::add_headers(makeAuthorisationHeader(lanaToken))
  )


  if (!discoveredModelRequestData$status_code==400){
    discoveredModelData <- jsonlite::fromJSON(httr::content(discoveredModelRequestData, as = "text", encoding = "UTF-8"))
    } else {
      return(NULL)
  }

  return(discoveredModelData)

}

#' @title Get activity performance statistics
#' @description Get the activity performance statistics, which include activity durations and counts.
#' @return activity performance statistics as data frame
#' @param discoveredModelData
#' @name activityPerformance

activityPerformance <- function(lanaUrl, lanaToken, logId){

  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId)

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- ldply(discoveredModelData$activityPerformanceStatistics, data.frame)

  return(actStats)
}

