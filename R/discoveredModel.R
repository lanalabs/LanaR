library(plyr)
library(data.table)

#' @title Get discovered model data
#' @description Get the discovered model data, which includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param logName Full name of the uploaded csv file in Lana
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation (optional, use func. handleTraceFilterArgument to pass this argument when passed as a string or just an R nested list otherwise)
#' @param runConformance Decide whether you want to include conformance data with a R boolean value (optional)
#' @param modelId Provide the ID of Target model for conformance checking (optional)
#' @param edgeThreshold A value between 0.0 and 1.0 that guides the heuristic for edge removal from the discovered graph. Lower values mean that "unimportant" edges are removed from the generated graph. The heuristic aims to remove edges that have low frequency and which most likely correspond to parallel activities in the underlying process. Care is taken to not have dangling activities after removal of edges.
#' @param includeHeader A flag used during export of data. If set to "true", the file will contain a header describing the function of each column in the CSV file.
#' @param exportCaseAttributes A flag used during export of data. If set to "true", the case attributes of the event log will be exported for every event in the event output, helping with 3rd party analysis of the data.
#' @param timestampFormatter Defines how the timestamps are exported to CSV. Can be set to value that is understood by Java's DateTimeFormatter
#' @name discoveredModel

# TODO: doesn't work wth multiple filteres together
discoveredModel <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {

  # Creating request body
  rqBody <- append(
    list(
      logId = logId,
      traceFilterSequence = handleTraceFilterArgument(traceFilterSequence)
    ),
    list(...)
  )

  discoveredModelRequestData <- httr::POST(
    paste0("https://", lanaUrl, "/api/v2/mining/discover-model"),
    body = list(request = jsonlite::toJSON(rqBody, auto_unbox = TRUE)),
    encode = "multipart",
    httr::add_headers(makeAuthorisationHeader(lanaToken))
  )


  if (!discoveredModelRequestData$status_code == 400) {
    discoveredModelData <- jsonlite::fromJSON(httr::content(discoveredModelRequestData, as = "text", encoding = "UTF-8"))
  } else {
    return(NULL)
  }

  return(discoveredModelData)
}

# Example of tracefilter sequences as R lists
# Activity filter: list(type = "activityFilter", activity = "Activity name", inverted=TRUE/FALSE)
# Numeric Attribute Filter: list(type = "numericAttributeFilter", attributeName = "{Attribute Name}", min= min value, max=18)


#' @title Get activity performance statistics
#' @description Get the activity performance statistics, which include activity durations and counts.
#' @return activity performance statistics as data frame
#' @param discoveredModelData
#' @name activityPerformance

activityPerformance <- function(lanaUrl, lanaToken, logId, traceFilterSequence) {
  suppressWarnings({
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence)

  if (length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }
  
  actStats <- discoveredModelData$activityPerformanceStatistics %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    tibble::rownames_to_column("Activity") %>%
    select_if(~!all(is.na(.))) %>% discard(~all(is.na(.) | . ==""))

  return(actStats)
  })
}

#' @title Get Conformance Statistics
#' @description Get different KPIs such as deviating activities, counts, frequency etc.,
#' @return Conformance result as data frame
#' @param discoveredModelData
#' @name conformanceResult

conformanceResult <- function(lanaUrl, lanaToken, logId, traceFilterSequence) {
  suppressWarnings({
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, runConformance = TRUE)
  
  conRes <- discoveredModelData$conformanceResult$activityDeviationCounts

  return(conRes)
                   })
}


#' @title Get Log Statistics
#' @description Get different KPIs such as case, variant counts, frequency etc.,
#' @return Log statistics as data frame
#' @param discoveredModelData
#' @name logStatistics

logStatistics <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, ...)
  
  if (length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }
  
  logStat <- discoveredModelData$logStatistics %>%
    within(rm(
    "numericAttributeRanges", "attributeCounts",
    "conformanceStatistics")) %>%
    t() %>%
    as.data.frame
  
  return(logStat)
}

#' @title Get direct followers
#' @description Get data frame with the follower information
#' @return Direct followers as data frame
#' @name directFollowers

directFollowers <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, ...)
  
  directFollowersData <- discoveredModelData$directFollowerStatistics$directFollowers %>%
                          select_if(~sum(!is.na(.)) > 0)
  
  return(directFollowersData)
}
