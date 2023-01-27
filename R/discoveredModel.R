library(plyr)
library(data.table)

#' @title Get discovered model data
#' @description Get the discovered model data, which includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param logId Id of the uploaded csv file in Appian Process Mining
#' @param lanaUrl URL of the Appian Process Mining instance being used
#' @param lanaToken API Token of the user
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation, can be passed as string as well as R list
#' @param renderDiscoveredModel  Decide whether you ant to include conformance data with a boolean value (optional, default = TRUE)
#' @param runConformance Decide whether you want to include conformance data with a R boolean value (optional)
#' @param hideActivityFilter apply a filter to hide single or multiple activities from the discovered model. Should be passed as R list (optional)
#' @param modelId Provide the ID of Target model for conformance checking (optional)
#' @param edgeThreshold A value between 0.0 and 1.0 that guides the heuristic for edge removal from the discovered graph. Lower values mean that "unimportant" edges are removed from the generated graph. The heuristic aims to remove edges that have low frequency and which most likely correspond to parallel activities in the underlying process. Care is taken to not have dangling activities after removal of edges. (optional)
#' @param includeHeader A flag used during export of data. If set to "true", the file will contain a header describing the function of each column in the CSV file (optional)
#' @param exportCaseAttributes A flag used during export of data. If set to "true", the case attributes of the event log will be exported for every event in the event output, helping with 3rd party analysis of the data (optional)
#' @param timestampFormatter Defines how the timestamps are exported to CSV. Can be set to value that is understood by Java's DateTimeFormatter (optional)
#' @name discoveredModel

discoveredModel <- function(lanaUrl, lanaToken, logId, traceFilterSequence, renderDiscoveredModel = FALSE, ...) {
  # Creating request body
  rqBody <- append(
    list(
      logId = logId,
      traceFilterSequence = handleTraceFilterArgument(traceFilterSequence),
      renderDiscoveredModel = renderDiscoveredModel
    ),
    list(...)
  )

  rqBody[rqBody == "true"] <- TRUE
  rqBody[rqBody == "false"] <- FALSE

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

#' @title Get activity performance statistics
#' @description Get the activity performance statistics, which include activity durations and counts.
#' @return activity performance statistics as data frame
#' @param logId Id of the uploaded csv file in Appian Process Mining
#' @param lanaUrl URL of the Appian Process Mining instance being used
#' @param lanaToken API Token of the user
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation, can be passed as string as well as R list
#' @name activityPerformance

activityPerformance <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, ...)

  if (length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- discoveredModelData$activityPerformanceStatistics %>%
    rbindlist(fill = TRUE, idcol = "activity") %>%
    as.data.frame()

  return(actStats)
}

#' @title Get Conformance Statistics
#' @description Get different KPIs such as deviating activities, counts, frequency etc.,
#' @return Conformance result as data frame
#' @param logId Id of the uploaded csv file in Appian Process Mining
#' @param lanaUrl URL of the Appian Process Mining instance being used
#' @param lanaToken API Token of the user
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation, can be passed as string as well as R list
#' @name conformanceResult

conformanceResult <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, runConformance = TRUE, ...)

  conRes <- discoveredModelData$conformanceResult$activityDeviationCounts

  return(conRes)
}

#' @title Get Log Statistics
#' @description Get different KPIs such as case, variant counts, frequency etc.,
#' @return Log statistics as data frame
#' @param logId Id of the uploaded csv file in Appian Process Mining
#' @param lanaUrl URL of the Appian Process Mining instance being used
#' @param lanaToken API Token of the user
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation, can be passed as string as well as R list
#' @name logStatistics

logStatistics <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, ...)

  if (length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  logStat <- discoveredModelData$logStatistics %>%
    within(rm(
      "numericAttributeRanges", "attributeCounts",
      "conformanceStatistics"
    )) %>%
    t() %>%
    as.data.frame() %>%
    mutate_if(is.list, as.numeric)

  return(logStat)
}

#' @title Get direct followers
#' @description Get data frame with the follower information
#' @param logId Id of the uploaded csv file in Appian Process Mining
#' @param lanaUrl URL of the Appian Process Mining instance being used
#' @param lanaToken API Token of the user
#' @param traceFilterSequence Integrate any kind of filter from Appian Process Mining into your aggregation, can be passed as string as well as R list
#' @return Direct followers as data frame
#' @name directFollowers

directFollowers <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...) {
  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence, ...)

  directFollowersData <- discoveredModelData$directFollowerStatistics$directFollowers %>%
    select_if(~ sum(!is.na(.)) > 0)

  return(directFollowersData)
}
