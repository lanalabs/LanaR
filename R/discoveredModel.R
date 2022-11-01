library(plyr)
library(data.table)


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
#' @param timestampFormatter Defines how the timestamps are exported to CSV. Can be set to value that is understood by Java's DateTimeFormatter
#'
#' @name discoveredModel

discoveredModel <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...){

   # converting tracefiltersequence to acceptable format
  traceFilterSequence <-if (typeof(traceFilterSequence) == 'list' &  length(traceFilterSequence !=0))
   handleTraceFilterArgument(traceFilter = list(traceFilterSequence)) else
      handleTraceFilterArgument(traceFilter = traceFilterSequence)


  # Creating request body
  rqBody <- append(list(logId = logId,
                        traceFilterSequence = traceFilterSequence),
                   list(...))

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

activityPerformance <- function(lanaUrl, lanaToken, logId, traceFilterSequence){

  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence)

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- as.data.frame(do.call(rbind, lapply(discoveredModelData$activityPerformanceStatistics, `length<-`,
                                                  max(sapply(discoveredModelData$activityPerformanceStatistics, length)))))

  return(actStats)
}



#' @title Get Conformance Statistics
#' @description Get different KPIs such as deviating activities, counts, frequency etc.,
#' @return Conformance result as data frame
#' @param discoveredModelData
#' @name conformanceResult

conformanceResult <- function(lanaUrl, lanaToken, logId, traceFilterSequence, ...){

  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence,
                                         modelID= list(...)$modelID, runConformance = list(...)$runConformance)

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  conRes <- as.data.frame(do.call(rbind, lapply(discoveredModelData$conformanceResult, `length<-`,
                                                  max(sapply(discoveredModelData$conformanceResult,length)))))

  return(conRes)
}


#' @title Get Log Statistics
#' @description Get different KPIs such as case, variant counts, frequency etc.,
#' @return Log statistics as data frame
#' @param discoveredModelData
#' @name logStat

logStat <- function(lanaUrl, lanaToken, logId, traceFilterSequence){

  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId, traceFilterSequence
                                         )

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  statList <- within(discoveredModelData$logStatistics, rm("numericAttributeRanges", "attributeCounts",
                                                           "conformanceStatistics"))
  logstat <- as.data.frame(do.call(rbind, lapply(statList, `length<-`,
                                                  max(sapply(statList,length)))))
  logstat <- setNames(cbind(rownames(logstat), logstat, row.names = NULL), c("Stats", "Values"))
  rownames(logstat)<- logstat$Stats
  logstat<- transpose(logstat)
  colnames(logstat) <- logstat[1,]

  return(logstat[-1,])
}


