#' @title \code Get discovered model data
#' @description Get the discovered model data, which includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param rqBody - request body as JSON
#' @name getDiscoveredModel
getDiscoveredModel <- function(rqBody){

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  discoveredModelRequestData <- httr::GET(
    paste0(lanaApiUrl, "/api/discoveredModelWithFilter?request=",
    URLencode(rqBody, reserved = T)),
    httr::add_headers(Authorization = lanaAuthorization)
  )

  checkHttpErrors(discoveredModelRequestData)

  discoveredModelData <- jsonlite::fromJSON(httr::content(discoveredModelRequestData, as = "text", encoding = "UTF-8"))
  return(discoveredModelData)
}

#' @title \code Get activity performance statistics
#' @description Get the activity performance statistics, which include activity durations and counts.
#' @return activity performance statistics as data frame
#' @param discoveredModelData - retrieved by \code{\link{getDiscoveredModel}}
#' @name getActivityPerformanceStatistics
getActivityPerformanceStatistics <- function(discoveredModelData){

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- ldply(discoveredModelData$activityPerformanceStatistics, data.frame)

  return(actStats)
}

