#' @title \code Get discovered model data
#' @description Get the discovered model data that includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param rqBody
#' @name getDiscoveredModel
#' @examples

getDiscoveredModel <- function(rqBody){

  logId = 18
  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

   if(length(rqBody) == 0) {
    stop(paste0("You haven't defined a request body. Please define a request body"))
  }

  discoveredModelRequestData <- GET(paste0(lanaApiUrl, "/api/discoveredModelWithFilter?request=", URLencode(rqBody)),
                                    add_headers(Authorization = lanaAuthorization)
  )

  discoveredModelData <- fromJSON(content(discoveredModelRequestData, as = "text"))
  return(discoveredModelData)
}

#' @title \code Get activity performance statistics
#' @description a method that gets the activity performance statistics
#' @return activity performance statistics
#' @param discoveredModelData
#' @name getActivityPerformanceStatistics

getActivityPerformanceStatistics <- function(discoveredModelData){

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- ldply(discoveredModelData$activityPerformanceStatistics, data.frame)

  return(actStats)
}

