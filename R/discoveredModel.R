#' @title \code Get discovered model data
#' @description Get the discovered model data that includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param rqBody
#' @name getDiscoveredModel
#' @examples

getDiscoveredModel <- function(rqBody){

checkAuthentication()
lanaApiUrl <- Sys.getenv("LANA_URL")
lanaAuthorization <- Sys.getenv("LANA_TOKEN")
lanaUserId <- Sys.getenv("LANA_USERID")

userInfo <- httr::GET(paste0(lanaApiUrl, "/api/userInfo"), httr::add_headers(Authorization = lanaAuthorization))

if(userInfo["status_code"] == 400) {
stop(httr::paste0("Invalid rqBody JSON " + "Statuscode: ", userInfo["status_code"]))
}

else if(userInfo["status_code"] == 403) {
stop(paste0("Forbidden. Statuscode: ", userInfo["status_code"]))
}

else if(userInfo["status_code"] == 404) {
 stop(paste0("Not Found. Statuscode: ", userInfo["status_code"]))
}

else if(userInfo["status_code"] == 500) {
  stop(paste0("Backend Error. Statuscode: ", userInfo["status_code"]))
}

else if(userInfo["status_code"] != 200){
stop(paste0("Unknown Error. Statuscode: ", userInfo["status_code"]))
}


discoveredModelRequestData <- httr::GET(paste0(lanaApiUrl, "/api/discoveredModelWithFilter?request=", URLencode(rqBody)),
                                  httr::add_headers(Authorization = lanaAuthorization)
)

discoveredModelData <-  jsonlite::fromJSON(httr::content(discoveredModelRequestData, as = "text"))
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

