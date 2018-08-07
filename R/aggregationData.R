#' @title \code Aggregate data by different dimensions
#' @description a method that gets the aggregation of the requested data. \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @return total of the aggregated data
#' @param discoveredModelData
#' @name getAggregationRequestData

getAggregationRequestData <- function(rqBody){
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

# Make request LANA
aggregationRequestData <- httr::GET(paste0(lanaApiUrl, "/api/aggregatedData?request=", URLencode(rqBody)),
                                    httr::add_headers(Authorization = lanaAuthorization)
)



# Read response into data frame
actAggrData <- jsonlite::fromJSON(httr::content(aggregationRequestData, as = "text"))

chartValues <- plyr::rename(actAggrData$chartValues, c(".id"="action"))
return(chartValues)
}
