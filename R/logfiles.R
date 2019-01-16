#' Get all logs
#' @return data frame with all log files owned by or shared with the authenticated user
#' @name getLogs
#'
#' DEPRECEATED

getLogs <- function() {

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  logData <- httr::GET(paste0(lanaApiUrl, "/api/users/", lanaUserId, "/logs"), httr::add_headers(Authorization = lanaAuthorization))

  checkHttpErrors(logData)

  jsonlite::fromJSON(
    httr::content(logData, encoding = "UTF-8", type = "text")
  )

}

# getLogs <- function(lanaUrl, lanaToken, lanaUserId) {
#
#   lanar::checkAuthentication(lanaUrl, lanaToken)
#
#   logData <- httr::GET(paste0(lanaUrl, "/api/users/", lanaUserId, "/logs"), httr::add_headers(Authorization = lanaToken))
#
#   lanar::checkHttpErrors(logData)
#
#   jsonlite::fromJSON(httr::content(logData, encoding = "UTF-8", type = "text"))
#
# }


#' Choose specific log file
chooseLog <- function(logName){

  userLogs <- lanar::getLogs()
  logId<- max(userLogs[grepl(logName, userLogs$name, fixed=T), ]$id)

}
