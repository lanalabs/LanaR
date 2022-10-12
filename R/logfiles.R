#' Get all logs
#'
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' 
#' @return Data Frame with all logs owned by or shared with the authenticated user

getLogs <- function(lanaUrl, lanaToken) {
  
  response <- httr::GET(
    paste0("https://", lanaUrl, "/api/logs"),
    httr::add_headers(Authorization = lanaToken))
  lanar::checkHttpErrors(response)
  content <- jsonlite::fromJSON(httr::content(response, encoding = "UTF-8", type = "text"))
  return(content)
}


#' Get ID of authenticated user
#' 
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' 
#' @return A string with the user ID

getUserId <- function(lanaUrl, lanaToken) {
  
  response <- httr::GET(
    paste0("https://", lanaUrl, "/api/users/by-token"),
    httr::add_headers(Authorization = lanaToken)  )
  lanar::checkHttpErrors(response)
  content <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(content$id)
}


#' Get the ID of a log by name
#' 
#' @param logName Name of the log
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' 
#' @return A string with the log ID, or IDs in case of multiple identical names

getLogIdByName <- function(lanaUrl, lanaToken, logName){
  
  userLogs <- lanar::getLogs(lanaUrl, lanaToken)
  logId <- max(userLogs[grepl(logName, userLogs$name, fixed=T), ]$id)
  return(logId)
}
