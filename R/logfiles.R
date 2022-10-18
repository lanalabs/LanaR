#' Get all logs
#'
#' @param host URL of the instance that Appian Process Mining is running on
#' @param token Appian Process Mining API token
#' 
#' @return Data Frame with all logs owned by or shared with the authenticated user

getLogs <- function(host, token) {
  
  response <- httr::GET(
    paste0("https://", host, "/api/logs"),
    httr::add_headers(Authorization = token))
  lanar::checkHttpErrors(response)
  content <- jsonlite::fromJSON(httr::content(response, encoding = "UTF-8", type = "text"))
  return(content)
}


#' Get ID of authenticated user
#' 
#' @param host URL of the instance Appian Process Mining is running on
#' @param token Appian Process Mining API token
#' 
#' @return A string with the user ID

getUserId <- function(host, token) {
  
  response <- httr::GET(
    paste0("https://", host, "/api/users/by-token"),
    httr::add_headers(Authorization = token)  )
  lanar::checkHttpErrors(response)
  content <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(content$id)
}


#' Get the ID of a log by name
#' 
#' @param logName Name of the log
#' @param host URL of the instance that Appian Process Mining is running on
#' @param token Appian Process Mining API token
#' 
#' @return A string with the log ID, or IDs in case of multiple identical names

getLogIdByName <- function(host, token, logName){
  
  userLogs <- lanar::getLogs(host, token)
  logId <- userLogs$id[grep(logName, userLogs$name)]
  return(logId)
}
