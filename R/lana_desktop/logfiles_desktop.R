#' Get all logs
#' @return data frame with all log files owned by or shared with the authenticated user
#' @name getLogs
getLogs_desktop <- function() {

  logData <- httr::GET(paste0(lanaApiUrl, "/api/users/0/logs"))

  checkHttpErrors(logData)

  jsonlite::fromJSON(
    httr::content(logData, encoding = "UTF-8", type = "text")
  )

}

