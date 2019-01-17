checkHttpErrors <- function(response) {

  if(response["status_code"] != 200) {
      stop(paste0(httr::content(response), " \nHTTP error details = ", httr::http_status(response)$message))
  }

  if(isEmptyLog(response)) {
      warning("No response data. Your filter settings led to an empty log.\n")
  }
}

isEmptyLog <- function(response) {
  responseData <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  if(length(responseData$chartValues)==0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
