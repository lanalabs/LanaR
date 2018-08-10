checkHttpErrors <- function(response) {

  if(response["status_code"] != 200) {
    if(!isEmptyLog(response)) {
      stop(paste0(httr::content(response), " \nHTTP error details = ", httr::http_status(response)$message))
    } else {
      warning("No response data. Your filter settings led to an empty log.\n")
    }
  }
}

isEmptyLog <- function(response) {
  return(any(httr::content(response) == "EmptyLog"))
}
