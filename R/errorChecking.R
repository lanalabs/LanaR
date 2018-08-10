checkHttpErrors <- function(response) {

  if(response["status_code"] != 200){
    stop(paste0(httr::content(response), " \nHTTP error details = ", httr::http_status(response)$message))
  }
}
