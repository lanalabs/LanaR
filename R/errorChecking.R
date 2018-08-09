checkHttpErrors <- function(response) {
  if(response["status_code"] == 400) {
    stop(httr::paste0("Invalid rqBody JSON " + "Statuscode: ", response["status_code"]))
  }

  if(response["status_code"] == 403) {
    stop(paste0("Forbidden. Statuscode: ", response["status_code"]))
  }

  if(response["status_code"] == 404) {
    stop(paste0("Not Found. Statuscode: ", response["status_code"]))
  }

  if(response["status_code"] == 500) {
    stop(paste0("Backend Error. Statuscode: ", response["status_code"]))
  }

  if(response["status_code"] != 200){
    stop(paste0("Unknown Error. Statuscode: ", response["status_code"]))
  }
}
