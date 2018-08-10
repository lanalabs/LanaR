checkHttpErrors <- function(response) {

  if(response["status_code"] != 200){
    stop(paste0("Error: ", content(response), "(", http_status(response)$message, ")"))
  }
}
