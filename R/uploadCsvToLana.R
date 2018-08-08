#' @title \code Upload Csv Log without Case Attributes
#' @description uploads a csv log file without case attributes to Lana
#' @param logName, logSemantics
#' @name uploadWithoutCase

uploadWithoutCase <- function(logName, eventSemanticsJson){

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")


  responseLogCreation <- httr::POST(paste0(lanaApiUrl, "/api", "logs/csv"),
                              body = list(eventSemantics=eventSemanticsJson, file=httr::upload_file(logName, "text/csv")),
                              encode = "multipart",
                              httr::add_headers(Authorization = lanaAuthorization),
                              httr::verbose(data_out = F)
)

  if(responseLogCreation["status_code"] == 400) {
    stop(httr::paste0("Invalid rqBody JSON " + "Statuscode: ", responseLogCreation["status_code"]))
  }

  else if(responseLogCreation["status_code"] == 403) {
    stop(paste0("Forbidden. Statuscode: ", responseLogCreation["status_code"]))
  }

  else if(responseLogCreation["status_code"] == 404) {
    stop(paste0("Not Found. Statuscode: ", responseLogCreation["status_code"]))
  }

  else if(responseLogCreation["status_code"] == 500) {
    stop(paste0("Backend Error. Statuscode: ", responseLogCreation["status_code"]))
  }

  else if(responseLogCreation["status_code"] != 200){
    stop(paste0("Unknown Error. Statuscode: ", responseLogCreation["status_code"]))
  }


}
