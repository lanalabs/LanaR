uploadWithoutCase <- function(logName, logSemantics){

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  csvId <- content(POST(paste0(lanaApiUrl, "uploadLogCsv"),
                        body = list(file=upload_file(logName)),
                        add_headers(Authorization = lanaAuthorization)
  ))$id

  stopifnot(!is.null(csvId))


}
