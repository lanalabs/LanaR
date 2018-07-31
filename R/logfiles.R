getLogs <- function() {

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")

  myUserLogs <- jsonlite::fromJSON(
    content(GET(paste0(lanaApiUrl, "/api/devtouserLogs"), add_headers(Authorization = lanaAuthorization)),  encoding = "UTF-8", type = "text")
  )
}
