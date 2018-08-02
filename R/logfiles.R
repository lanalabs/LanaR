getLogs <- function() {

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  jsonlite::fromJSON(
    content(GET(paste0(lanaApiUrl, "/api/users/", lanaUserId, "/logs"), add_headers(Authorization = lanaAuthorization)),  encoding = "UTF-8", type = "text")
  )
}
