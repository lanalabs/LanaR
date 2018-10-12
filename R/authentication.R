#' @name authenticate
#' a method, that authenticates the user
authenticate <- function(url, token) {

  userInfo <- httr::GET(paste0(url, "/api/users/by-token"), httr::add_headers(Authorization = token))

  ## TOOo check for 404 => Wrong URL
  if(userInfo$status_code != 200) {
    stop(paste0("Wrong user credentials. Error code = ", userInfo$status_code, " - ", httr::content(userInfo)$message))
  }

  # Store token, url and user id in environment so it can be used by all other API calls
  set_renv("LANA_URL" = url)
  set_renv("LANA_TOKEN" = token)

  userId = jsonlite::fromJSON(httr::content(userInfo, encoding = "UTF-8", type = "text"))$id
  set_renv("LANA_USERID" = userId)

}

checkAuthentication <- function() {
  if(is.null(Sys.getenv("LANA_URL")) || Sys.getenv("LANA_URL") == "") {
    stop("Authentication error: You haven't defined a URL for LANA. Please use `authenticate()`.")
  }

  if(is.null(Sys.getenv("LANA_TOKEN")) || Sys.getenv("LANA_TOKEN") == "") {
    stop("Authentication error: You haven't defined a TOKEN for LANA. Please use `authenticate()`")
  }
}
