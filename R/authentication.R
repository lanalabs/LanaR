authenticate <- function(url, token) {
  set_renv("LANA_URL" = url)
  set_renv("LANA_TOKEN" = token)
}

checkAuthentication <- function() {
  if(is.null(Sys.getenv("LANA_URL")) || Sys.getenv("LANA_URL") == "") {
    stop("Authentication error: You haven't defined a URL for LANA. Please use `authenticate()`.")
  }

  if(is.null(Sys.getenv("LANA_TOKEN")) || Sys.getenv("LANA_TOKEN") == "") {
    stop("Authentication error: You haven't defined a TOKEN for LANA. Please use `authenticate()`")
  }
}
