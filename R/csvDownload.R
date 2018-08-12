#' @title Download example CSV file
#' @description Download the incident example CSV file from Lana Labs' homepage
#' @name downloadIncidentExample
downloadIncidentExample <- function(){

  csvUrl <- 'https://lana-labs.com/examples/Incident_withImpactAttributes.csv'

  url <- csvUrl
  filename <- substr(csvUrl, 31, 100)
  workingDirectory <- getwd()

  destination <- paste0(workingDirectory,filename)

  download.file(csvUrl, destfile = destination)
}
