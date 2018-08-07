download <- function(csvUrl){

  #csvFile <- 'https://lana-labs.com/examples/Incident_withImpactAttributes/'
  #read.csv(csvFile)

url <- csvUrl
filename <- substr(csvUrl, 31, 100)
workingDirectory <- getwd()

destination <- paste(workingDirectory + filename)

download.file(csvUrl, destfile = workingDirectory)
}
