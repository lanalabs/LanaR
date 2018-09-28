# lanar - R API for LANA Process Mining
This package provides an R API for [LANA Process Mining](https://www.lana-labs.com/en/). 

**Attention**: This package is still in alpha state. Functions and parameters may be renamed and changed at any time.

# Setup
In order to use `lanar` you need to manually install the package `xml2` using `install.packages("xml2")`.

# Usage
First upload your event logs to your Lana software. Then enter the log name as a parameter to the different functions as they will use the last uploaded log of that name. Look also at the examples.

downloadExample() - downloads an example event log to your working direction


getLogs() - shows you a list of your uploaded event logs in Lana.


discoveredModel(logName) - gives statistics about predecessor and successor followers.

example:
discoveredModel("Incident_withImpactAttributes.csv")


activityPerformance(logName) - gives statistics about activities like minimum/maximum duration, average/median/total duration, standard deviation and frequency.

example:
activityPerformance("Incident_withImpactAttributes.csv")


aggregate(logName, xDimension, yDimension, zDimension) - aggregation with different dimensions for example:time, attribute, frequency, average duration, median duration, total duration.

examples:
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency")
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=dayOfWeek", yDimension = "avgDuration")
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byHour", yDimension = "medianDuration")
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "totalDuration")
aggregate("Incident_withImpactAttributes.csv", xDimension = "byAttribute=Activity", yDimension = "frequency", zDimension = "byAttribute=Stoerung vorhanden")
