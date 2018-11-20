# lanar - R API for LANA Process Mining
This package provides an R API for [LANA Process Mining](https://www.lana-labs.com/en/). 

**Attention**: This package is still in alpha state. Functions and parameters may be renamed and changed at any time.

# Setup
In order to use `lanar` you need to manually install the packages `xml2`, `plyr` using `install.packages("xml2", "plyr")`.

# Usage
First upload your event logs to your Lana software. Then enter the log name as a parameter to the different functions as they will use the last uploaded log of that name. Look also at the examples.

## Download Sample Event Log
downloads an example event log to your working directory

```
downloadExample()
```

## A list with all your uploaded logs
```
getLogs()
```

| id                | owner         | name                             | date                       | timezone       |
| -------------     | ------------- | -------------                    | -------------              | -------------  |
| 546               | 34            | Incident_withImpactAttributes.csv| 2018-09-28T13:19:26.175Z   | Etc/GMT        |
| 547               | 34            | SalesProcess.csv                 | 2018-09-13T13:27:50.414Z   | Europe/Berlin  |

## Filter and performance / followers statistics
```
filter(logName)
```
#### Example
```
discoveredModel("Incident_withImpactAttributes.csv")
```
## Activity Performance Statistics
gives statistics about activities such as minimum/maximum duration, average/median/total duration, standard deviation and frequency.
```
activityPerformance(logName)
```
#### Example
```
activityPerformance("Incident_withImpactAttributes.csv")
```

| .id                           | frequency       | totalDuration   | minDuration     | maxDuration   | avgDuration    | standardDeviation| median         |
| -------------                 | -------------   | -------------   | -------------   | ------------- | -------------  | -------------    | -------------  |
| Initial diagnosis             | 2337            | 1270500000      | 0               | 1320000       | 543645.7       | 238600.5         | 540000         |
| Functional escalation         | 851             | 254640000       | 0               | 660000        | 299224.4       | 118595.1         | 300000         |
|  Incident closure             | 2000            | 1582380000      | 0               | 2280000       | 791190.0       | 361650.6         | 780000         |
|  Incident classification      | 2000            | 497580000       | 0               | 900000        | 248790.0       | 166740.6         | 240000         |
|  Investigation and diagnosis  | 851             | 16600800000     | 0               | 60240000      | 19507403.1     | 12030401.3       | 19020000       |
|  Incident logging             | 2000            | 1079040000      | 0               | 2040000       | 539520.0       | 389029.4         | 480000         |
|  Resolution and recovery      | 1635            | 2944380000      | 0               | 6240000       | 1800844.0      | 1111854.5        | 1800000        |


## Aggregation
Aggregation with different dimensions such as: time, attribute, frequency, average duration, median duration, total duration.
```
aggregate(logName, xDimension, yDimension)
```
#### Aggregation Dimensions

| Time Dimension         | what it does              |
| -------------          | -------------             |
| byTime=byMonth         | aggregate by month        |
| byTime=dayOfWeek       | aggregate by day of week  |
| byTime=byHour          | aggregate by day of week  |
|                        |                           |


| Aggregation Dimension     | what it does                                        |
| -------------             | -------------                                       |
| frequency                 | counts the cases given time dimension               |
| avgDuration               | outputs the average duration given time dimension   |
| medianDuration            | outputs the median duration given time dimension    |
| totalDuration             | outputs the total duration given time dimension     |


#### Optional Dimension Examples

##### TraceFilterSequence: 

###### attributeFilter

traceFilterSequence":[{"type":"attributeFilter","attributeName":"```attribute name```","values":["```first value```","```second value```"],"inverted":```false or true```},{"max":```max number```,"min":```min number```,"type":"variantSliderFilter"}]






#### Examples
```
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency")
```

| byTime=byMonth    | frequency     | Case Count    |
| -------------     | ------------- | ------------- |
| Jan 2016          | 2000          | 2000          |

```
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=dayOfWeek", yDimension = "avgDuration")
```

| byTime=dayOfWeek  | avgDuration   | Case Count    |
| -------------     | ------------- | ------------- |
| Monday            | 42516060      | 2000          |

```
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byHour", yDimension = "medianDuration")
```

| byTime=byHour     | medianDuration   | Case Count    |
| -------------     | -------------    | ------------- |
| 09                | 42516060         | 2000          |
| 10                | 42516060         | 2000          |
| 11                | 42516060         | 2000          |
| 12                | 42516060         | 2000          |
| 13                | 42516060         | 2000          |
| 14                | 42516060         | 2000          |
| 15                | 42516060         | 2000          |
| 16                | 42516060         | 2000          |

```
aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "totalDuration")
```

| byTime=byMonth    | totalDuration | Case Count    |
| -------------     | ------------- | ------------- |
| Jan 2016           | 85032120000  | 2000          |

