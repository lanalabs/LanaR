# LanaR: API for LANA Process Mining
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
#### Example Usage
```R
# With empty trace filter sequence
discoveredModel(lanaURL, lanaToken, logId)

# With non-empty trace filter sequence
discoveredModel(lanaURL, lanaToken, logId, traceFilterSequence = "trace sequence")
```
Optional Parameters:
* `traceFilterSequence` integrate filter from lana (default = '[]')
* `runConformance` include conformance data (default = 'true')
* `computeAttributeCounts` (default = 'true')
* `renderDiscoveredModel` (default = 'false')

## Aggregation
Aggregation with different dimensions such as: time, attribute, frequency, average duration, median duration, total duration.
```
aggregate(lanaURL, lanaToken, logId, xDimension, yDimension)
```
Optional Parameters:
* `zDimension` (default = 'null')
* `aggrLevel` (default = 'traces')
* `followers` (default = 'null')
* `type` (default = 'aggregation')
* `cache` (default = 'null')
* `maxValueAmount` (default = 5)
* `activityExclusionFilter` hide activities in aggregation (default = '[]')
* `traceFilterSequence` integrate filter from lana (default = '[]' no filter applied)
* `limit` (default = 10)
* `page` (default = 1)
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


#### Advanced Optional Dimensions

If you are more experienced with Lana, you can insert any kind of filter that you use in Lana into your aggregation or discovered model functions, by simply adding a tracefilter sequence.

##### TraceFilterSequence: 

###### Variant Filter

[{"max":```max variants```,"min":```min variants```,"type":"variantSliderFilter"}]

###### Attribute Filter

[{"type":"attributeFilter","attributeName":"```attribute name```","values":["```first value```","```second value```"],"inverted":```true or false```}]

###### Case Duration Filter

[{"type":"traceDurationFilter","minTraceDuration":```min duration in miliseconds```,"maxTraceDuration":```max duration in miliseconds```}]

###### Timespan Filter

[{"from":```start time in miliseconds```,"to":```ènd time in miliseconds```,"startInRange":true,"endInRange":true,"inverted":false,"type":"timeRangeFilter"}]

###### Activity Filter

[{"type":"activityFilter","activity":"```activity1```","inverted":```true or false```},{"type":"activityFilter","activity":"```activity 2```","inverted":```true or false```}]

###### Follower Filter

[{"direct":```true or false```,"pre":"```from```","succ":"```to```","useDuration":```true or false```,"inverted":```true or false```,"type":"followerFilter"}]

###### Endpoint Filter

[{"type":"endpointFilter","activities":["```activity1```","```activity2```"],"inverted":```true or false```}]

###### Hide Activities Filter

[{"type":"endpointFilter","activities":["```activity1```","```activity2```"],"inverted":```true or false```}]


#### Examples
```
aggregate(lanaURL, lanaToken, logId, xDimension = "byTime=byMonth", yDimension = "frequency")
```

| byTime=byMonth    | frequency     | Case Count    |
| -------------     | ------------- | ------------- |
| Jan 2016          | 2000          | 2000          |

```
aggregate(lanaURL, lanaToken, logId, xDimension = "byTime=dayOfWeek", yDimension = "avgDuration")
```

| byTime=dayOfWeek  | avgDuration   | Case Count    |
| -------------     | ------------- | ------------- |
| Monday            | 42516060      | 2000          |

```
aggregate(lanaURL, lanaToken, logId, xDimension = "byTime=byHour", yDimension = "medianDuration")
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
aggregate(lanaURL, lanaToken, logId, xDimension = "byTime=byMonth", yDimension = "totalDuration")
```

| byTime=byMonth    | totalDuration | Case Count    |
| -------------     | ------------- | ------------- |
| Jan 2016           | 85032120000  | 2000          |


```
aggregate(lanaURL, lanaToken, logId, xDimension = "byTime=byMonth", yDimension = "totalDuration", traceFilterSequence = [{"max":```max variants```,"min":```min variants```,"type":"variantSliderFilter"}])
```

```
aggregate(lanaURL, lanaToken, logId, xDimension = "byTime=byMonth", yDimension = "totalDuration", traceFilterSequence = [{"max":```max variants```,"min":```min variants```,"type":"variantSliderFilter"}, {"type":"attributeFilter","attributeName":"```attribute name```","values":["```first value```","```second value```"],"inverted":```false or true```}])
```




