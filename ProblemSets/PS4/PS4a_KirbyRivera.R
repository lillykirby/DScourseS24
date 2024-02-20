system('wget dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')
system('cat dates.json')

library(tidyverse)
library(jsonlite)
library(dplyr)

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

class(mydf$date)
head(mydf)