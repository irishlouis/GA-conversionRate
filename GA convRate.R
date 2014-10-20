library(dplyr)
library(lubridate)
library(ggplot2)
library(lattice)

setwd("C:/Users/Louis/SkyDrive/Documents/R-files/GA ConvRate")
data <- read.table("data.csv", sep = ","
    , skip = 6, header = T, stringsAsFactors = F)
head(data)
data$Date.Range <- dmy_hm(data$Date.Range)
data$hour <- hour(data$Date.Range)
data$date <- as.Date(data$Date.Range)
class(data$date)

data$day <- wday(data$Date.Range, label = T)
data$Segment <- as.factor(data$Segment)

any(is.na(data))
head(data)
convRate <- select(data, -Hour.Index) %>%
  group_by(Date.Range) %>% mutate(rate = (Sessions/sum(Sessions)) ) %>%
  filter(Segment == "ROI Product Confirmation")

summaryData <- select(convRate, Sessions, hour, day, rate) %>%
  group_by(day, hour) %>% 
  summarize(meanRate = mean(na.omit(rate)), sdRate = sd(na.omit(rate)))

#lattice plots for mean hourly conversion rate of prospect sessions by day of week
xyplot(meanRate ~ hour|day, data = summaryData)




