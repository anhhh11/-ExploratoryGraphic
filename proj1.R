library("data.table")
library("date")
library("lubridate")
dataUrl  <-  "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dataFilename <- "household_power_consumption.zip"
download.file(dataUrl,method="curl",mode="b",destfile=dataFilename)
unzip(zipfile=dataFilename)
list.files(".","house")
dataFilename <- "household_power_consumption.txt"
preview <- read.csv(dataFilename,nrow=2,header=TRUE)
preview1 <- read.csv(dataFilename,nrow=2,header=TRUE,sep=";")
str(preview1)
tryReadData <- fread(dataFilename,sep=";",showProgress=TRUE,
              colClasses=c("date","time",rep("numeric",7)),
              nrow=10)
tryReadMoreData <- fread(dataFilename,sep=";",showProgress=TRUE,
                     colClasses=c("date","time",rep("numeric",7)))
#WARNING ? character
read.csv(dataFilename,nrow=1,skip=6840,header=FALSE)
data <- fread(dataFilename,sep=";",na.strings=c("?"),header=TRUE)
data$Date <- as.date(data$Date,"dmy")
data$Date <- as.Date(data$Date)
