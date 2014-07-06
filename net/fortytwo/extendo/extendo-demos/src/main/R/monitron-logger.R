

toTime <- function(timestamps) {
    as.POSIXlt(ISOdatetime(1970,1,1,0,0,0, tz="GMT") + timestamps/1000, tz="America/New_York")
}


########################################
# plot Gaussian sensor data

data <- read.table(file("/tmp/monitron.log"), header=FALSE)
time <- data$V1
mean <- data$V6
var <- data$V7

plot(mean, type="l", ylim=c(0, max(mean)))
plot(var, type="l", ylim=c(0, max(var)))

plot(x=toTime(time), y=mean, type="l", xlab="time", ylim=c(0, max(mean)))


########################################
# plot Boolean sensor data

data <- read.table(file("/tmp/monitron.log"), header=FALSE)
df <- data.frame(V4=c("false", "true"), int=c(0, 1))
data2 <- merge(data, df, by="V4")
time <- data2$V1
value <- data2$int

plot(x=toTime(time), y=value, ylim=c(0, 1), type="p")
