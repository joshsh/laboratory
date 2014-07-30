

toTime <- function(timestamps) {
    as.POSIXlt(ISOdatetime(1970,1,1,0,0,0, tz="GMT") + timestamps/1000, tz="America/New_York")
}


########################################
# plot Gaussian sensor data

data <- read.table(file("/tmp/monitron.log"), header=FALSE)
time <- data$V1
mean <- data$V6
var <- data$V7


plot(x=toTime(time), y=mean, type="l", xlab="time", ylim=c(0, max(mean)))
#plot(x=toTime(time), y=var, type="l", ylim=c(0, max(var)))


########################################
# plot Boolean sensor data

data <- read.table(file("/tmp/monitron.log"), header=FALSE)
df <- data.frame(V4=c("false", "true"), int=c(0, 1))
data2 <- merge(data, df, by="V4")
time <- data2$V1
value <- data2$int

plot(x=toTime(time), y=value, ylim=c(0, 1), type="p")


########################################
# plot sound level

data <- read.table(file("/tmp/monitron.log"), header=FALSE)
time <- data$V1
vol <- data$V5 - data$V4

plot(x=toTime(time), vol, type="l", xlab="time")

sv = smooth.spline(vol)
plot(x=toTime(time), sv$y, type="l", xlab="time")


########################################
# compare thermometers

bosch <- read.table(file("/tmp/bosch.log"), header=FALSE)
maxdetect <- read.table(file("/tmp/maxdetect.log"), header=FALSE)

# not necessarily exactly the same for both time series
bosch.time <- bosch$V1
maxdetect.time <- maxdetect$V1

bosch.mean <- bosch$V6
maxdetect.mean <- maxdetect$V6

ymin <- min(bosch.mean, maxdetect.mean)
ymax <- max(bosch.mean, maxdetect.mean)

# visually, they are very close except that the maxdetect
# consistently reads around 0.5 degrees (C) lower
plot(x=toTime(bosch.time), y=bosch.mean, ylim=c(ymin, ymax), type="l", xlab="time", col="blue")
lines(x=toTime(maxdetect.time), y=maxdetect.mean + 0.5, col="red")

abs(bosch.mean - maxdetect.mean)


########################################
# compare dust with temperature

bosch <- read.table(file("/tmp/bosch.log"), header=FALSE)
sharp <- read.table(file("/tmp/sharp.log"), header=FALSE)

bosch.time <- bosch$V1
sharp.time <- sharp$V1

bosch.mean <- bosch$V6
sharp.mean <- sharp$V6

bosch.min <- min(bosch.mean); bosch.max <- max(bosch.mean);
sharp.min <- min(sharp.mean); sharp.max <- max(sharp.mean);

newsharp.mean <- bosch.min + (bosch.max-bosch.min)/(sharp.max-sharp.min)*(sharp.mean-sharp.min)

# apparent dust level follows temperature pretty closely when there is little dust.
# This can probably be considered an artifact.
# Perhaps the accuracy of the sensor can be enhanced by subtracting temperature.
plot(x=toTime(bosch.time), y=bosch.mean, ylim=c(bosch.min, bosch.max), type="l", xlab="time", col="blue")
lines(x=toTime(sharp.time), y=newsharp.mean, col="red")
