to.time <- function(timestamps) {
    as.POSIXlt(ISOdatetime(1970,1,1,0,0,0, tz="GMT") + timestamps/1000, tz="America/New_York")
}

load.gaussian <- function(dir, file.name) {
  path <- paste(dir, "/", file.name, sep="")
  # note: stddev for these early samples may not have been correct
  df <- read.table(file(path), header=FALSE,
    col.names=c("from.t", "to.t", "n.samples", "min", "max", "mean", "stddev"))
  subset(df, from.t > 1400000000000)
}

load.boolean <- function(dir, file.name) {
  path <- paste(dir, "/", file.name, sep="")
  df <- read.table(file(path), header=FALSE, col.names=c("from.t", "to.t", "n.samples", "value"))
  subset(df, from.t > 1400000000000)
}

first.break <- function(data) {
  n <- nrow(data)
  df1 <- data[1:(n-1),]
  df2 <- data[2:n,]
  d <- df2$from.t - df1$from.t
  breaks <- subset(data.frame(index=c(1:length(d)), diff=d), diff < 0)
  breaks[1,]$index
}

to.first.break <- function(data) {
  data[1:(first.break(data)-1),]
}

plot.gaussian <- function(data) {
  plot(data$mean, x=data$from.t, type="l")
}

gaussian.simple <- function(name, data) {
  data.frame(variable=name, t=data$from.t, min=data$min, max=data$max, value=data$mean)
}

boolean.simple <- function(name, data) {
  data.frame(variable=name, t=data$from.t, min=data$value, max=data$value,
  value=sapply(data$value, function(v) { if (v == "true") 1 else 0 }))
}

norm <- function(data.simple) {
  mn <- min(data.simple$value)
  mx <- max(data.simple$value)
  data.frame(variable=data.simple$id, t=data.simple$t, value=((data.simple$value-mn)/(mx-mn)))
}

shorter <- function(data.simple) {
  subset(data.simple, t > start.t & t < end.t)
}


########################################

dir <- "/tmp/monitron"

bosch.baro.file <- "bosch-bmp085_1_barometer.log"
bosch.thermo.file <- "bosch-bmp085_1_thermometer.log"
photo.file <- "generic-photoresistor_1.log"
audio.file <- "knowles-md9745apz-f_1.log"
maxdetect.hygro.file <- "maxdetect-rht03_1_hygrometer.log"
maxdetect.thermo.file <- "maxdetect-rht03_1_thermometer.log"
vibro.file <- "murata-7bb-20-6l0_1.log"
dust.file <- "sharp-gp2y1010au0f_1.log"
motion.file <- "hanse-se10_1.log"

bosch.baro.data <- load.gaussian(dir, bosch.baro.file)
bosch.thermo.data <- load.gaussian(dir, bosch.thermo.file)
photo.data <- load.gaussian(dir, photo.file)
audio.data <- load.gaussian(dir, audio.file)
maxdetect.hygro.data <- load.gaussian(dir, maxdetect.hygro.file)
maxdetect.thermo.data <- load.gaussian(dir, maxdetect.thermo.file)
vibro.data <- load.gaussian(dir, vibro.file)
dust.data <- load.gaussian(dir, dust.file)
motion.data <- load.boolean(dir, motion.file)

bosch.baro.data <- to.first.break(bosch.baro.data)
bosch.thermo.data <- to.first.break(bosch.thermo.data)
photo.data <- to.first.break(photo.data)
audio.data <- to.first.break(audio.data)
maxdetect.hygro.data <- to.first.break(maxdetect.hygro.data)
maxdetect.thermo.data <- to.first.break(maxdetect.thermo.data)
vibro.data <- to.first.break(vibro.data)
dust.data <- to.first.break(dust.data)
motion.data <- to.first.break(motion.data)

##########

bosch.baro.simple <- gaussian.simple("bosch.baro", bosch.baro.data)
bosch.thermo.simple <- gaussian.simple("bosch.thermo", bosch.thermo.data)
photo.simple <- gaussian.simple("photo", photo.data)
audio.simple <- gaussian.simple("audio", audio.data)
maxdetect.hygro.simple <- gaussian.simple("maxdetect.hygro", maxdetect.hygro.data)
maxdetect.thermo.simple <- gaussian.simple("maxdetect.thermo", maxdetect.thermo.data)
vibro.simple <- gaussian.simple("vibro", vibro.data)
dust.simple <- gaussian.simple("dust", dust.data)
motion.simple <- boolean.simple("motion", motion.data)

all.simple <- rbind(bosch.baro.simple, bosch.thermo.simple, photo.simple, audio.simple,
  maxdetect.hygro.simple, maxdetect.thermo.simple, vibro.simple, dust.simple, motion.simple)

ggplot(data = all.simple, aes(x = t, y = value, color = id)) +
  geom_line() +
  #scale_color_manual(values=c("black", "#CC6666", "#66CC66", "#9999CC"), labels=c("|a|", expression("a"["x"]), expression("a"["y"]), expression("a"["z"]))) +
  xlab("time (ms)") +
  ylab("value")

##########

# see all series together (normalized so they don't flatten out)
all.simple.norm <- rbind(
  norm(bosch.baro.simple), norm(bosch.thermo.simple),
  norm(photo.simple), norm(audio.simple),
  norm(maxdetect.hygro.simple), norm(maxdetect.thermo.simple),
  norm(vibro.simple), norm(dust.simple), norm(motion.simple))
ggplot(data = all.simple.norm, aes(x = t, y = value, color = variable)) +
  geom_line() +
  #scale_color_manual(values=c("black", "#CC6666", "#66CC66", "#9999CC"), labels=c("|a|", expression("a"["x"]), expression("a"["y"]), expression("a"["z"]))) +
  xlab("time (ms)") +
  ylab("value")

##########

library(ggplot2)
library(gtable)
library(gridExtra)

offset <- 9

day <- 1000*60*60*24
start.t <- as.numeric(ISOdatetime(2014,7,2,4,0,0, tz="GMT"))*1000 + offset*day
end.t <- start.t + 1*day

bosch.baro.shorter <- shorter(bosch.baro.simple)
bosch.thermo.shorter <- shorter(bosch.thermo.simple)
photo.shorter <- shorter(photo.simple)
audio.shorter <- shorter(audio.simple)
maxdetect.hygro.shorter <- shorter(maxdetect.hygro.simple)
maxdetect.thermo.shorter <- shorter(maxdetect.thermo.simple)
vibro.shorter <- shorter(vibro.simple)
dust.shorter <- shorter(dust.simple)
motion.shorter <- shorter(motion.simple)

dated <- function(data.simple) {
  data.frame(data.simple, date=to.time(data.simple$t))
}

plot.simple <- function(data.simple, ylab, color, bottom) {
  df <- dated(data.simple)
  if (bottom) {
    ggplot(data = df, aes(x=date, y=value, ymin=min, ymax=max, color = variable)) +
      geom_line() +
      #geom_ribbon(alpha=0.5) +
      scale_color_manual(values=c(color), guide=FALSE) +
      theme_bw() +
      ylab(ylab) +
      theme(axis.title.x=element_blank(), plot.margin=unit(c(0,0.5,0,0.5), "cm"))
  } else {
    ggplot(data = df, aes(x=date, y=value, ymin=min, ymax=max, color = variable)) +
      geom_line() +
      #geom_ribbon(alpha=0.5) +
      scale_color_manual(values=c(color), guide=FALSE) +
      theme_bw() +
      ylab(ylab) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin=unit(c(0,0.5,0,0.5), "cm"))
  }
}

# note: exclude motion, as it seems to have merely floated
#       exclude both audio and vibro, as they were somehow coupled to light level
#       exclude one of the thermometers, as we don't need both in this figure
#       exclude dust level because, when it varies at all, it seems to be coupled with temperature
p1 <- plot.simple(photo.shorter, "illum.", "#66CC66", FALSE)
p2 <- plot.simple(maxdetect.hygro.shorter, "humid. (%)", "#9999CC", FALSE)
p3 <- plot.simple(bosch.thermo.shorter, "temp. (°C)", "#CC6666", FALSE)
p4 <- plot.simple(bosch.baro.shorter, "press. (Pa)", "#A0A0A0", TRUE)
#plot.simple(audio.shorter, "sound level (uncalibrated)", "black", FALSE),
#plot.simple(maxdetect.thermo.shorter, "temp. (°C)", "black", FALSE),
#plot.simple(vibro.shorter, "vibration level (uncalibrated)", "black", FALSE),
#plot.simple(dust.shorter, "dust level (uncalibrated)", "black", FALSE),
#plot.simple(motion.shorter, "motion", "black", FALSE)

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3], g3$widths[2:3], g4$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
g3$widths[2:3] <- maxWidth
g4$widths[2:3] <- maxWidth

g4$heights <- g1$heights

#pdf("/tmp/graphic.pdf", width=6.25, height=3.75)
#par(mar=c(0,0,0,0))
  grid.arrange(ncol=1,padding=2, g1,g2,g3,g4)
#dev.off()


########################################
# compare thermometers

# not necessarily exactly the same for both time series
bosch.time <- bosch.thermo.data$from.t
maxdetect.time <- maxdetect.thermo.data$from.t

bosch.mean <- bosch.thermo.data$mean
maxdetect.mean <- maxdetect.thermo.data$mean

ymin <- min(bosch.mean, maxdetect.mean)
ymax <- max(bosch.mean, maxdetect.mean)

# visually, they are very close except that the maxdetect
# consistently reads around 0.5 degrees (C) lower
plot(x=to.time(bosch.time), y=bosch.mean, ylim=c(ymin, ymax), type="l", xlab="time", col="blue")
lines(x=to.time(maxdetect.time), y=maxdetect.mean + 0.5, col="red")

abs(bosch.mean - maxdetect.mean)


########################################
# compare dust with temperature

bosch.mean <- bosch.thermo.data$mean
sharp.mean <- dust.data$mean

bosch.min <- min(bosch.mean); bosch.max <- max(bosch.mean);
sharp.min <- min(sharp.mean); sharp.max <- max(sharp.mean);

newsharp.mean <- bosch.min + (bosch.max-bosch.min)/(sharp.max-sharp.min)*(sharp.mean-sharp.min)

# apparent dust level follows temperature pretty closely when there is little dust.
# This can probably be considered an artifact.
# Perhaps the accuracy of the sensor can be enhanced by subtracting temperature.
plot(x=to.time(bosch.time), y=bosch.mean, ylim=c(bosch.min, bosch.max), type="l", xlab="time", col="blue")
lines(x=to.time(sharp.time), y=newsharp.mean, col="red")
