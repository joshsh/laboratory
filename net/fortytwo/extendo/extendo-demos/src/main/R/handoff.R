#install.packages("rgl")
library(rgl)

mag <- function(p) { sqrt(p$x^2 + p$y^2 + p$z^2) }
norm <- function(p) { p / mag(p) }
mean3d <- function(p) { data.frame(x=mean(p$x), y=mean(p$y), z=mean(p$z))}

# min.amp: minimum trough-to-peak amplitude of features of interest
find.peaks <- function(x, min.amp) {
    peaks <- c()
    prev.x <- 0
    prev.rising <- TRUE
    ref.x <- 0
    ref.i <- 0
    high <- FALSE
    last.added <- FALSE
    n <- length(x)
    for (i in 1:n) {
        xi <- x[i]
        rising <- xi >= prev.x
        if (rising) {
            if (!prev.rising) { # local minimum
                if (high) {
                    if (ref.x - prev.x >= min.amp) {
                        peaks <- c(peaks, ref.i)
                        ref.x <- prev.x
                        high <- FALSE
                    }
                } else if (prev.x < ref.x) {
                    ref.x <- prev.x
                }
            }
        } else {
            if (prev.rising) { # local maximum
                if (high) {
                    if (xi > ref.x) {
                        ref.x <- prev.x
                        ref.i <- i-1
                    }
                } else {
                    if (xi - ref.x >= min.amp) {
                        ref.i <- i-1
                        ref.x <- prev.x
                        high <- TRUE
                    }
                }
            }
        }
        prev.x <- xi
        prev.rising <- rising
    }

    peaks
}

a.of <- function(series) {
    data.frame(x=series$V2,y=series$V3,z=series$V4)
}

running.average <- function(x, len) {
    buffer <- x[1:len]
    sum <- sum(buffer)
    n <- length(x)
    j <- 0
    result <- c()
    for (i in 1:n) {
        sum <- sum - buffer[j+1] + x[i]
        buffer[j+1] <- x[i]
        result <- c(result, sum/len)
        j <- (j + 1) %% len
    }
    result
}

running.average.3d <- function(motion, len) {
    a <- a.of(motion)
    data.frame(x=running.average(a$x, len), y=running.average(a$y, len), z=running.average(a$z, len))
}

# sampling rate in Hz
sampling.rate <- function(motion) {
    times <- motion$V1*1000000
    t1 <- times[1:(length(times)-1)]
    t2 <- times[2:length(times)]
    diff <- t2 - t1

    data.frame(local=1/mean(diff), local.sd=sd(1/diff),
        global=nrow(motion)/(max(times)-min(times)))
}

lowpass <- function(x, dt, rc) {
    n <- length(x)
    y <- 1:n
    a <- dt / (rc + dt)
    y[1] <- x[1]
    for (i in 2:n) {
        y[i] <- a * x[i] + (1-a) * y[i-1]
    }
    y
}

highpass <- function(x, dt, rc) {
    n <- length(x)
    y <- 1:n
    a <- rc / (rc + dt)
    y[1] <- x[1]
    for (i in 2:n) {
        y[i] <- a * y[i-1] + a * (x[i] - x[i-1])
    }
    y
}

bandpass <- function(x, dt, rc.low, rc.high) {
    y.lp <- lowpass(x, dt, rc.low)
    highpass(y.lp, dt, rc.high)
}

bandpass.3d <- function(df, dt, rc.low, rc.high) {
    data.frame(x=bandpass(df$x, dt, rc.low, rc.high),
        y=bandpass(df$y, dt, rc.low, rc.high),
        z=bandpass(df$z, dt, rc.low, rc.high))
}


########################################
# load and crop data

hand1 <- read.csv(file("/tmp/hand1.csv"), header=FALSE)
hand2 <- read.csv(file("/tmp/hand2.csv"), header=FALSE)

hand1.sr <- sampling.rate(hand1)
hand2.sr <- sampling.rate(hand2)
hand1.dt <- 1/hand1.sr$global
hand2.dt <- 1/hand2.sr$global

plot(mag(data.frame(x=hand1$V2,y=hand1$V3,z=hand1$V4)), type="l")
plot(mag(data.frame(x=hand2$V2,y=hand2$V3,z=hand2$V4)), type="l")

# interval 1: hand2 gives, hand1 receives
i1.hand1.from <- 3300; i1.hand1.to <- 7700
i1.hand2.from <- 1800; i1.hand2.to <- 6200
# interval 2: hand1 gives, hand2 receives
i2.hand1.from <- 7700; i2.hand1.to <- 12000
i2.hand2.from <- 6200; i2.hand2.to <- 10400
# interval 3: hand2 gives, hand1 receives (with more noise and natural motion)
i3.hand1.from <- 13300; i3.hand1.to <- 17500
i3.hand2.from <- 11600; i3.hand2.to <- 15800
# interval 4: hand1 gives, hand2 receives (with more noise and natural motion)
i4.hand1.from <- 17500; i4.hand1.to <- 22000
i4.hand2.from <- 15800; i4.hand2.to <- 20000

hand1.i1 <- hand1[i1.hand1.from:i1.hand1.to,]
hand1.i2 <- hand1[i2.hand1.from:i2.hand1.to,]
hand1.i3 <- hand1[i3.hand1.from:i3.hand1.to,]
hand1.i4 <- hand1[i4.hand1.from:i4.hand1.to,]
hand2.i1 <- hand2[i1.hand2.from:i1.hand2.to,]
hand2.i2 <- hand2[i2.hand2.from:i2.hand2.to,]
hand2.i3 <- hand2[i3.hand2.from:i3.hand2.to,]
hand2.i4 <- hand2[i4.hand2.from:i4.hand2.to,]

# finds peaks and highlights the ten highest
dir.of <- function(series) {
    a <- a.of(series)
    min.amp <- 0.3
    peaks <- find.peaks(mag(a), min.amp)

    plot(mag(a), type="l")
    abline(col="gray", v=peaks)

    d <- data.frame(i=c(1:length(peaks)), mag=mag(a)[peaks])
    highest <- peaks[d[with(d, order(-mag)),][1:10,]$i]
    dir <- norm(a[highest,])
    abline(col="red", v=highest)
    lines(mag(a))

    dir
}

dir.hand1.i1 <- dir.of(hand1.i1)
dir.hand1.i2 <- dir.of(hand1.i2)
dir.hand1.i3 <- dir.of(hand1.i3)
dir.hand1.i4 <- dir.of(hand1.i4)
dir.hand2.i1 <- dir.of(hand2.i1)
dir.hand2.i2 <- dir.of(hand2.i2)
dir.hand2.i3 <- dir.of(hand2.i3)
dir.hand2.i4 <- dir.of(hand2.i4)

example <- hand2.i1[3000:3400,]
dir.example <- dir.of(example)


#dir <- dir.hand1.i1
# etc

mean3d(norm(a))
mean3d(dir)

plot3d(dir, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1))



plot3d(dir.hand1.i1, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue")
points3d(norm(mean3d(dir.hand1.i1)), col="blue", size=10)
points3d(dir.hand1.i2, col="red")
points3d(norm(mean3d(dir.hand1.i2)), col="red", size=10)
points3d(dir.hand2.i1, col="green")
points3d(norm(mean3d(dir.hand2.i1)), col="green", size=10)
points3d(dir.hand2.i2, col="black")
points3d(norm(mean3d(dir.hand2.i2)), col="black", size=10)



hand1.p2p.i1 <- data.frame(
    p1=c(278,711,1175,1573,1971,2336,2709,3089,3490,3876),
    p2=c(376,795,1254,1662,2039,2392,2776,3142,3556,3951))
hand2.p2p.i2 <- data.frame(
    p1=c(250,624,1025,1435,1799,2242,2642,3064,3445,3847),
    p2=c(338,750,1109,1520,1915,2318,2758,3172,3551,3941))

hand1.step.i1 <- (hand1.p2p.i1$p2 - hand1.p2p.i1$p1) * 1/hand1.sr$global
mean(hand1.step.i1)
sd(hand1.step.i1)

hand2.step.i2 <- (hand2.p2p.i2$p2 - hand2.p2p.i2$p1) * 1/hand2.sr$global
mean(hand2.step.i2)
sd(hand2.step.i2)

# Based on this data from two individuals, we start with a half-period
# of between 0.4s and 0.8s for band-pass filtering.
# This is the time between the hand beginning to accelerate downwards to intercept the other hand,
# and beginning to pull back after making contact.
# If the hand were to immediately execute another handoff, it would take about the same amount of
# time to return to the ready position (so the period is between 0.8s and 1.6s).
min.step <- min(mean(hand1.step.i1)-sd(hand1.step.i1), mean(hand2.step.i2)-sd(hand2.step.i2))
max.step <- max(mean(hand1.step.i1)+sd(hand1.step.i1), mean(hand2.step.i2)+sd(hand2.step.i2))
min.step; max.step

freq.low <- 1/(2*max.step)
freq.high <- 1/(2*min.step)
rc.low <- 1/(2 * pi * freq.low)
rc.high <- 1/(2 * pi * freq.high)

hand1.i2.bp <- bandpass.3d(a.of(hand1.i2), hand1.dt, rc.low, rc.high)
hand2.i1.bp <- bandpass.3d(a.of(hand2.i1), hand2.dt, rc.low, rc.high)

hand1.i2.bp.mag <- mag(hand1.i2.bp)
hand2.i1.bp.mag <- mag(hand2.i1.bp)

ma <- 0.05
hand1.i2.bp.peaks <- find.peaks(hand1.i2.bp.mag, ma)
hand2.i1.bp.peaks <- find.peaks(hand2.i1.bp.mag, ma)

# show filtered motion with significant peaks (possibly excluding "spikes")
plot(hand1.i2.bp.mag, type="l")
abline(col="red", v=hand1.i2.bp.peaks)
plot(hand2.i1.bp.mag, type="l")
abline(col="red", v=hand2.i1.bp.peaks)


hand1.len <- (mean(c(min.step, max.step))/2) %/% hand1.dt
hand2.len <- (mean(c(min.step, max.step))/2) %/% hand2.dt
hand1.i2.ra <- running.average.3d(hand1.i2, hand1.len)
hand2.i1.ra <- running.average.3d(hand1.i1, hand2.len)


# this shows that a circular buffer with an appropriate frequency
# provides clear peaks with less noise than the
# corresponding band-pass filter (although it requires much more memory, and lags
# behind the filter slightly)
plot(mag(hand1.i2.ra), type="l")
abline(col="red", v=hand1.i2.bp.peaks)
plot(mag(hand2.i1.ra), type="l")
abline(col="red", v=hand2.i1.bp.peaks)
