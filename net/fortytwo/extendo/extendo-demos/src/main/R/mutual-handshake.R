#install.packages("rgl")
library(rgl)

mag <- function(p) { sqrt(p$x^2 + p$y^2 + p$z^2) }
norm <- function(p) { p / mag(p) }

sampling.rate <- function(motion) {
    times <- motion$V1
    t1 <- times[1:(length(times)-1)]
    t2 <- times[2:length(times)]
    diff <- t2 - t1

     # time step in seconds
    dt <- mean(diff/1000000)

    dt; sd(diff/1000000)
    rate <- mean(1000000/diff)
    rate; sd(1000000/diff)

    # sampling rate in Hz
    data.frame(rate.local=mean(1000000/diff), sd=sd(1000000/diff),
        rate.global=nrow(motion)*1000000/(max(times)-min(times)))
}

local.which.max <- function(series, center, radius) {
    center - radius - 1 + which.max(series[(center-radius):(center+radius)])
}

diffs <- function(s) {
    s.low <- s[1:(length(s)-1)]
    s.high <- s[2:length(s)]
    s.high - s.low
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

# minimum peak-to-peak amplitude of features of interest in the signal after band-pass filtering
min.amp <- 0.1

# expected.period: the number of samples expected for each full cycle, i.e. each pair of peaks
find.peaks <- function(x, expected.period) {
    peak.maxgap <- expected.period * 2
    peaks <- c()
    prev.x <- 0
    prev.rising <- TRUE
    ref.x <- 0
    ref.i <- 0
    high <- FALSE
    last.i <- NULL
    last.added <- FALSE
    n <- length(x)
    for (i in 1:n) {
        xi <- x[i]
        rising <- xi >= prev.x
        prev.x <- xi
        if (rising) {
            if (!prev.rising) {
                # local minimum
                if (high) {
                    if (ref.x - xi >= min.amp) {
                        if (!is.null(last.i) && ref.i - last.i <= peak.maxgap) {
                            if (!last.added) {
                                peaks <- c(peaks, last.i)
                            }
                            peaks <- c(peaks, ref.i)
                            last.added <- TRUE
                        } else {
                            last.added <- FALSE
                        }

                        last.i <- ref.i

                        ref.x <- xi
                        high <- FALSE
                    }
                } else if (xi < ref.x) {
                    ref.x <- xi
                    #ref.i <- i
                }
            }
        } else {
            if (prev.rising) {
                # local maximum
                if (high) {
                    if (xi > ref.x) {
                        ref.x <- xi
                        ref.i <- i
                    }
                } else {
                    if (xi - ref.x >= min.amp) {
                        #peaks <- c(peaks, ref.i)
                        ref.i <- i
                        ref.x <- xi
                        high <- TRUE
                    }
                }
            }
        }
        prev.rising <- rising
    }

    peaks
}

group.peaks <- function(peaks) {
    groups <- list()
    cur <- c()
    last <- -1000

    for (i in peaks) {
        if (i - last > 200) {
            if (length(cur) > 1) {
                groups[[length(groups)+1]] <- list(cur)
            }
            cur <- c()
        }

        cur <- c(cur, i)
        last <- i
    }

    groups
}

collect.diffs <- function(l) {
    r <- c()
    for (i in 1:length(l)) { r <- c(r, diffs(l[i][[1]][[1]])) }
    r
}

# for stick #1. See gyro.mean derived in keyboard.R
gyro.cal <- data.frame(x=-110.996, y=105.5632, z=-37.2404)

accel <- function(motion) { data.frame(x=motion$V2, y=motion$V3, z=motion$V4) }
gyro <- function(motion) { data.frame(x=(motion$V5 - gyro.cal$x[1]), y=(motion$V6 - gyro.cal$y[1]), z=(motion$V7 - gyro.cal$z[1])) }
magnet <- function(motion) { data.frame(x=motion$V8, y=motion$V9, z=motion$V10) }
accel.cal <- function(accel) { data.frame(x=(accel$x/230 - 0.05), y=(accel$y/230), z=(accel$z/230)) }


########################################
# load and crop data

hand1 <- read.csv(file("/tmp/hand1.csv"), header=FALSE)
hand2 <- read.csv(file("/tmp/hand2.csv"), header=FALSE)

plot(mag(data.frame(x=hand1$V2,y=hand1$V3,z=hand1$V4)), type="l")
plot(mag(data.frame(x=hand2$V2,y=hand2$V3,z=hand2$V4)), type="l")

# crop the data appropriately after viewing it, so as to avoid complicated workarounds
# for leading and trailing noise
hand1.from <- 2000; hand1.to <- 28000
hand2.from <- 33000; hand2.to <- 58000
hand1 <- hand1[hand1.from:hand1.to,]
hand2 <- hand2[hand2.from:hand2.to,]


########################################
# define time series

hand1.sr <- sampling.rate(hand1)
hand2.sr <- sampling.rate(hand2)
hand1.sr; hand2.sr

hand1.accel <- accel(hand1)
hand1.gyro <- gyro(hand1)
hand1.magnet <- magnet(hand1)
hand1.accel.cal <- accel.cal(hand1.accel)
hand1.accel.mag <- mag(hand1.accel)
hand1.gyro.mag <- mag(hand1.gyro)
hand1.magnet.mag <- mag(hand1.magnet)
hand1.accel.cal.mag <- mag(hand1.accel.cal)

hand2.accel <- accel(hand2)
hand2.gyro <- gyro(hand2)
hand2.magnet <- magnet(hand2)
hand2.accel.cal <- accel.cal(hand2.accel)
hand2.accel.mag <- mag(hand2.accel)
hand2.gyro.mag <- mag(hand2.gyro)
hand2.magnet.mag <- mag(hand2.magnet)
hand2.accel.cal.mag <- mag(hand2.accel.cal)


# the below does *not* work for aligning the two time series; calculated sampling rates are not sufficiently accurate
rate.ratio <- hand1.sr$rate.global/hand2.sr$rate.global
#hand1.from <- 1; hand1.to <- nrow(hand1)
#hand2.from <- 1; hand2.to <- nrow(hand2)
hand1.from <- 1; hand1.to <- 30000
hand2.from <- 32000; hand2.to <- hand2.from + 30000
plot(hand1.accel.cal.mag[hand1.from:hand1.to], ylim=c(min(hand1.accel.cal.mag), max(hand1.accel.cal.mag, 3+hand2.accel.cal.mag)), type="l")
lines(x=rate.ratio*c(0:(hand2.to-hand2.from)), y=hand2.accel.cal.mag[hand2.from:hand2.to] + 3, col="red")


# OLD: manually align the time series using two pairs of reference points
plot(hand1.accel.cal.mag, type="l")
hand1.point1 <- 2800
abline(col="red", v=hand1.point1)
hand1.point2 <- 26750
abline(col="red", v=hand1.point1)
plot(hand2.accel.cal.mag, type="l")
hand2.point1 <- 34200
abline(col="red", v=hand2.point1)
hand2.point2 <- 56280
abline(col="red", v=hand2.point2)
hand1.point1 <- local.which.max(hand1.accel.cal.mag, hand1.point1, 200)
hand1.point2 <- local.which.max(hand1.accel.cal.mag, hand1.point2, 200)
hand2.point1 <- local.which.max(hand2.accel.cal.mag, hand2.point1, 200)
hand2.point2 <- local.which.max(hand2.accel.cal.mag, hand2.point2, 200)
rate.ratio <- (hand1.point2 - hand1.point1)/(hand2.point2 - hand2.point1)
hand1.from <- hand1.point1; hand1.to <- hand1.point2
hand2.from <- hand2.point1; hand2.to <- hand2.point2
plot(hand1.accel.cal.mag[hand1.from:hand1.to], ylim=c(min(hand1.accel.cal.mag), max(hand1.accel.cal.mag, 3+hand2.accel.cal.mag)), type="l")
lines(x=rate.ratio*c(0:(hand2.to-hand2.from)), y=hand2.accel.cal.mag[hand2.from:hand2.to] + 3, col="red")


from <- 5000; to <- 6500
hand1.subset <- hand1[(hand1.point1+from):(hand1.point1+to),]
hand2.subset <- hand2[(hand2.point1+floor(from/rate.ratio)):(hand2.point1+floor(to/rate.ratio)),]
hand1.bk <- hand1
hand2.bk <- hand2
hand1 <- hand1.subset
hand2 <- hand2.subset

# repeat the above using the subset

plot(hand1.accel.cal.mag, ylim=c(min(hand1.accel.cal.mag), max(hand1.accel.cal.mag, 3+hand2.accel.cal.mag)), type="l")
lines(x=rate.ratio*c(1:nrow(hand2)), y=hand2.accel.cal.mag + 3, col="red")


########################################
# band-pass filtering

# derived through successive cycles of filtering and frequency detection
freq.mean <- 5.665298
freq.low <- 4.737343
freq.high <- 7.045349

rc.low <- 1/(2 * pi * freq.low)
rc.high <- 1/(2 * pi * freq.high)

# apply a band-pass filter to the components of acceleration (as opposed to the magnitude time series)
hand1.dt <- 1/hand1.sr$rate.global
hand1.accel.cal.bp <- bandpass.3d(hand1.accel.cal, hand1.dt, rc.low, rc.high)
hand1.accel.cal.bp.mag <- mag(hand1.accel.cal.bp)
hand2.dt <- 1/hand2.sr$rate.global
hand2.accel.cal.bp <- bandpass.3d(hand2.accel.cal, hand2.dt, rc.low, rc.high)
hand2.accel.cal.bp.mag <- mag(hand2.accel.cal.bp)

plot(hand1.accel.cal.bp.mag, type="l")
abline(col="red", v=hand1.peaks)

plot(hand2.accel.cal.bp.mag, type="l")
abline(col="red", v=hand2.peaks)


# inspect detected extrema in 3D for accuracy
series <- hand1.accel.cal.bp
m <- series
plot3d(m$x, m$y, m$z, type="l", size=1)
m <- hand1.accel.cal.bp[hand1.peaks,]
points3d(col="red", size=5, m$x, m$y, m$z)


########################################
# extremum detection

hand1.peaks <- find.peaks(hand1.accel.cal.bp.mag, 1/(hand1.dt*freq.mean))
hand2.peaks <- find.peaks(hand2.accel.cal.bp.mag, 1/(hand2.dt*freq.mean))

hand1.groups <- group.peaks(hand1.peaks)
hand2.groups <- group.peaks(hand2.peaks)
# note that we exclude some non-shake extrema
hand1.count <- sapply(2:(length(hand1.groups)-1),function(i){length(hand1.groups[i][[1]][[1]])})
hand2.count <- sapply(2:(length(hand2.groups)-1),function(i){length(hand2.groups[i][[1]][[1]])})
# with thresholds of 0.2 and 0.3, we have 4.1 +- 1.8 extrema per handshake
mean(hand1.count)
mean(hand2.count)
sd(hand1.count)
sd(hand2.count)

hand1.diffs <- collect.diffs(hand1.groups)
hand2.diffs <- collect.diffs(hand2.groups)


########################################
# matching of extrema

ratio <- hand2.dt/hand1.dt * 1.0032
s1 <- hand1.peaks[3]
s2 <- hand2.peaks[2]
plot(x=hand1.peaks, y=hand1.peaks)
abline(v=hand1.peaks)
abline(col="red", v=(s1 + (hand2.peaks-s2)*ratio))

#low <- 500; high <- 1000
#low <- 22500; high <- 23000
low <- 10300; high <- 10700
plot(x=hand1.peaks, y=hand1.peaks, xlim=c(low,high))
abline(v=hand1.peaks)
abline(col="red", v=(s1 + (hand2.peaks-s2)*ratio))


########################################
# zoom in on single detected handshake

# compare raw (calibrated) acceleration with band-passed acceleration and detected extrema

from <- 6250; to <- 6550
plot(hand1.accel.cal.mag[from:to], type="l", xlab="time (dt = 4ms)",
    ylab="magnitude of (ax,ay,az)", main="unfiltered acceleration with eventually detected extrema")
abline(col="red", v=(hand1.peaks-from+1))

from <- 6250; to <- 6550
plot(hand1.accel.cal.bp.mag[from:to], type="l", xlab="time (dt = 4ms)",
    ylab="magnitude of (ax,ay,az) after band-pass", main="filtered acceleration with detected extrema")
abline(col="red", v=(hand1.peaks-from+1))


########################################
# frequency estimation

# substitute hand2 to obtain an only slightly different frequency estimate
hand.diffs <- hand1.diffs
hand.dt <- hand1.dt
hand.groups <- hand1.groups
# separate frequency diffs from noise
hist(hand.diffs, breaks=60)
low <- floor(0.056/hand.dt)
high <- ceiling(0.12/hand.dt)
abline(col="red", v=c(low,high))
real.diffs <- subset(data.frame(diff=hand.diffs), diff >= low & diff <= high)$diff
freq.mean <- 1/(2*hand.dt*mean(real.diffs))
freq.low <- 1/(2*hand.dt*(mean(real.diffs)+sd(real.diffs)))
freq.high <- 1/(2*hand.dt*(mean(real.diffs)-sd(real.diffs)))
