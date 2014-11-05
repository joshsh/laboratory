library(rgl)

mag <- function(p) { sqrt(p$x^2 + p$y^2 + p$z^2) }
norm <- function(p) { p / mag(p) }

# import data using SIMPLE_OUTPUT in extendo_hand.ino
motion <- read.csv(file("/tmp/motion.csv"), header=FALSE)

# find sampling rate
times <- motion$V1
t1 <- times[1:(length(times)-1)]
t2 <- times[2:length(times)]
diff <- t2 - t1
dt <- mean(diff/1000000) # time step in seconds
dt; sd(diff/1000000)
rate <- mean(1000000/diff)  # sampling rate in Hz
rate; sd(1000000/diff)

# for stick #1. See gyro.mean derived in keyboard.R
gyro.cal <- data.frame(x=-110.996, y=105.5632, z=-37.2404)

accel <- data.frame(x=motion$V2, y=motion$V3, z=motion$V4)
gyro <- data.frame(x=(motion$V5 - gyro.cal$x[1]), y=(motion$V6 - gyro.cal$y[1]), z=(motion$V7 - gyro.cal$z[1]))
magnet <- data.frame(x=motion$V8, y=motion$V9, z=motion$V10)
a <- data.frame(x=(accel$x/230 - 0.05), y=(accel$y/230), z=(accel$z/230))

accel.mag <- mag(accel)
gyro.mag <- mag(gyro)
magnet.mag <- mag(magnet)
a.mag <- mag(a)


# acceleration varies up to around 2 in each dimension
max(abs(a$x)); max(abs(a$y)); max(abs(a$z))
# magnitude of acceleration varies up to around sqrt(12) = 3.46
max(abs(a.mag))


b.low <- 1.25
b.high <- 1.5

plot(a.mag, type="l")

abline(col="green", h=b.low)
abline(col="green", h=b.high)

ids <- 1:nrow(motion)
high <- a.mag[1] > b.high
low <- a.mag[1] < b.low
counter <- 1
n <- nrow(motion)
for (i in 1:n) {
    high.last <- high
    low.last <- low
    v <- a.mag[i]
    high <- v > b.high
    low <- v < b.low
    if (high) {
        counter <- if (high.last) counter else counter + 1
        id <- counter
    } else if (low) {
        counter <- if (low.last) counter else counter + 1
        id <- -counter
    } else {
        id = 0
    }
    ids[i] <- id
}

tmp <- data.frame(i=c(1:n), id=ids, a=a.mag)
tmp.peak <- subset(tmp, a > b.high)
tmp.trough <- subset(tmp, a < b.low)

peak.whichmax <- function(peak.id) {
    s <- subset(tmp.peak, id == peak.id)
    s$i[which.max(s$a)]
}
trough.whichmin <- function(trough.id) {
    s <- subset(tmp.trough, id == trough.id)
    s$i[which.min(s$a)]
}

peaks <- sapply(unique(tmp.peak$id), peak.whichmax)
troughs <- sapply(unique(tmp.trough$id), trough.whichmin)
peaks.and.troughs <- sort(c(peaks, troughs))

for (i in peaks) {
    abline(col="red", v=i)
}
for (i in troughs) {
    abline(col="blue", v=i)
}

peak.a <- a[peaks,]
trough.a <- a[troughs,]
peak.and.trough.a <- a[peaks.and.troughs,]

m <- a
plot3d(m$x, m$y, m$z, type="l")
n <- nrow(peak.a)
m <- peak.a[2 * 1:floor((n+1)/2) - 1,]
points3d(m$x, m$y, m$z, col="red", size="5")
m <- peak.a[2 * 1:floor((n+1)/2),]
points3d(m$x, m$y, m$z, col="blue", size="5")


#m <- a
m <- norm(peak.and.trough.a)
plot3d(m$x, m$y, m$z, type="l", xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1))
n <- nrow(peak.a)
m <- norm(peak.a)
points3d(m$x, m$y, m$z, col="red", size="5")
m <- norm(trough.a)
points3d(m$x, m$y, m$z, col="blue", size="5")



d.a.mag <- a.mag[2:length(a.mag)] - a.mag[1:(length(a.mag) - 1)]
plot(d.a.mag, type="l")


########################################
# frequency estimation (first pass)

# using hand1 time series from 2014-11-03
# Only time between apparently corresponding peaks is counted in the below
# Note that in the accelerometer magnitude time series, peaks appear at twice this frequency
# (alternating between the up-swing and the down-swing)
wholes <- c(48,52,38,41,37,44,40,37,42,52,32,39,38,49,31,40,40,42,32,39,61,46,43)

# calculated as (length(times)*1000000)/(times[length(times)]-times[1])
sampling.frequency <- 252.6819

# I find around 6Hz for handshakes between Xixi and Josh, similar to Melnyk et al.,
# who find 4Hz overall
freq.mean <- sampling.frequency/mean(wholes)

freq.low <- sampling.frequency/(mean(wholes)+sd(wholes))
freq.high <- sampling.frequency/(mean(wholes)-sd(wholes))


########################################
# handshake-specific filtering

diffs <- function(s) {
    s.low <- s[1:(length(s)-1)]
    s.high <- s[2:length(s)]
    s.high - s.low
}

# use diffs to find typical frequency of handshakes

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

# I find roughly 6Hz for handshakes between Xixi and Josh, similar to Melnyk et al.,
# who find 4Hz overall
t.low <- (16.3 - 3.5) * dt
t.high <- (16.3 + 3.5) * dt

f.low <- 1/t.low
rc.low <- 1/(2 * pi * f.low)
a.mag.lowpass <- lowpass(a.mag, dt, rc.low)

f.high <- 1/t.high
rc.high <- 1/(2 * pi * f.high)
a.mag.highpass <- highpass(a.mag, dt, rc.high)

a.mag.bandpass <- bandpass(a.mag, dt, rc.low, rc.high)

plot(a.mag, type="l", col="gray", main="lowpass", ylim=c(0, max(a.mag)))
lines(a.mag.lowpass, col="red")

plot(a.mag, type="l", col="gray", main="highpass", ylim=c(min(a.mag.highpass), max(a.mag)))
lines(a.mag.highpass, col="red")

plot(a.mag, type="l", main="bandpass", ylim=c(min(a.mag.bandpass), max(a.mag)))
abline(h=0, col="gray")
abline(h=0.15, col="gray")
abline(h=-0.15, col="gray")
lines(a.mag.bandpass, col="red")


########################################
# repetitive band-pass doesn't help

# This only attenuates the entire signal, rather than boosting the target frequency.
# However, it does smooth the signal a little, as well.
a.mag.bandpass <- bandpass(a.mag.bandpass, dt, rc.low, rc.high)


########################################
# attempts at a tuned amplifier for handshake frequencies

# approximation of discrete dt
t <- 16

amp <- function(x, t) {
    n <- length(x)
    y <- 0*1:n
    for (i in (t+1):n) {
        y[i] <- x[i] * x[i-t]
    }
    y
}

# this is not yet very useful without a band-pass filter
a.mag.amp <- amp(a.mag, t)
plot(a.mag.amp, type="l", main="boost")

# This nicely eliminates everything other than the handshake.
# However, the remaining signal is "sharp", ragged, and probably unusable.
a.mag.bandpass.amp <- 4 * amp(a.mag.bandpass, t)
plot(a.mag, type="l", main="bandpass and boost", ylim=c(min(a.mag.bandpass.amp), max(a.mag)))
lines(a.mag.bandpass.amp, col="red")

amp2 <- function(x, t) {
    n <- length(x)
    y <- 0*1:n
    for (i in (t+1):n) {
        y[i] <- x[i] + x[i-t]
    }
    y
}

# again, not useful without a band-pass filter
a.mag.amp2 <- amp2(a.mag, t)
plot(a.mag.amp2, type="l", main="boost (additive)")

# No, this is useless even with the filter.
a.mag.bandpass.amp2 <- amp2(a.mag.bandpass, t)
plot(a.mag, type="l", main="bandpass and boost (additive)", ylim=c(min(a.mag.bandpass.amp2), max(a.mag)))
lines(a.mag.bandpass.amp2, col="red")

amp3 <- function(x, t1, t2) {
    n <- length(x)
    y <- 0*1:n
    for (i in (t2+1):n) {
        sum <- sum(x[i]*x[i-t1:t2])
        y[i] <- sum / (1+t2-t1)
    }
    y
}

# Useless.  This doesn't even have the property of damping the non-handshake portion of the signal.
a.mag.amp3 <- amp3(a.mag, t-3, t+3)
plot(a.mag, type="l", main="boost (band, experimental)", ylim=c(min(a.mag.amp3), max(a.mag.amp3)))
lines(a.mag.amp3, col="red")


########################################
# amplitude of the band-passed signal does not depend on sampling rate

bp.without.head <- a.mag.bandpass[50:length(a.mag.bandpass)]
min(bp.without.head)
max(bp.without.head)

# cut the sampling rate in half, then repeat the analysis above
a.mag <- a.mag[2*1:(length(a.mag)/2)-1]
dt <- dt * 2

# cut the sampling rate in thirds; signal quality continues to deteriorate,
# but amplitude is not significantly affected
a.mag <- a.mag[3*1:(length(a.mag)/3)-2]
dt <- dt * 3
