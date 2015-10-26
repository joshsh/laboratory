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

# minimum trough-to-peak amplitude of features of interest in the signal after band-pass filtering
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
            if (!prev.rising) { # local minimum
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
                }
            }
        } else {
            if (prev.rising) { # local maximum
                if (high) {
                    if (xi > ref.x) {
                        ref.x <- xi
                        ref.i <- i
                    }
                } else {
                    if (xi - ref.x >= min.amp) {
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

# these are stored locally under ~/data/research/extend-o-hand/handshakes/mutual
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

hand1.dt <- 1/hand1.sr$rate.global
hand2.dt <- 1/hand2.sr$rate.global

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


########################################
# band pass filtering

# derived through successive cycles of filtering and frequency detection
freq.mean <- 5.665298
freq.low <- 4.737343
freq.high <- 7.045349

rc.low <- 1/(2 * pi * freq.low)
rc.high <- 1/(2 * pi * freq.high)

# apply a band-pass filter to the components of acceleration (as opposed to the magnitude time series)
hand1.accel.cal.bp <- bandpass.3d(hand1.accel.cal, hand1.dt, rc.low, rc.high)
hand1.accel.cal.bp.mag <- mag(hand1.accel.cal.bp)
hand2.accel.cal.bp <- bandpass.3d(hand2.accel.cal, hand2.dt, rc.low, rc.high)
hand2.accel.cal.bp.mag <- mag(hand2.accel.cal.bp)


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
# examine individual handshakes

# manually segmented handshake intervals
hand1.breaks <- c(2000, 3500, 5500, 7500, 9300, 10700, 12000, 13200, 14300, 15400, 16400, 17300, 18300, 19400, 20500, 21500, 23000, 24300, 25700, 27500)
i <- 1
ts <- hand1[hand1.breaks[i]:hand1.breaks[i+1],]; plot(mag(data.frame(x=ts$V2,y=ts$V3,z=ts$V4)), type="l")
# first 5 handshakes manually annotated with grip and release times
grips <- c(620, 340, 1000, 750, 590)
releases <- c(1175, 850, 1470, 1230, 1030)


# example handshake
grp <- 2
grp2 <- 4
from <- hand1.breaks[2*grp - 1] + trim
to <- hand1.breaks[2*grp] - trim
events1 <- data.frame(Events="Shake", t=(hand1.groups[grp2][[1]][[1]]-from)*hand1.dt)
events2 <- data.frame(Events="Grip", t=c(grips[grp2]-trim)*hand1.dt)
events3 <- data.frame(Events="Release", t=c(releases[grp2]-trim)*hand1.dt)
events <- rbind(events1,events2,events3)
df <- data.frame(t=hand1.dt*c(0:(to-from)), acceleration=ts)

library(ggplot2)
pdf("/tmp/graphic.pdf", width=6.25, height=2.25)
par(mar=c(4.5,5,2,1.5))
  ggplot(data=df, aes(x=t, y=acceleration)) +
    geom_vline(data=events, aes(xintercept=t,linetype=Events,colour=Events), show_guide = TRUE) +
    geom_line() +
    xlab("time (seconds)") +
    ylab("acceleration (g)") +
    theme_bw()
dev.off()


# time between largest handshake peak and the retraction peak in three handshakes
retraction.time <- c(1.464292, 1.187264, 1.385141)
# 1.345566 seconds
mean(retraction.time)


hand1.first.peaks <- sapply(hand1.groups, function(l){v <- l[[1]]; v[1]})
hand1.last.peaks <- sapply(hand1.groups, function(l){v <- l[[1]]; v[length(v)]})


# the spectral density plot does reveal the characteristic handshake frequency as a low bump,
# but the center of the peak is perhaps not as precise as that found with the method below.
series <- mag(hand1.accel.cal)
catted <- c()
for (i in 1:length(hand1.first.peaks)) {
    catted <- c(catted, series[hand1.first.peaks[i]:hand1.last.peaks[i]])
}
hand1.shakes.only <- catted
spectrum(catted)
abline(col="red", v=freq.mean*hand1.dt)

# fast Fourier analysis, similarly, shows (perhaps more clearly) a peak around the previously found value.
# There is a large amount of variability, but the swell of the peak is clearly visible.
# More data would be helpful for a less ambiguous result.
p <- fft(hand1.shakes.only)
p2 <- abs(p)
plot(p2, type="l", xlim=c(0,250), ylim=c(0,350)); abline(col="red", v=1/(5.4*hand1.dt))
# With frequency (rather than time) on the x axis, a second, weaker peak at around 7Hz is visible...
# but again, this appears to be just noise.  The trend is more clearly visible in the previous plot.
x=c(1:length(hand1.shakes.only))
plot(x=1/(x*hand1.dt), y=p2/length(x), type="l", xlim=c(0,15), ylim=c(0,0.2))

# as the superposition of many handshakes, the series does not produce any coherent peaks of frequency.
# However, several features are clearly visible, one of which is the characteristic up-and-down motion centered
# quite close to the calculated point.
# There are also lower-frequency features which presumably correspond to the large-scale motions of the hand
# approaching, gripping, releasing, and swinging back from the other hand.
s <- spectrum(series)
s <- spectrum(catted)
plot(x=s$freq,y=s$spec,type="l",xlim=c(0,0.06))
abline(col="red", v=freq.mean*hand1.dt); abline(col="purple", v=freq.low*hand1.dt); abline(col="purple", v=freq.high*hand1.dt)
# the interval full of up-down peaks is very broad.  Taking the top 10 peaks:
0.0153/hand1.dt
#[1] 3.799125009
0.0287/hand1.dt
#[1] 7.126463251

##########
# average spectral density

# overall
spectra <- sapply(c(1:(length(hand1.breaks)-2)), function(i){ spectrum(series[hand1.breaks[i]:hand1.breaks[i+1]]) })
inc <- max(freq)/125  # set this as high as possible without leaving gaps

# only the shake
spectra <- sapply(c(1:length(hand1.first.peaks)), function(i){ spectrum(series[hand1.first.peaks[i]:hand1.last.peaks[i]]) })
inc <- max(freq)/15  # set this as high as possible without leaving gaps

freqs <- c(); for (i in 1:length(spectra[1,])) {freqs <- c(freqs, spectra[,i]$freq)}
specs <- c(); for (i in 1:length(spectra[1,])) {specs <- c(specs, spectra[,i]$spec)}
points <- data.frame(freq=freqs, spec=specs, inc=freqs %/% inc)
points.ord <- points[with(points, order(freq)),]
plot(x=points.ord$freq, y=points.ord$spec, type="l", xlim=c(0,0.06))
steps <- 1:max(points.ord$inc)
means <- sapply(steps, function(i){ mean(subset(points.ord, inc==i)$spec) })
#plot(x=(steps*inc/hand1.dt), y=means, type="l")
barplot(means, names.arg=(floor(steps*inc/hand1.dt)), xlim=c(0,50))

##########

# handshake total duration (from grip to release)
# 1.943155
mean((rels-grips)*hand1.dt)
# 0.1727322
sd((rels-grips)*hand1.dt)

# handshake "shake duration": average interval between the first and last handshake peak
hand1.width <- hand1.dt * (hand1.last.peaks - hand1.first.peaks)
# 0.4042633
mean(hand1.width)
# 0.1522405
sd(hand1.width)

# time from grip to first peak
hand1.grip.time <- hand1.dt * (hand1.first.peaks[1:length(releases)] - hand1.breaks[1:length(releases)] - grips)
# 0.3158121
mean(hand1.grip.time)
# 0.1195611
sd(hand1.grip.time)

# time from last peak to release
hand1.hold.time <- hand1.dt * (hand1.breaks[1:length(releases)] + releases - hand1.last.peaks[1:length(releases)])
# 1.088325
mean(hand1.hold.time)
# 0.08455637
sd(hand1.hold.time)


########################################

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
# band-pass filtering: plotting

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
plot(x=hand1.peaks, y=hand1.peaks, xlim=c(low,high), xlab="", ylab="", main="unpaired handshake peaks")
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
freq <- 1/(2*hand.dt*real.diffs)
# 5.773614 (hand1), 6.299301 (hand2)
freq.mean <- mean(freq)
# 1.178395 (hand1), 1.283529 (hand2)
freq.sd <- sd(freq)
freq.low <- freq.mean - freq.sd
freq.high <- freq.mean + freq.sd
