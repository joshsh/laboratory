#install.packages("rgl")
library(rgl)

mag <- function(p) { sqrt(p$x^2 + p$y^2 + p$z^2) }
norm <- function(p) { p / mag(p) }
mean3d <- function(p) { data.frame(x=mean(p$x), y=mean(p$y), z=mean(p$z))}
diff3d <- function(p1, p2) { data.frame(x=p1$x-p2$x, y=p1$y-p2$y, z=p1$z-p2$z)}
dist3d <- function(p1, p2) { mag(diff3d(p1, p2)) }

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

# min.amp: minimum absolute magnitude of a spike
# min.period: minimum distance between spikes
find.spikes <- function(x, min.amp, min.period) {
    spikes <- c()
    n <- length(x)
    last.i <- 0
    for (i in 1:n) {
        xi <- x[i]
        if (xi >= min.amp) {
            if (i - last.i >= min.period) {
               spikes <- c(spikes, i)
            }

            # this becomes
            last.i <- i
        }
    }

    spikes
}


min.spike.amp <- 2.5
find.gives <- function(x, buflen) {
    buffer <- 0*x[1:buflen]  # initialize the buffer with zeroes
    j <- 0 # index in buffer
    n <- length(x)
    for (i in 1:n) {
        xi <- x[i]
        sum <- sum - buffer[j+1] + xi
        buildup <- sum/buflen
        j <- (j + 1) %% buflen


    }
}


a.of <- function(series) {
    data.frame(x=series$V2,y=series$V3,z=series$V4)
}

running.average <- function(x, buflen) {
    buffer <- x[1:buflen]
    sum <- sum(buffer)
    n <- length(x)
    j <- 0 # index in buffer
    result <- c()
    for (i in 1:n) {
        xi <- x[i]
        sum <- sum - buffer[j+1] + xi
        buffer[j+1] <- xi
        result <- c(result, sum/buflen)
        j <- (j + 1) %% buflen
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

hand1.i1.mag <- mag(a.of(hand1.i1))
hand1.i2.mag <- mag(a.of(hand1.i2))
hand1.i3.mag <- mag(a.of(hand1.i3))
hand1.i4.mag <- mag(a.of(hand1.i4))
hand2.i1.mag <- mag(a.of(hand2.i1))
hand2.i2.mag <- mag(a.of(hand2.i2))
hand2.i3.mag <- mag(a.of(hand2.i3))
hand2.i4.mag <- mag(a.of(hand2.i4))


########################################
# detect spikes

spikes.min.amp <- 2.5
# TODO: change
hand1.spikes.min.period <- 40
hand2.spikes.min.period <- 40

hand1.i1.spikes <- find.spikes(hand1.i1.mag, spikes.min.amp, hand1.spikes.min.period)
hand1.i2.spikes <- find.spikes(hand1.i2.mag, spikes.min.amp, hand1.spikes.min.period)
hand2.i1.spikes <- find.spikes(hand2.i1.mag, spikes.min.amp, hand2.spikes.min.period)
hand2.i2.spikes <- find.spikes(hand2.i2.mag, spikes.min.amp, hand2.spikes.min.period)

# note: these are not yet used
hand1.i3.spikes <- find.spikes(hand1.i3.mag, spikes.min.amp, hand1.spikes.min.period)
hand2.i4.spikes <- find.spikes(hand2.i4.mag, spikes.min.amp, hand2.spikes.min.period)

# total spikes: 18
length(c(hand1.i1.spikes, hand2.i2.spikes, hand1.i3.spikes, hand2.i4.spikes))

plot(hand1.i1.mag, type="l"); abline(col="red", v=hand1.i1.spikes)
plot(hand1.i2.mag, type="l"); abline(col="red", v=hand1.i2.spikes)
plot(hand2.i1.mag, type="l"); abline(col="red", v=hand2.i1.spikes)
plot(hand2.i2.mag, type="l"); abline(col="red", v=hand2.i2.spikes)

# this shows that "give" spikes are localized and consistent.  We can usefully constrain them to a region.
v1 <- norm(a.of(hand1.i2[hand1.i2.spikes,]))
v2 <- norm(a.of(hand2.i1[hand2.i1.spikes,]))
v3 <- rbind(v1, v2)
# 0.392898 -0.3569294 0.8474861
give.spikes.vector <- norm(mean3d(v3))
d <- dist3d(v3, give.spikes.vector)
# 0.7220269
3 * sd(d)
plot3d(v1, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue")
points3d(norm(mean3d(v1)), col="blue", size=10)
points3d(v2, col="green")
points3d(norm(mean3d(v2)), col="green", size=10)
points3d(give.spikes.vector, col="gray", size=10)

# "take" spikes are nearly as localized as "gives"
# hand #2 has few "take" spikes, but they are consistent in direction.
v1 <- norm(a.of(hand1.i1[hand1.i1.spikes,]))
v2 <- norm(a.of(hand2.i2[hand2.i2.spikes,]))
v3 <- rbind(v1, v2)
# 0.6365883 -0.4451356 0.6297695
take.spikes.vector <- norm(mean3d(v3))
d <- dist3d(v3, take.spikes.vector)
# 0.9175305
3 * sd(d)
plot3d(v1, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue")
points3d(norm(mean3d(v1)), col="blue", size=10)
points3d(v2, col="green")
points3d(norm(mean3d(v2)), col="green", size=10)
points3d(take.spikes.vector, col="gray", size=10)

# The two centers at first seem strangely close together, despite one being palm-up, the other palm-down
# *because* this is the acceleration at the moment of impact, which is always into the palm and by definition
# (with spikes.min.amp larger than 1) is more significant than gravity
dist3d(give.spikes.vector, take.spikes.vector)

# See the two centers (close) together
plot3d(give.spikes.vector, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue", size=10)
points3d(take.spikes.vector, col="green", size=10)


# Now find the characteristic vectors of "give" and "take" just before the spikes using the smoothing circular
# buffer instead of the spikes themselves.
# note: the -5 just excludes the actual spike
# Both "gives" and "takes" are, predictably, even better localized when we use the smoothing buffer.

v1 <- norm(hand1.i2.ra[hand1.i2.spikes-5,])
v2 <- norm(hand2.i1.ra[hand2.i1.spikes-5,])
v3 <- rbind(v1, v2)
# 0.686863 -0.6251692 0.3706516
give.spikes.ra.vector <- norm(mean3d(v3))
d <- dist3d(v3, give.spikes.ra.vector)
# 0.3498809
3 * sd(d)
plot3d(v1, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue")
points3d(norm(mean3d(v1)), col="blue", size=10)
points3d(v2, col="green")
points3d(norm(mean3d(v2)), col="green", size=10)
points3d(give.spikes.ra.vector, col="gray", size=10)

v1 <- norm(hand1.i1.ra[hand1.i1.spikes-5,])
v2 <- norm(hand2.i2.ra[hand2.i2.spikes-5,])
v3 <- rbind(v1, v2)
# 0.8353023 -0.49748 -0.2340593
take.spikes.ra.vector <- norm(mean3d(v3))
d <- dist3d(v3, take.spikes.ra.vector)
# 0.3846736
3 * sd(d)
plot3d(v1, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue")
points3d(norm(mean3d(v1)), col="blue", size=10)
points3d(v2, col="green")
points3d(norm(mean3d(v2)), col="green", size=10)
points3d(take.spikes.vector, col="gray", size=10)

# the centers are now twice as far apart, although hardly diametrically opposed
dist3d(give.spikes.ra.vector, take.spikes.ra.vector)

# compare buildup peaks to smoothed instant before "give" spikes
# result: they're pretty close, though not right on top of each other.
# Roughtly intermediate between "give" and "take" spikes.
buildup.peaks.vector <- norm(mean3d(norm(hand1.i2.ra[hand1.i2.ra.peaks.buildup,])))
dist3d(buildup.peaks.vector, give.spikes.ra.vector)

plot3d(give.spikes.ra.vector, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue", size=10)
points3d(take.spikes.ra.vector, col="green", size=10)
points3d(buildup.peaks.vector, col="red", size=10)


########################################
# plot a representative "give" time series, for a figure

require(ggplot2)
library(reshape)

series <- hand2.i1[3040:3200,]

a <- a.of(series)
min.amp <- 1.0
peaks <- find.peaks(mag(a), min.amp)
p1 <- peaks[1]

df <- data.frame(t=((1:length(mag(a)))-p1)*hand2.dt, a=mag(a), x=a$x, y=a$y, z=a$z)
df.melted <- melt(df, id="t")

pdf("/tmp/graphic.pdf", width=6.25, height=3)
par(mar=c(4.5,5,2,1.5))
ggplot(data = df.melted, aes(x = t, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(values=c("black", "#CC6666", "#66CC66", "#9999CC"), labels=c("|a|", expression("a"["x"]), expression("a"["y"]), expression("a"["z"]))) +
  xlab("time (s)") +
  ylab("acceleration (g)")
dev.off()


########################################
# duration of a spike -- around 30ms

spi <- 2
h <- hand2.i1[(hand2.i1.spikes[spi]-5):(hand2.i1.spikes[spi]+5),]
plot(mag(a.of(h)), type="l")
# 0.0339344
6*hand2.dt


########################################

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

# Based on this data from two individuals, we start with a quarter-period
# of between 0.4s and 0.8s for band-pass filtering.
# This is the time between the hand beginning to accelerate downwards to intercept the other hand,
# and making contact.
# If the hand were to immediately execute another handoff, it would take about the same amount of
# time to begin to recoil before returning to the ready position (so the half-period is between 0.8s and 1.6s).
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
hand1.i2.ra.mag <-mag(hand1.i2.ra)
hand2.i1.ra.mag <- mag(hand2.i1.ra)

# this shows that a circular buffer with an appropriate frequency
# provides clear peaks with less noise than the
# corresponding band-pass filter (although it requires much more memory, and lags
# behind the filter slightly)
plot(hand1.i2.ra.mag, type="l"); abline(col="red", v=hand1.i2.bp.peaks)
plot(hand2.i1.ra.mag, type="l"); abline(col="red", v=hand2.i1.bp.peaks)

# show the improved peaks found with the circular buffer
ma <- 0.12
hand1.i2.ra.peaks <- find.peaks(hand1.i2.ra.mag, ma)
hand2.i1.ra.peaks <- find.peaks(hand2.i1.ra.mag, ma)
plot(hand1.i2.ra.mag, type="l"); abline(col="blue", v=hand1.i2.ra.peaks)
plot(hand2.i1.ra.mag, type="l"); abline(col="blue", v=hand2.i1.ra.peaks)

buildup.peaks <- function(pks) {
    pks[1+3*c(0:(length(pks)/3-1))]
}

retract.peaks <- function(pks) {
    pks[3+3*c(0:(length(pks)/3-1))]
}

hand1.i2.ra.peaks.buildup <- buildup.peaks(hand1.i2.ra.peaks)
hand2.i1.ra.peaks.buildup <- buildup.peaks(hand2.i1.ra.peaks)
hand1.i2.ra.peaks.retract <- retract.peaks(hand1.i2.ra.peaks)
hand2.i1.ra.peaks.retract <- retract.peaks(hand2.i1.ra.peaks)
plot(hand1.i2.ra.mag, type="l");
    abline(col="gray", v=hand1.i2.ra.peaks);
    abline(col="red", v=hand1.i2.ra.peaks.buildup);
    abline(col="blue", v=hand1.i2.ra.peaks.retract);
plot(hand2.i1.ra.mag, type="l");
    abline(col="gray", v=hand2.i1.ra.peaks);
    abline(col="red", v=hand2.i1.ra.peaks.buildup);
    abline(col="blue", v=hand2.i1.ra.peaks.retract);

hand1.i2.ra.p2p <- hand1.i2.ra.peaks.retract - hand1.i2.ra.peaks.buildup
hand2.i1.ra.p2p <- hand2.i1.ra.peaks.retract - hand2.i1.ra.peaks.buildup
# 1.603462s
hand1.give.halfperiod <- mean(hand1.i2.ra.p2p) * hand1.dt
# 1.602458s -- almost exactly the same as hand #1's
hand2.give.halfperiod <- mean(hand2.i1.ra.p2p) * hand2.dt


# Despite data from two different sensors, on different gloves, on different hands, the characteristic vectors
# of the "give" peaks are fairly close
# Note: we do not attempt to identify peaks in the "take"
hand1.i2.vectors <- norm(a.of(hand1.i2[hand1.i2.ra.peaks.buildup,]))
hand2.i1.vectors <- norm(a.of(hand2.i1[hand2.i1.ra.peaks.buildup,]))
plot3d(hand1.i2.vectors, xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1), col="blue")
points3d(norm(mean3d(hand1.i2.vectors)), col="blue", size=10)
points3d(hand2.i1.vectors, col="green")
points3d(norm(mean3d(hand2.i1.vectors)), col="green", size=10)


# repeat the above circular buffer operations, but using a more accurate half-period which is itself
# derived from the circular buffer data
# This now looks very clean, with almost exactly one peak and exactly one spike per gesture,
# the spike being very close to (usually immediately after) the peak.
hand1.len <- hand1.give.halfperiod %/% hand1.dt
hand2.len <- hand2.give.halfperiod %/% hand2.dt
hand1.i1.ra <- running.average.3d(hand1.i1, hand1.len)
hand1.i2.ra <- running.average.3d(hand1.i2, hand1.len)
hand2.i1.ra <- running.average.3d(hand2.i1, hand2.len)
hand2.i2.ra <- running.average.3d(hand2.i2, hand2.len)
hand1.i1.ra.mag <-mag(hand1.i1.ra)
hand1.i2.ra.mag <-mag(hand1.i2.ra)
hand2.i1.ra.mag <- mag(hand2.i1.ra)
hand2.i2.ra.mag <- mag(hand2.i2.ra)
ma <- 0.12
hand1.i2.ra.peaks <- find.peaks(hand1.i2.ra.mag, ma)
hand2.i1.ra.peaks <- find.peaks(hand2.i1.ra.mag, ma)
plot(hand1.i2.ra.mag, type="l"); abline(col="blue", v=hand1.i2.ra.peaks); abline(col="red", v=hand1.i2.spikes);
plot(hand2.i1.ra.mag, type="l"); abline(col="blue", v=hand2.i1.ra.peaks); abline(col="red", v=hand2.i1.spikes);

# example of a single handoff
plot(hand2.i1.ra.mag[530:950], x=(0:(950-530))*hand2.dt, type="l", xlab="time (s)", ylab="hand2 (giving) smoothed");
abline(col="blue", v=hand2.dt*(hand2.i1.ra.peaks-530)); abline(col="red", v=hand2.dt*(hand2.i1.spikes-530));

# unfortunately, the Arduino Nano's 2KB of memory makes a circular buffer infeasible,
# so we are limited to the simple band-pass technique.  However, we will use the more accurate frequency estimate.
hp <- c(hand1.i2.ra.p2p * hand1.dt, hand2.i1.ra.p2p * hand2.dt)
freq.low <- 1/(2*(mean(hp)+sd(hp)))
freq.high <- 1/(2*(mean(hp)-sd(hp)))
rc.low <- 1/(2 * pi * freq.low)
rc.high <- 1/(2 * pi * freq.high)
hand1.i2.bp <- bandpass.3d(a.of(hand1.i2), hand1.dt, rc.low, rc.high)
hand2.i1.bp <- bandpass.3d(a.of(hand2.i1), hand2.dt, rc.low, rc.high)
hand1.i2.bp.mag <- mag(hand1.i2.bp)
hand2.i1.bp.mag <- mag(hand2.i1.bp)
ma <- 0.05
hand1.i2.bp.peaks <- find.peaks(hand1.i2.bp.mag, ma)
hand2.i1.bp.peaks <- find.peaks(hand2.i1.bp.mag, ma)
plot(hand1.i2.bp.mag, type="l"); abline(col="blue", v=hand1.i2.bp.peaks); abline(col="red", v=hand1.i2.spikes);
plot(hand2.i1.bp.mag, type="l"); abline(col="blue", v=hand2.i1.bp.peaks); abline(col="red", v=hand2.i1.spikes);
