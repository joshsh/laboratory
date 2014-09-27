library(rgl)

mag <- function(p) { sqrt(p$x^2 + p$y^2 + p$z^2) }
norm <- function(p) { p / mag(p) }

# import data using SIMPLE_OUTPUT in extendo_hand.ino
motion <- read.csv(file("/tmp/motion.csv"), header=FALSE)

# find sampling rate
times <- motion$V3
t1 <- times[1:(length(times)-1)]
t2 <- times[2:length(times)]
diff <- t2 - t1
mean(1000000/diff)
sd(1000000/diff)

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

