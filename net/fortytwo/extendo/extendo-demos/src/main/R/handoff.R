#install.packages("rgl")
library(rgl)

mag <- function(p) { sqrt(p$x^2 + p$y^2 + p$z^2) }
norm <- function(p) { p / mag(p) }
mean3d <- function(p) { data.frame(x=mean(p$x), y=mean(p$y), z=mean(p$z))}

# minimum trough-to-peak amplitude of features of interest
min.amp <- 0.3

find.peaks <- function(x) {
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
        prev.x <- xi
        if (rising) {
            if (!prev.rising) { # local minimum
                if (high) {
                    if (ref.x - xi >= min.amp) {
                        peaks <- c(peaks, ref.i)
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

########################################
# load and crop data

hand1 <- read.csv(file("/tmp/hand1.csv"), header=FALSE)
hand2 <- read.csv(file("/tmp/hand2.csv"), header=FALSE)

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



dir.of <- function(series) {
    a <- data.frame(x=series$V2,y=series$V3,z=series$V4)
    peaks <- find.peaks(mag(a))

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

series <- hand2.i1
#series <- hand1.i1


#dir.hand1.i1 <- dir
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

