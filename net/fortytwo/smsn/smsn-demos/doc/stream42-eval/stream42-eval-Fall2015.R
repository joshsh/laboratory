
################################################################################
# create calling script (Bash)

#for i in 50 100 150 200 250 300 350 400 450 500 550 600 650 700 750 800 850 900 950 1000; \

#export JAVA_OPTIONS="-Xms8G -Xmx8G"
export JAVA_OPTIONS="-Xms12G -Xmx12G"

for j in 1 2 3 4; do
  for i in 100 200 300 400 500 600 700 800 900 1000; \
    do echo "./new-sesamestream-evaluation.sh -l 300 -p $i -t $j -q topics > /tmp/sp2bench/$i/topics-p$i-t$j.txt 2> /tmp/sp2bench/$i/topics-p$i-t$j-error.out"; done; done

for j in 1 2 3 4; do
  for i in 100 200 300 400 500 600 700 800 900 1000; \
    do echo "./new-sesamestream-evaluation.sh -l 300 -p $i -t $j -q friends > /tmp/sp2bench/$i/friends-p$i-t$j.txt 2> /tmp/sp2bench/$i/friends-p$i-t$j-error.out"; done; done

for j in 1 2 3 4; do
  for i in 100 200 300 400 500 600 700 800 900 1000; \
    do echo "./new-sesamestream-evaluation.sh -l 300 -p $i -t $j -q friends,topics > /tmp/sp2bench/$i/both-p$i-t$j.txt 2> /tmp/sp2bench/$i/both-p$i-t$j-error.out"; done; done


################################################################################
# convert results to R-friendly TSV (Bash)

for k in friends topics both; do \
  for j in 1 2 3 4; do \
    for i in 100 200 300 400 500 600 700 800 900 1000; do \
      for m in MATCH EVENT TOO_SHORT; do \
        cd /tmp/sp2bench/$i && cat $k-p$i-t$j.txt | grep $m > $k-p$i-t$j-$m.tsv; done; done; done; done


################################################################################
# load results (expects result data in "/tmp/sp2bench")

size <- 100 * 1:10
threads <- 1:4
query <- c("friends", "topics", "both")

data <- expand.grid(size=size,threads=threads,query=query)
all.params <- data

fix.time <- function(df) {
  matchtime <- df$time

  eventtime <- df$eventtime
  # fix for a strange issue whereby the event time is off by 12h, presumably due to date formatting
  if (matchtime - eventtime >= 43200000) {
    eventtime <- eventtime + 43200000
  }

  # TODO: temporary fix for millisecond precision issue
  eventtime <- floor(eventtime/1000)*1000

  eventtime
}

matches <- function(s, t, q) {
  tmp <- read.table(file(paste("/tmp/sp2bench/", s, "/", q, "-p", s, "-t", t, "-MATCH.tsv", sep="")), header=FALSE,
             col.names=c("time", "match", "query", "eventtime", "actor1", "actor2", "aux"))
  if (0 == nrow(tmp)) {
      NULL
  } else {
    eventtime <- sapply(1:nrow(tmp), function(i) {d <- tmp[i,]; fix.time(d)})

    data.frame(size=s, threads=t, query=q, time=tmp$time, eventtime=eventtime,
      actor1=tmp$actor1, actor2=tmp$actor2, aux=tmp$aux)
  }
}
events <- function(s, t, q) {
  tmp <- read.table(file(paste("/tmp/sp2bench/", s, "/", q, "-p", s, "-t", t, "-EVENT.tsv", sep="")), header=FALSE,
    col.names=c("time", "event", "thread.id", "actor1", "actor2"))
  if (0 == nrow(tmp)) {
      NULL
  } else {
    data.frame(size=s, threads=t, query=q, time=tmp$time, thread.id=tmp$thread.id, actor1=tmp$actor1, actor2=tmp$actor2)
  }
}
tshorts <- function(s, t, q) {
  file.name <- paste("/tmp/sp2bench/", s, "/", q, "-p", s, "-t", t, "-TOO_SHORT.tsv", sep="")
  tmp <- tryCatch(
    read.table(file(file.name), header=FALSE,
        col.names=c("time", "tooshort", "thread.id", "delay")),
    error = function(e) {NULL})
  if (is.null(tmp) | 0 == nrow(tmp)) {
      NULL
  } else {
    data.frame(size=s, threads=t, query=q, time=tmp$time, thread.id=tmp$thread.id, delay=tmp$delay)
  }
}

bind.results <- function(func) {
  df <- NULL
  for (i in 1:nrow(data)) { d <- data[i,]; rbind(df, func(d$size, d$threads, d$query)) -> df }
  df[with(df, order(time)),]
}

all.matches <- bind.results(matches)
all.events <- bind.results(events)
all.tshorts <- bind.results(tshorts)


################################################################################
# analysis of results

select.sim <- function(data, s, t, q) {
    subset(data, size==s & threads==t & query==q)
}

# see how much data we have in each run (including whether we are missing any runs)
records.by.run <- function(df) {
  data.frame(data, count=sapply(1:nrow(data),
    function(i){d <- data[i,]; nrow(select.sim(df, d$size, d$threads, d$query))}))
}

# there are "almost no" non-unique matches
tmp <- data.frame(eventtime=all.matches$eventtime, actor1=all.matches$actor1, actor2=all.matches$actor2, aux=all.matches$aux)
nrow(tmp) - nrow(unique(tmp))

records.by.run(all.matches)
records.by.run(all.events)
records.by.run(all.tshorts)


########################################
# comparing "delay too short" exceptions with source events (--> limits of throughput)
# do this first so as to attach the wait times to all.events

too.short.rel <- function(s, t, q) {
  nrow(select.sim(all.tshorts, s, t, q)) / nrow(select.sim(all.events, s, t, q))
}

na.to.zero <- function(v) { sapply(v, function(x) {if (is.na(x)) 0 else x}) }

# although threads have no effect on latency,
# threads have a clear negative effect on rate of exceptions
table(all.tshorts$threads)

# roughly: rate of exceptions by thread.  4 threads have less than half as many exceptions as 1.
# However, see below.  Although there are fewer exceptions with more threads, the exceptions are disproportionately long.
table(all.tshorts$threads) / table(all.events$threads)

ae <- data.frame(ev.id=row.names(all.events), all.events)
tmp <- merge(ae, data.frame(time=all.tshorts$time, thread.id=all.tshorts$thread.id, delay=all.tshorts$delay),
  by=c("thread.id","time"))
tmp <- data.frame(ev.id=tmp$ev.id, thread.id=tmp$thread.id, time=tmp$time, delay=tmp$delay)
# delay timestamp is not necessarily the same as the next event's timestamp,
# but we lose only around 6% of tshort exceptions by making this assumption.
# The remainder is a representative sample.
(nrow(all.tshorts) - nrow(tmp)) / nrow(all.tshorts)

tmp <- merge(ae, tmp, by="ev.id", all.x=TRUE)
tmp <- data.frame(ev.id=tmp$ev.id, thread.id=tmp$thread.id.x, time=tmp$time.x, delay=tmp$delay)
tmp <- tmp[with(tmp, order(thread.id, time)),]
tmp <- data.frame(tmp[2:nrow(tmp),], prevtime=tmp$time[1:(nrow(tmp)-1)])
tmp <- data.frame(ev.id=tmp$ev.id, delay=tmp$delay, elapsed=(tmp$time - tmp$prevtime))
tmp <- data.frame(ev.id=tmp$ev.id,
  delay=na.to.zero(tmp$delay), elapsed=na.to.zero(tmp$elapsed), wait=na.to.zero(tmp$elapsed - tmp$delay))
tmp <- merge(ae, tmp, by="ev.id", all.x=TRUE)

# do this only once
bk <- all.events
all.events <- tmp


########################################
# temporary workaround for second- (rather than millisecond-) level precision in event timestamps

deg.nonunique <- function(l) {
    tmp <- length(l);
    (tmp - length(unique(l)))/tmp
}
# 50% of all events have a non-unique rounded time.
#deg.nonunique(all.events.rounded$roundedtime)

second.precision <- function(t) { floor(t/1000)*1000 }

# note: all.events.rounded won't be needed once results reflect the millisecond-precision fix
all.events.rounded <- data.frame(all.events, roundedtime=sapply(all.events$time, second.precision))

length(unique(all.events$time)) - length(all.events.rounded$roundedtime)

# there are no repeats here
tmp <- data.frame(time=all.events$time, actor1=all.events$actor1, actor2=all.events$actor2)
nrow(tmp)-nrow(unique(tmp))

tmp1 <- data.frame(ev.id=all.events$ev.id, wait=all.events$wait, eventtime=all.events$time, roundedtime=all.events.rounded$roundedtime,
  actor1=all.events$actor1, actor2=all.events$actor2)
tmp2 <- data.frame(ev.id=all.events$ev.id, wait=all.events$wait, eventtime=all.events$time, roundedtime=all.events.rounded$roundedtime,
  actor2=all.events$actor1, actor1=all.events$actor2)
tmp <- subset(rbind(tmp1, tmp2), as.character(actor1) < as.character(actor2))
# now using ordered actors, there are no repeats of events with rounded times
nrow(tmp) - nrow(all.events)
nrow(tmp) - nrow(unique(tmp))
events.with.actors <- tmp


########################################
# joining matches to source events (--> latency)

tmp <- subset(merge(events.with.actors, all.matches, by.x="roundedtime", by.y="eventtime"),
  as.character(actor1.x) == as.character(actor1.y) & as.character(actor2.x) == as.character(actor2.y))
tmp <- tmp[with(tmp, order(eventtime)),]

tmp2 <- data.frame(eventtime=tmp$eventtime, roundedtime=tmp$roundedtime, time=tmp$time,
  raw.latency=(tmp$time - tmp$eventtime),
  latency=(tmp$time - tmp$eventtime + tmp$wait),
  size=tmp$size, threads=tmp$threads, query=tmp$query,
  actor1=tmp$actor1.x, actor2=tmp$actor2.x, aux=tmp$aux)
nrow(tmp2) - nrow(unique(tmp2))
events.with.matches <- tmp2

# make sure we have merged results for every run
records.by.run(events.with.matches)


########################################
# find the proportion of waiting time by run

wait.by.run <- data.frame(data, wait.per.match=sapply(1:nrow(data), function(i) {
  d <- data[i,]
  df <- select.sim(events.with.matches, d$size, d$threads, d$query)
  #nrow(subset(df, df$latency != df$raw.latency)) / nrow(df)
  sum(df$latency - df$raw.latency) / nrow(df)
}))

# it is pretty clear that with more threads comes *much more* waiting time per match,
# even though there are less events overall which have to wait
wait.by.run


########################################
# look at some individual results

select.latency <- function(s,t,q,lim) {
    l <- select.sim(events.with.matches, s, t, q)$latency
    l[l < lim]
}

# peaks at 9ms, 13ms (Marvin)
# peaks at 16, 33 (EC2)
plot(density(select.latency(100,1,"topics",400)))
# peaks around 19 and 45ms
# peak at 15 (EC2)
plot(density(select.latency(200,1,"topics",100)))
# peaks around 18 and 42 (EC2)
plot(density(select.latency(300,1,"topics",400)))
# peak around 30 (EC2)
plot(density(select.latency(300,2,"topics",400)))
plot(density(select.latency(300,3,"topics",4000)))
# peak around 20 and 45 (EC2)
plot(density(select.latency(300,4,"topics",100)))


plot.by.variable.people <- function(query, threads) {
  lim <- 100
  plot(density(select.latency(100,threads,query,lim)), col="red", xlim=c(0,lim),
    main="delay (density) by variable # people")
  lines(density(select.latency(200,threads,query,lim)), col="orange")
  lines(density(select.latency(300,threads,query,lim)), col="yellow")
  lines(density(select.latency(400,threads,query,lim)), col="green")
  lines(density(select.latency(500,threads,query,lim)), col="cyan")
  lines(density(select.latency(600,threads,query,lim)), col="blue")
  lines(density(select.latency(700,threads,query,lim)), col="purple")
  lines(density(select.latency(800,threads,query,lim)), col="pink")
  lines(density(select.latency(900,threads,query,lim)), col="gray")
  lines(density(select.latency(1000,threads,query,lim)), col="black")
}

plot.by.variable.threads <- function(query, people) {
  lim <- 200
  plot(density(select.latency(people,1,query,lim)), col="red", xlim=c(0,lim),
    main="delay (density) by variable # threads")
  lines(density(select.latency(people,2,query,lim)), col="green")
  lines(density(select.latency(people,3,query,lim)), col="blue")
  lines(density(select.latency(people,4,query,lim)), col="purple")
}

plot.by.variable.people("topics",1)

# peak is around 75ms with no clear relationship to #threads
plot.by.variable.threads("friends",1000)
# peak is around 40ms, again with no clear relationship to #threads
plot.by.variable.threads("friends",500)

l <- select.latency(500,4,"friends",200); mean(l); sd(l)


# get a rough idea of whether the system is slowing down
plot.sim <- function(s, t, q) {
  es <- select.sim(all.events,s,t,q)
  ms <- select.sim(all.matches,s,t,q)
  ts <- select.sim(all.tshorts,s,t,q)
  plot(es$time, type="l")
  abline(col="gray", h=ms$time)
  abline(col="red", h=ts$time)
  lines(es$time)
}

plot.sim(100,1,"friends")


########################################
# combine latency results with success probability

###
# imported from personal-equation.R

pss <- 50
jnd <- 110
lag.mean <- 82
lag.sd <- 24
bt.mean <- 34
bt.sd <- 10
hand.offset <- 40
# see ~/data/research/thesis/latency/wifi-pings
wifi <- wifi.pings
intgr.nat <- function(mu1,sd1,tcep) {
    dom <- c(-1000:1000)
    dist1 <- dnorm(dom, mean=mu1, sd=sd1)
    mx <- max(dist1)
    nw <- wifi + bt.mean + hand.offset + tcep
    dist2 <- density(nw, from=min(dom), to=max(dom), n=length(dom))$y
    # assuming unit increments on x axis
    sum(dist1/mx*dist2)
}
# probability of synchrony, by tcep, based on sampled bias time
pos.nat <- function(tcep) {
    intgr.nat(pss,jnd,tcep)
}

###

library(ggplot2)
library(gridExtra)
require(grid)

# see http://people.duke.edu/~csm29/ggplot2/ggplot2.html
interquartile <- function(x){
    out <- quantile(x, probs = c(0.25, 0.5, 0.75))
    names(out) <- c("ymin", "y", "ymax")
    out
}

plot.by.query.new <- function(t) {
  toplot <- subset(events.with.matches, threads==t)

  ggplot(toplot, aes(x=size, y=latency, col = query, group = query))+
    stat_summary(fun.data = "interquartile", geom = "errorbar", width=20.0)+
    stat_summary(fun.y = 'median', geom='line', lwd=1.5) +
    theme_bw() +
    theme(legend.position="top", legend.title=element_blank(),
      axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(),
      plot.margin=unit(c(0,0.5,0,0.5), "cm")) +
    ylab(expression(paste("T"[CEP], " (ms)")))
}

plot.prob.by.query.prepare <- function(t) {
  tmp <- subset(events.with.matches, threads==t)
  data.frame(tmp, success=sapply(tmp$latency, pos.nat))
}

plot.prob.by.query <- function(toplot) {
  ggplot(toplot, aes(x=size, y=success, col = query, group = query))+
    stat_summary(fun.data = "interquartile", geom = "errorbar", width=20.0)+
    stat_summary(fun.y = 'median', geom='line', lwd=1.5) +
    theme_bw() +
    theme(legend.position = "none", plot.margin=unit(c(0,0.5,0.5,0.5), "cm")) +
    #guides(colour = guide_legend(override.aes = list(alpha = 0))) +
    xlab(expression(paste("n = |G|/100 = ", f %.% 360,"s"))) +
    ylab(expression("p"[CEP]))
}

pdf("/tmp/graphic.pdf", width=6.25, height=5.0)
par(mar=c(0,0,0,0))
  threads <- 4
  #toplot <- plot.prob.by.query.prepare(threads)
  p1 <- plot.by.query.new(threads)
  p2 <- plot.prob.by.query(toplot)
  grid.arrange(p1,p2,ncol = 1,padding=0)
dev.off()


########################################
# look only at first responses

# this makes only the slightest difference in response times
# It is perhaps slightly more noticeable for the "both" runs.
tmp <- subset(data.frame(events.with.matches, dup=duplicated(events.with.matches$eventtime)), dup==FALSE)
bk <- events.with.matches
events.with.matches <- tmp

