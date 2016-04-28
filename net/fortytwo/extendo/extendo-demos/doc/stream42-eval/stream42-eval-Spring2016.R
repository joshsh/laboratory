# all statements in this evaluation are relevant; they all are matched by at least one query
# two queries, with different scaling behavior.  The combination takes on the worst performance of the two.

# each run lasts for 60 seconds
time.limit <- 60

# each handshake graph contains 6 statements
statements.per.event <- 6

# note: the average number of Friends results per result-producing events is pretty constant from p=100 to p=1000
#   Friends: (1.39543931698 .. 1.38659205116)
# however, the number of Topics results falls, despite a constant hit ratio of solution events to source events:
#  (5.70475161987 .. 1.76508006142)
# Friends hit ratio: (0.49309885425 .. 0.500199942873)
# Topics hit ratio: (0.246735944578 .. 0.260439874322)


################################################################################
# load results (expects result data in "/tmp/sp2bench")

#size <- 100 * 1:7
#threads <- 1:4
#queries <- c("Friends", "Topics", "FriendsTopics")

n.people <- 50 * 1:20
n.threads <- c(1,2,4,8)
queries <- c("Friends", "Topics", "FriendsTopics")

all.params <- expand.grid(n.people=n.people,n.threads=n.threads,queries=queries)

read.output <- function(n.people, n.threads, queries, suffix, colnames) {
  filename <- paste("/tmp/stream42/", n.people, "/", queries, "-p", n.people, "-t", n.threads, ".out-", suffix, sep="")
  if (file.exists(filename)) {
    con <- file(filename)
    #on.exit(close(con))
    read.table(con, header=FALSE, col.names=colnames)
  } else {
    NULL
  }
}

bind.results <- function(suffix, colnames) {
  df <- NULL
  for (i in 1:nrow(all.params)) {
    d <- all.params[i,];
    output <- read.output(d$n.people, d$n.threads, d$queries, suffix, colnames)
    if (nrow(output) > 0) {
      rbind(df, cbind(d, output, row.names=NULL)) -> df
    }
  }
  df[with(df, order(time)),]
}

all.starttime <- bind.results("STARTED", c("time", "thread.id", "type"))
all.stoptime <- bind.results("STOPPED", c("time", "thread.id", "type"))
all.statements <- bind.results("STATEMENTS", c("time", "thread.id", "type", "n.statements"))
all.results <- bind.results("RESULTS", c("time", "thread.id", "type", "n.results", "query"))


################################################################################
# analysis of results

select.sim <- function(data, p, t, q) {
    subset(data, n.people==p & n.threads==t & queries==q)
}

# see how much data we have in each run (including whether we are missing any runs)
records.by.run <- function(df) {
  data.frame(all.params, count=sapply(1:nrow(all.params), function(i) {
    d <- all.params[i,];
    nrow(select.sim(df, d$n.people, d$n.threads, d$queries))
  }))
}

check.runs <- function(data) {
  byrun <- records.by.run(data)
  subset(byrun, n.threads != count)
}

#records.by.run(all.starttime)
#records.by.run(all.stoptime)
#records.by.run(all.statements)
#records.by.run(all.results)

# these should be non-empty only for multiple-query runs
check.runs(all.starttime)
check.runs(all.stoptime)
check.runs(all.statements)
check.runs(all.results)


########################################
# look at some individual results

to.rate <- function(total) {
  total / time.limit
}

to.statement.latency <- function(n.statements) {
  1000 * time.limit / n.statements
}

# finds the latency in milliseconds
to.result.latency <- function(n.results) {
  1000 * statements.per.event * time.limit / n.results
}


statements.by.people <- function(q, t) {
  df <- subset(all.statements, queries==q & n.threads==t)
  agg <- aggregate(df$n.statements, list(key = df$n.people), sum)
  data.frame(key=agg$key, total=agg$x)
}

results.by.people <- function(q, t) {
  df <- subset(all.results, queries==q & n.threads==t)
  agg <- aggregate(df$n.results, list(key = df$n.people), sum)
  data.frame(key=agg$key, total=agg$x)
}

results.per.query.by.people <- function(q, query.list, t) {
  out <- NULL
  for (q2 in query.list) {
    df <- subset(all.results, query==q2 & queries==q & n.threads==t)
    agg <- aggregate(df$n.results, list(key = df$n.people), sum)
    rbind(out, data.frame(query=q2, key=agg$key, total=agg$x)) -> out
  }
  out
}

plot.aggregate <- function(func, filter, query, threads, xlab, ylab, main) {
  allmax <- 0
  for (t in threads) {
    mx <- max(filter(func(query, t)$total))
    if (mx > allmax) allmax <- mx
  }
  for (i in 1:length(threads)) {
    t <- threads[i]
    df <- func(query, t)
    fdf <- filter(df$total)
    if (i == 1) {
      plot(x=df$key, y=fdf, xlab=xlab, ylab=ylab, main=main, type="l", ylim=c(0, allmax), col="blue")
    } else {
      lines(x=df$key, y=fdf)
    }
  }
}

# plot rate
plot.aggregate(statements.by.people, to.rate, "Friends", n.threads, "# people", "# statements / s", NULL)
plot.aggregate(results.by.people, to.rate, "Friends", n.threads, "# people", "# results / s", NULL)

# plot latency
plot.aggregate(statements.by.people, to.statement.latency, "Friends", n.threads, "# people", "input latency (ms)", NULL)
plot.aggregate(results.by.people, to.result.latency, "Friends", n.threads, "# people", "output latency (ms)", NULL)


df0 <- results.per.query.by.people("FriendsTopics", c("Friends", "Topics"), t)


t <- 4
df1 <- results.by.people("Friends", t)
df2 <- results.by.people("Topics", t)
df3 <- results.by.people("FriendsTopics", t)
plot(x=df1$key, y=df1$rate, col="blue", type="l", ylim=c(0, max(df1$rate, df2$rate, df3$rate)))
df <- results.by.people("Topics", 1)
lines(x=df2$key, y=df2$rate, col="red")
df <- results.by.people("FriendsTopics", 1)
lines(x=df3$key, y=df3$rate, col="purple")







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

