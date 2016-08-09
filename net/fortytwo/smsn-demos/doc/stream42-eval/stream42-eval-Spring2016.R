# all statements in this evaluation are relevant; they all are matched by at least one query
# two queries, with different scaling behavior.  The combination takes on the worst performance of the two.

# each run lasts for 60 seconds
time.limit <- 60

# each handshake graph contains 6 statements
statements.per.input.event <- 6

# note: the average number of Friends solutions per result-producing events is pretty constant from p=100 to p=1000
#   Friends: (1.39543931698 .. 1.38659205116)
# however, the number of Topics solutions falls, despite a constant hit ratio of solution events to source events:
#  (5.70475161987 .. 1.76508006142)
# Friends hit ratio: (0.49309885425 .. 0.500199942873)
# Topics hit ratio: (0.246735944578 .. 0.260439874322)

solutions.per.output.event.Friends <- 1.39


n.people <- 50 * 1:20
n.threads <- 1:8
queries <- c("Friends", "Topics", "FriendsTopics")

# use the rate of input events for throughput; we can assume we get one output event per input event if we want to



################################################################################
# stats on the source data

# Bash
# cd /tmp/stream42
# for i in 50 100 150 200 250 300 350 400 450 500 550 600 650 700 750 800 950 1000; do cat $i/*.nt|wc -l; done

# number of pre-loaded source statements
n.statements <- c(6686, 12379, 18003, 23453, 29068, 34685, 40353, 45919, 51608, 57296, 62928, 68572, 74299, 80066, 85801, 91619, 97372, 102931, 108580, 114245)

# approaches 114 statements/person asymptotically, but starts out at 133
plot(x=n.people, y=n.statements/n.people, type="l")


################################################################################
# load solutions (expects result data in "/tmp/sp2bench")


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

bind.solutions <- function(suffix, colnames) {
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

all.starttime <- bind.solutions("STARTED", c("time", "thread.id", "type"))
all.stoptime <- bind.solutions("STOPPED", c("time", "thread.id", "type"))
all.statements <- bind.solutions("STATEMENTS", c("time", "thread.id", "type", "n.statements"))
all.solutions <- bind.solutions("RESULTS", c("time", "thread.id", "type", "n.solutions", "query"))


################################################################################
# analysis of solutions

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
#records.by.run(all.solutions)

# these should be non-empty only for multiple-query runs
check.runs(all.starttime)
check.runs(all.stoptime)
check.runs(all.statements)
check.runs(all.solutions)


########################################
# look at some individual solutions

to.rate <- function(total) {
  total / time.limit
}

# TODO
to.statement.throughput <- function(total) {
  total / time.limit
}
# TODO
to.result.throughput <- function(total) {
  total / time.limit
}

to.statement.latency <- function(n.statements) {
  1000 * time.limit / n.statements
}

# finds the latency in milliseconds
to.result.latency <- function(n.solutions) {
  1000 * statements.per.input.event * time.limit / n.solutions
}

id <- function(x) {x}

statements.by.people <- function(q, t) {
  df <- subset(all.statements, queries==q & n.threads==t)
  agg <- stats::aggregate(df$n.statements, list(key = df$n.people), sum)
  data.frame(key=agg$key, total=agg$x)
}

solutions.by.people <- function(q, t) {
  df <- subset(all.solutions, queries==q & n.threads==t)
  agg <- stats::aggregate(df$n.solutions, list(key = df$n.people), sum)
  data.frame(key=agg$key, total=agg$x)
}

solutions.per.statement.by.people <- function(q, t) {
  sols <- solutions.by.people(q, t)
  stmts <- statements.by.people(q, t)
  data.frame(key=sols$key, sols=sols, stmts=stmts, sols.per.stmt=(sols/stmts))
}

solutions.per.query.by.people <- function(q, query.list, t) {
  out <- NULL
  for (q2 in query.list) {
    df <- subset(all.solutions, query==q2 & queries==q & n.threads==t)
    agg <- stats::aggregate(df$n.solutions, list(key = df$n.people), sum)
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
plot.aggregate(solutions.by.people, to.rate, "Friends", n.threads, "# people", "# solutions / s", NULL)

# plot latency
plot.aggregate(statements.by.people, to.statement.latency, "Friends", n.threads, "# people", "input latency (ms)", NULL)
plot.aggregate(solutions.by.people, to.result.latency, "Friends", n.threads, "# people", "output latency (ms)", NULL)


plot.aggregate(solutions.by.people, to.result.latency, "Friends", n.threads, "# people", "# statements / s", NULL)
plot.aggregate(solutions.by.people, to.result.latency, "Topics", n.threads, "# people", "# statements / s", NULL)
plot.aggregate(solutions.by.people, to.result.latency, "FriendsTopics", n.threads, "# people", "# statements / s", NULL)

# this is just as expected on the basis of:
#  6 statements per input event
#  approx. 4 input events per output evend (Friends)
#  around 1.39 solutions per output event (Friends)
solutions.per.statement.by.people("Friends", 1)


t <- 4
df1 <- solutions.by.people("Friends", t)
df2 <- solutions.by.people("Topics", t)
df3 <- solutions.by.people("FriendsTopics", t)
plot(x=df1$key, y=df1$rate, col="blue", type="l", ylim=c(0, max(df1$rate, df2$rate, df3$rate)))
df <- solutions.by.people("Topics", 1)
lines(x=df2$key, y=df2$rate, col="red")
df <- solutions.by.people("FriendsTopics", 1)
lines(x=df3$key, y=df3$rate, col="purple")


################################################################################
# plot for multithreading


################################################################################
# efficiency/speedup

efficiency.matrix <- function(query, func2) {
  s1 <- func2(query, 1)
  df <- data.frame(key=s1$key, size=n.statements)
  for (t in n.threads) {
    sn <- func2(query, t)
    sup <- data.frame(key=s1$key, efficiency=(sn$total/(s1$total * t)))
    data.frame(df, sup$efficiency) -> df
  }
  df
}

s <- efficiency.matrix("Friends", statements.by.people)
colnames(s) <- c("key", as.character(n.threads))


##########
# plot the efficiency matrix

library(reshape2)
library(ggplot2)

queries <- "Friends"
#queries <- "Topics"  # note unexpected superlinear speedup
mat <- efficiency.matrix(queries, statements.by.people)
mat <- mat[,-1]
colnames(mat) <- c("size", as.character(n.threads))
melted <- melt(mat, id.var = "size")
melted <- data.frame(size=melted$size, threads=melted$variable, efficiency=melted$value)

pdf("/tmp/graphic.pdf", width=6, height=2.2)
ggplot(melted, aes(as.numeric(size), as.factor(threads), group=threads)) +
    geom_tile(aes(fill = efficiency)) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_bw() +
    xlab("dataset size") +
    ylab("total threads")
dev.off()


################################################################################
# probability of synchrony


audio.audio.ps <- function(delay) {
  pss <- 0
  sd <- 2
  dnorm(delay, mean=pss, sd=sd)/dnorm(0, mean=pss, sd=sd)
}

to.result.ps <- function(x) { audio.audio.ps(to.result.latency(x)) }
to.statement.ps <- function(x) { audio.audio.ps(to.statement.latency(x)) }

plot.sim.prob <- function(func, query, xlab, ylab, main) {
  xlab <- "initial dataset size"
  ylab <- "probability of synchrony"
  main <- NULL
  filter <- to.result.ps
  allmax <- 0
  threads <- n.threads
  for (t in threads) {
    mx <- max(filter(func(query, t)$total))
    if (mx > allmax) allmax <- mx
  }
  for (i in 1:length(threads)) {
    t <- threads[i]
    df <- func(query, t)
    fdf <- filter(df$total)
    if (i == 1) {
      plot(x=n.statements, y=fdf, xlab=xlab, ylab=ylab, main=main, type="l", ylim=c(0, allmax), col="blue")
    } else {
      lines(x=n.statements, y=fdf)
    }
  }
}

plot.sim.prob(solutions.by.people, "Friends")


plot.aggregate(solutions.by.people, to.result.ps, "Friends", n.threads, "# people", "probability of synchrony", NULL)
plot.aggregate(solutions.by.people, to.result.ps, "Topics", n.threads, "# people", "probability of synchrony", NULL)
plot.aggregate(solutions.by.people, to.result.ps, "FriendsTopics", n.threads, "size of static dataset", "probability of synchrony", NULL)



########################################
# ggplot viz of simple audio-audio synchrony probability

library(ggplot2)
library(gridExtra)
require(grid)

func2.grid <- function(func, filter, queries) {
  df <- NULL
  for (i in n.threads) {
    df <- rbind(df, data.frame(total=filter(func(queries, i)$total), threads=i))
  }
  series <- paste(queries, as.character(df$threads), sep="")
  data.frame(size=n.statements, people=n.people, threads=df$threads, value=df$total, queries=queries, series=series)
}

plot.func2 <- function(func, filter, queries, ylab) {
  df <- func2.grid(func, filter, queries)
  ggplot(data=df, aes(x=size, y=value, group = threads, colour = threads)) +
    geom_line() +
    theme_bw() +
    xlab("dataset size") +
    ylab(ylab)
}


func2.grid(solutions.by.people, to.result.latency, "Friends")

plot.func2(solutions.by.people, to.result.latency, "Friends", "latency (ms)")
plot.func2(solutions.by.people, to.result.latency, "FriendsTopics", "latency (ms)")

plot.func2(solutions.by.people, to.result.ps, "Friends", "probability of audio-audio synchrony")
plot.func2(solutions.by.people, to.result.ps, "Topics", "probability of audio-audio synchrony")
plot.func2(solutions.by.people, to.result.ps, "FriendsTopics", "probability of audio-audio synchrony")


# this is misleading; "probability of synchrony" does not apple to incoming statements
plot.func2(statements.by.people, to.statement.ps, "FriendsTopics", "probability of audio-audio synchrony")


##########
# here is the diagram

plot.prob.top <- function(toplot, ylab) {
  ggplot(data=toplot, aes(x=size, y=value, group = threads, colour = threads)) +
    geom_hline(yintercept = 2, color="red", linetype="dashed") +
    geom_line() +
    theme_bw() +
    ylab(ylab) +
    theme(legend.position="top",
      axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(),
      plot.margin=unit(c(0,0.5,0,0.95), "cm"))
}

plot.prob.bottom <- function(toplot, ylab) {
  ggplot(data=toplot, aes(x=size, y=value, group = threads, colour = threads)) +
    geom_hline(yintercept = 0.60653, color="red", linetype="dashed") +
    geom_line() +
    theme_bw() +
    xlab("dataset size") +
    ylab(ylab) +
    theme(legend.position = "none", plot.margin=unit(c(0,0.5,0.5,0.5), "cm"))
}

pdf("/tmp/graphic.pdf", width=5.5, height=5.0)
par(mar=c(0,0,0,0))
  toplot1 <- func2.grid(solutions.by.people, to.result.latency, "FriendsTopics")
  toplot2 <- func2.grid(solutions.by.people, to.result.ps, "FriendsTopics")

  p1 <- plot.prob.top(toplot1, "event latency")
  p2 <- plot.prob.bottom(toplot2, "P(synchrony)")
  grid.arrange(p1,p2,ncol=1,padding=0)
dev.off()


########################################
# plot the three query sets in terms of throughput

melted <- NULL
melted <- rbind(melted, func2.grid(statements.by.people, to.rate, "Friends"))
melted <- rbind(melted, func2.grid(statements.by.people, to.rate, "Topics"))
melted <- rbind(melted, func2.grid(statements.by.people, to.rate, "FriendsTopics"))

pdf("/tmp/graphic.pdf", width=6, height=3)
ggplot(melted, aes(x=size, y=value, col = queries, group = series)) +
  #geom_line(lwd=1.3) +
  geom_line() +
  geom_line(lwd=1.5, data=subset(func2.grid(statements.by.people, to.rate, "Friends"), threads==1)) +
  geom_line(lwd=1.5, data=subset(func2.grid(statements.by.people, to.rate, "Topics"), threads==1)) +
  geom_line(lwd=1.5, data=subset(func2.grid(statements.by.people, to.rate, "FriendsTopics"), threads==1)) +
  theme_bw() +
  #theme(legend.position="top", legend.title=element_blank()) +
  xlab("dataset size") +
  ylab("throughput (statements / s)") +
  scale_y_log10()
dev.off()




