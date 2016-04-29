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
n.threads <- 1:8
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

id <- function(x) {x}

statements.by.people <- function(q, t) {
  df <- subset(all.statements, queries==q & n.threads==t)
  agg <- stats::aggregate(df$n.statements, list(key = df$n.people), sum)
  data.frame(key=agg$key, total=agg$x)
}

results.by.people <- function(q, t) {
  df <- subset(all.results, queries==q & n.threads==t)
  agg <- stats::aggregate(df$n.results, list(key = df$n.people), sum)
  data.frame(key=agg$key, total=agg$x)
}

results.per.query.by.people <- function(q, query.list, t) {
  out <- NULL
  for (q2 in query.list) {
    df <- subset(all.results, query==q2 & queries==q & n.threads==t)
    agg <- stats::aggregate(df$n.results, list(key = df$n.people), sum)
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
# speedup

speedup.matrix <- function(query, func2) {
  s1 <- func2(query, 1)
  df <- data.frame(key=s1$key)
  for (t in n.threads) {
    sn <- func2(query, t)
    sup <- data.frame(key=s1$key, speedup=(sn$total/(s1$total * t)))
    data.frame(df, sup$speedup) -> df
  }
  df
}

s <- speedup.matrix("Friends", statements.by.people)
colnames(s) <- c("key", as.character(n.threads))


##########
# plot the speedup matrix

library(reshape2)
library(ggplot2)
dat <- matrix(rnorm(100, 3, 1), ncol=10)
names(dat) <- paste("X", 1:10)
dat2 <- melt(dat, id.var = "X1")
ggplot(dat2, aes(as.factor(Var1), Var2, group=Var2)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(fill = dat2$value, label = round(dat2$value, 1))) +
    scale_fill_gradient(low = "white", high = "red")



mat <- speedup.matrix("Friends", statements.by.people)#[,-1]
colnames(mat) <- c("key", as.character(n.threads))
#names(dat) <- paste("X", 1:10)
mat2 <- melt(mat, id.var = "key")
ggplot(mat2, aes(as.factor(key), as.factor(variable), group=variable)) +
    geom_tile(aes(fill = value)) +
    #geom_text(aes(fill = mat2$value, label = round(mat2$value, 1))) +
    scale_fill_gradient(low = "white", high = "blue") +
    xlab("dataset size (??)") +
    ylab("total threads")

