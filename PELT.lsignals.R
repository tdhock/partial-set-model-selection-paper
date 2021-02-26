source("packages.R")
nb.dt <- data.table::fread("../LOPART-paper/data-for-LOPART-signals.csv.gz")
nb.size <- nb.dt[, .(N.data=.N), by=.(sequenceID)][order(N.data)]
dir.create("lsignals")
max.pen <- 1e5
seq.i <- 300

mypelt <- function(sumstat, penalty){
  plist <- changepoint:::PELT(
    sumstat, pen = penalty, cost_func = "mean.norm", 
    shape = 1, minseglen = 1)
  last <- nrow(sumstat)
  cost <- plist[["lastchangelike"]][[last]]
  changes <- plist[["ncpts"]][[last]]-1
  loss <- cost-changes*penalty  
  list(cost=cost, changes=changes, loss=loss)
}
ss <- function(x){
  cbind(
    c(0, cumsum(x)),
    c(0, cumsum(x^2)), 
    cumsum(c(0, (x - mean(x))^2)))
}
penmap.crops <- function(x){
  N <- length(x)
  sumstat <- ss(x)
  next.pen.vec <- c(0, max.pen)
  m <- new(penmap::penmap)
  while(length(next.pen.vec)){
    for(penalty in next.pen.vec){
      plist <- mypelt(sumstat, penalty)
      m$insert(penalty, plist$loss, plist$changes)
    }
    next.pen.vec <- m$helpful()
  }#while
  m$df()
}

seq.i.vec <- 1:nrow(nb.size)
seq.i.vec <- 1:300
PELT.lsignals.list <- list()
for(seq.i in seq.i.vec){
  seq.size <- nb.size[seq.i]
  seqID <- seq.size[["sequenceID"]]
  cache.rds <- paste0("lsignals/", seqID, ".rds")
  if(file.exists(cache.rds)){
    this.result <- readRDS(cache.rds)
  }else{
    cat(sprintf(
      "%4d / %4d %s size=%d\n",
      seq.i, nrow(nb.size), seqID, seq.size[["N.data"]]))
    seq.dt <- nb.dt[seq.size, on=.(sequenceID)]
    seq.vec <- seq.dt[["logratio"]]
    time.df <- microbenchmark::microbenchmark(pdpa={
      pdpa.fit <- jointseg::Fpsn(seq.vec, length(seq.vec))
    },
    penmap={
      penmap.fit <- penmap.crops(seq.vec)
    },
    crops={
      ignored <- capture.output({
        crops.fit <- changepoint::cpt.mean(
          seq.vec, penalty="CROPS", method="PELT", pen.value=c(0, max.pen))
      })
    },
    times=5)
    crops.dt.list <- list()
    s <- ss(seq.vec)
    for(pen.val in crops.fit@pen.value.full){
      d <- mypelt(s, pen.val)
      crops.dt.list[[paste(pen.val)]] <- with(d, data.table(
        method="crops", loss, complexity=changes))
    }
    all.model.list <- list(
      crops=do.call(rbind, crops.dt.list),
      pdpa=with(pdpa.fit, data.table(
        method="pdpa", loss=J.est, complexity=seq_along(J.est)-1)),
      penmap=data.table(penmap.fit)[
        0 <= size_after, .(
          method="penmap", loss=loss_after, complexity=size_after)])
    sel.model.list <- list()
    for(model.name in names(all.model.list)){
      sel.model.list[[model.name]] <- data.table(
        penaltyLearning::modelSelection(
          all.model.list[[model.name]]))[, .(method, loss, complexity)]
    }
    sel.model <- do.call(rbind, sel.model.list)
    both.models <- rbind(
      data.table(models="all", do.call(rbind, all.model.list)),
      data.table(models="selected", do.call(rbind, sel.model.list)))
    both.counts <- both.models[, data.table(
      count=.N,
      seq.size
    ), .(models, method)]
    time.dt <- data.table(time.df)
    time.dt[, seconds := time/1e9]
    seconds.dt <- time.dt[, data.table(
      median=median(seconds),
      q25=quantile(seconds, 0.25),
      q75=quantile(seconds, 0.75),
      seq.size
    ), by=.(method=expr)]
    this.result <- list(
      seconds=seconds.dt,
      models=both.counts)
    saveRDS(this.result, cache.rds)
  }
  PELT.lsignals.list[[seq.i]] <- this.result
}
(summary.dt <- do.call(rbind, lapply(PELT.lsignals.list, "[[", "summary")))

saveRDS(PELT.lsignals.list, file="PELT.lsignals.rds")
