source("packages.R")

future::plan("multicore")
nb.dt <- data.table::fread("../LOPART-paper/data-for-LOPART-signals.csv.gz")
setkey(nb.dt, sequenceID)
nb.size <- nb.dt[, .(N.data=.N), by=.(sequenceID)][order(N.data)]
dir.create("lsignals")
max.pen <- 1e5
penalty.grid <- c(0, exp(seq(-5, 7, l=10)), max.pen)
penalty.grid <- c(0, max.pen)
seq.i <- 300


get.error <- function(end){
  seg.dt <- data.table(end)
  seg.dt[, start := c(1, end[-.N]+1)]
  err.dt <- seq.dt[seg.dt, .(
    seg.error=sum((logratio-mean(logratio))^2),
    N=.N
  ), on=.(data.i >= start, data.i <= end), by=.EACHI]
  sum(err.dt[["seg.error"]])
}

microbenchmark::microbenchmark(
  fpop=fpop::Fpop(seq.vec, penalty)[["t.est"]],
  LOPART=LOPART::LOPART(seq.vec, data.frame(start=integer(),end=integer(),changes=integer()), penalty),
  penmap={
    m <- new(penmap::penmap)
    m$insert(0,1,1)
  },
  pelt=changepoint::cpt.mean(
    seq.vec, penalty="Manual",
    pen.value=penalty, method="PELT"
  )@cpts)


FPOP.lsignals.list <- list()
for(seq.i in 1:nrow(nb.size)){
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
    N <- length(seq.vec)
    sumstat <- cbind(
      c(0, cumsum(seq.vec)),
      c(0, cumsum(seq.vec^2)), 
      cumsum(c(0, (seq.vec - mean(seq.vec))^2)))
    pdpa.seconds <- system.time({
      pdpa.fit <- jointseg::Fpsn(seq.vec, length(seq.vec))
    })
    LAPPLY <- future.apply::future_lapply
    LAPPLY <- lapply
    fpop.seconds <- system.time({
    ##profvis::profvis({
      next.pen.vec <- penalty.grid
      iteration <- 0
      keep.going <- TRUE
      prev.stored <- 0
      m <- new(penmap::penmap)
      while(keep.going){
        model.info.list <- LAPPLY(next.pen.vec, function(penalty){
          plist <- changepoint:::PELT(
            sumstat, pen = penalty, cost_func = "mean.norm", 
            shape = 1, minseglen = 1)
          with(plist, list(
            penalty=penalty, loss=lastchangelike[[N]], changes=ncpts[[N]]-1))
          ## fit <- .C(
          ##   "colibri_op_R_c",
          ##   signal = as.double(seq.vec),
          ##   n = N, 
          ##   lambda = as.double(penalty),
          ##   min = as.double(min(seq.vec)),
          ##   max = as.double(max(seq.vec)), 
          ##   path = integer(N),
          ##   cost = double(N),
          ##   PACKAGE = "fpop")
          ## fpop::Fpop(seq.vec, penalty)
          ##.libPaths("library")
          ## fit <- fpop::Fpop(seq.vec, penalty)
          ## with(fit, data.table(penalty, cost=J.est, changes=K-1))
          ## end.vec <- if(penalty < Inf){
          ##   fpop::Fpop(seq.vec, penalty)[["t.est"]]
          ##   changepoint::cpt.mean(
          ##     seq.vec, penalty="Manual",
          ##     pen.value=penalty, method="PELT"
          ##   )@cpts
          ## }else{
          ##   length(seq.vec)
          ## }
          ## cost <- get.error(end.vec)
          ##data.table(penalty, changes=length(end.vec)-1, cost)
        })
        for(model.info in model.info.list){
          with(model.info, m$insert(penalty, loss, changes))
          ## tryCatch({
          ##   model.info[, m$insert(penalty, cost, changes)]
          ## }, error=function(e){
          ##   if(!grepl(e$message, "penalty already known")){
          ##     stop(e)
          ##   }
          ##   NULL
          ## })
        }
        iteration <- iteration+1
        next.pen.vec <- m$helpful()
        cat(sprintf(
          "iteration=%d stored=%d next=%d\n",
          iteration, stored <- nrow(m$df()), length(next.pen.vec)))
        if(prev.stored == stored){
          keep.going <- FALSE
        }else{
          prev.stored <- stored
        }
        if(length(next.pen.vec)==0){
          keep.going <- FALSE
        }
      }#while
    })#system.time
    pelt.seconds <- system.time({
      fit <- changepoint::cpt.mean(
        seq.vec, penalty="CROPS", method="PELT", pen.value=c(0, max.pen))
    })
    end.mat <- cbind(fit@cpts.full, length(seq.vec))
    limit.dt <- data.table(
      end=as.integer(end.mat),
      model.i=as.integer(row(end.mat))
    )[!is.na(end)]
    limit.dt[, start := c(1, end[-.N]+1), by=model.i]
    seg.err.dt <- seq.dt[limit.dt, .(
      model.i,
      seg.error=sum((logratio-mean(logratio))^2),
      N=.N
    ), on=.(data.i >= start, data.i <= end), by=.EACHI]
    model.err.dt <- seg.err.dt[, .(
      model.error=sum(seg.error),
      N=sum(N),
      changes=.N-1
    ), by=model.i]
    model.err.dt[, penalty := fit@pen.value.full]
    model.err.dt[N != length(seq.vec)]
    sel.dt <- data.table(penaltyLearning::modelSelection(
      model.err.dt, "model.error", "changes"))
    not.sel.i.vec <- model.err.dt[, which(!model.i %in% sel.dt$model.i)]
    for(not.sel.i in not.sel.i.vec){
      not.sel.one <- model.err.dt[not.sel.i]
      model.err.dt[, crit := model.error + not.sel.one$penalty * changes]
      sel.one <- model.err.dt[which.min(crit)]
      print(data.table(selected=sel.one$changes, not=not.sel.one$changes))
    }
    fpop.pen <- m$df()$penalty
    pelt.pen <- c(fit@pen.value.full, Inf)
    rbind(pelt=pelt.pen, fpop=fpop.pen)
    all.equal(pelt.pen, fpop.pen)
    seconds.mat <- rbind(pdpa.seconds, fpop.seconds, pelt.seconds)
    print(seconds.mat)
    mselect <- function(loss, complexity){
      dt <- data.table(loss, complexity)
      data.table(penaltyLearning::modelSelection(dt))
    }
    fpop.tall <- nc::capture_melt_multiple(
      m$df(), column="loss|size", "_", where="on|after")
    selected.list <- list(
      pdpa=with(pdpa.fit, mselect(J.est, seq_along(J.est)-1)),
      fpop=unique(
        fpop.tall[0 <= size, .(loss, size)]
      )[order(size), mselect(loss, size)])
    result.dt <- data.table(
      seq.size,
      algo=names(selected.list),
      selected.models=sapply(selected.list, nrow),
      elapsed.seconds=seconds.mat[, "elapsed"])
    this.result <- list(
      selected=selected.list,
      summary=result.dt)
    saveRDS(this.result, cache.rds)
  }
  FPOP.lsignals.list[[seq.i]] <- this.result
}
(summary.dt <- do.call(rbind, lapply(FPOP.lsignals.list, "[[", "summary")))

save(FPOP.lsignals, file="FPOP.lsignals.RData")
