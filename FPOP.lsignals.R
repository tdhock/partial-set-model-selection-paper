source("packages.R")

future::plan("multiprocess")

nb.dt <- data.table::fread("../LOPART-paper/data-for-LOPART-signals.csv.gz")
setkey(nb.dt, sequenceID)
nb.size <- nb.dt[, .(N.data=.N), by=.(sequenceID)][order(N.data)]

dir.create("lsignals")

penalty.grid <- c(0, exp(seq(-5, 7, l=10)), Inf)

seq.i <- 390
FPOP.lsignals.list <- list()
for(seq.i in 1:nrow(nb.size)){
  seq.size <- nb.size[seq.i]
  seqID <- seq.size[["sequenceID"]]
  cache.rds <- paste0("lsignals/", seqID, ".rds")
  if(file.exists(cache.rds)){
    this.result <- readRDS(cache.rds)
  }else{
    cat(sprintf("%4d / %4d %s size=%d\n", seq.i, nrow(nb.size), seqID, seq.size[["N.data"]]))
    seq.dt <- nb.dt[seq.size, on=.(sequenceID)]
    seq.vec <- seq.dt[["logratio"]]
    pdpa.seconds <- system.time({
      pdpa.fit <- jointseg::Fpsn(seq.vec, length(seq.vec))
    })
    fpop.seconds <- system.time({
      next.pen.vec <- penalty.grid
      iteration <- 0
      keep.going <- TRUE
      prev.stored <- 0
      m <- new(penmap::penmap)
      while(keep.going){
        pen.dt.list <- future.apply::future_lapply(next.pen.vec, function(penalty){
          .libPaths("library")
          fit.list <- if(penalty < Inf){
            fpop::Fpop(seq.vec, penalty)
          }else{
            list(K=1, J.est=sum((seq.vec-mean(seq.vec))^2))
          }
          with(fit.list, data.table(penalty, changes=K-1, cost=J.est))
        })
        for(model.info in pen.dt.list){
          tryCatch({
            model.info[, m$insert(penalty, cost, changes)]
          }, error=function(e){
            NULL
          })
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
    seconds.mat <- rbind(pdpa.seconds, fpop.seconds)
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
