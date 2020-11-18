source("packages.R")

future::plan("multiprocess")

data(neuroblastoma, package="neuroblastoma")
nb.dt <- data.table(neuroblastoma[["profiles"]])
data.table::fread("../LOPART-paper/data-for-LOPART-signals.csv.gz")
setkey(nb.dt, profile.id, chromosome, position)
nb.size <- nb.dt[, .(N.data=.N), by=.(profile.id, chromosome)][order(N.data)]

dir.create("neuroblastoma")

penalty.grid <- c(0, exp(seq(-5, 7, l=10)), Inf)

FPOP.neuroblastoma.list <- list()
for(pid.chr.i in 1:nrow(nb.size)){
  pid.chr.size <- nb.size[pid.chr.i]
  pid_chr <- pid.chr.size[, paste0(profile.id, "_", chromosome)]
  cache.rds <- paste0("neuroblastoma/", pid_chr, ".rds")
  if(file.exists(cache.rds)){
    this.result <- readRDS(cache.rds)
  }else{
    cat(sprintf("%4d / %4d %s\n", pid.chr.i, nrow(nb.size), pid_chr))
    pid.chr.dt <- nb.dt[pid.chr.size, on=.(profile.id, chromosome)]
    pid.chr.vec <- pid.chr.dt[["logratio"]]
    pdpa.time <- system.time({
      pdpa.fit <- jointseg::Fpsn(pid.chr.vec, length(pid.chr.vec))
    })
    fpop.time <- system.time({
      next.pen.vec <- penalty.grid
      iteration <- 0
      keep.going <- TRUE
      prev.stored <- 0
      m <- new(penmap::penmap)
      while(keep.going){
        pen.dt.list <- future.apply::future_lapply(next.pen.vec, function(penalty){
          .libPaths("library")
          fit.list <- if(penalty < Inf){
            fpop::Fpop(pid.chr.vec, penalty)
          }else{
            list(K=1, J.est=sum((pid.chr.vec-mean(pid.chr.vec))^2))
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
    print(rbind(pdpa.time, fpop.time))
    saveRDS(this.result, cache.rds)
  }
  FPOP.neuroblastoma.list[[pid.chr]] <- this.result
}
FPOP.neuroblastoma <- do.call(rbind, FPOP.neuroblastoma.list)

save(FPOP.neuroblastoma, penalty.grid, file="FPOP.neuroblastoma.RData")
