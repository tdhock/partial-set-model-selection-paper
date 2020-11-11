source("packages.R")

L3.list <- list(
  "4"=6.5,
  "3"=7.5)
pen.list <- list(
  random=c(4, 0.5, 1, 2, 3.5),
  systematic=c(0, Inf, 8/3, 3.25))
iteration.dt.list <- list()
vline.dt.list <- list()
for(models.selected in names(L3.list)){
  for(penalties in names(pen.list)){
    L3 <- L3.list[[models.selected]]
    loss.vec <- c(10, L3, 3.5, 2)
    pen.vec <- pen.list[[penalties]]
    loss.dt <- data.table(loss=loss.vec, complexity=seq_along(loss.vec)-1)
    selection.dt <- data.table(penaltyLearning::modelSelection(loss.dt))
    cost <- function(loss, complexity, penalty){
      pen.comp <- ifelse(complexity==0, 0, complexity*penalty)
      loss + pen.comp
    }
    select.at <- function(pen.unsort){
      pen.dt <- data.table(penalty=sort(pen.unsort))
      sel.dt <- selection.dt[
        pen.dt, .(penalty, loss, complexity),
        mult="first",
        on=.(min.lambda <= penalty, max.lambda >= penalty)]
      sel.dt[, next.complexity := c(complexity[-1], -Inf)]
      sel.dt[, next.pen := c(penalty[-1], -Inf)]
      sel.dt[, next.loss := c(loss[-1], Inf)]
      sel.dt[, b := ifelse(
        is.finite(next.loss),
        (next.loss-loss)/(complexity-next.complexity),
        -Inf)]
      sel.dt[, breakpoint := ifelse(
        complexity-1==next.complexity | b==penalty, b, NA_real_)]
      sel.dt[, pen.max := fcase(
        complexity==0, Inf,
        complexity==next.complexity, next.pen,
        is.finite(breakpoint), breakpoint,
        default=NA_real_)]
      sel.dt[]
    }
    status <- "optimal"
    vline.dt.list[[paste(models.selected, penalties)]] <- data.table(
      models.selected, penalties, L3,
      penalty=pen.vec,
      status,
      iteration=seq_along(pen.vec))
    for(iteration in seq_along(pen.vec)){
      it.pen.vec <- pen.vec[1:iteration]
      it.selection <- select.at(it.pen.vec)
      iteration.dt.list[[
        paste(models.selected, penalties, iteration)
        ]] <- data.table(
          L3,
        models.selected, penalties, iteration, it.selection)
    }
  }
}
iteration.dt <- do.call(rbind, iteration.dt.list)
vline.dt <- do.call(rbind, vline.dt.list)

for(p in names(pen.list)){
  pen.iterations <- iteration.dt[penalties==p]
  pen.vlines <- vline.dt[penalties==p]
  (gg <- ggplot()+
     ggtitle(paste(p, "penalty selection"))+
     facet_grid(models.selected + L3 ~ iteration, labeller=label_both)+
     scale_size_manual(values=c(optimal=2))+
     coord_equal(xlim=c(0, 4.5))+
     xlab("penalty")+
     ylab("loss + penalty*complexity")+
     geom_vline(aes(
       xintercept=penalty, color=status),
       size=1,
       data=pen.vlines)+
     geom_segment(aes(
       penalty, cost(loss, complexity, penalty), 
       color=status, size=status,
       xend=pen.max, yend=cost(loss, complexity, pen.max)),
       data=pen.iterations[!is.na(pen.max)])+
     ## after breakpoint.
     geom_segment(aes(
       breakpoint, cost(loss, complexity, breakpoint), 
       color=status, size=status,
       xend=next.pen, yend=cost(next.loss, next.complexity, next.pen)),
       data=pen.iterations[is.finite(breakpoint)])+
     geom_point(aes(
       penalty, cost(loss, complexity, penalty),
       fill=status, size=status),
       shape=21,
       color="black",
       data=pen.iterations)+
     geom_abline(aes(
       slope=complexity,
       intercept=loss),
       data=unique(pen.iterations))
  )
  png(
    sprintf("figure-four-models-%s.png", p),
    width=10, height=6, units="in", res=100)
  print(gg)
  dev.off()
}


