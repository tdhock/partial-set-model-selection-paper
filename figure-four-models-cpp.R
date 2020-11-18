source("packages.R")

L3.list <- list(
  "4"=6.5,
  "3"=7.5)
pen.list <- list(
  random=c(4, 0.5, 1, 2, 3.5),
  systematic=c(0, Inf, 8/3, 3.25))
iteration.dt.list <- list()
vline.dt.list <- list()
models.selected <- "4"
penalties <- "random"

for(models.selected in names(L3.list)){
  print(models.selected)
  for(penalties in names(pen.list)){
    print(penalties)
    L3 <- L3.list[[models.selected]]
    loss.vec <- c(10, L3, 3.5, 2)
    pen.vec <- pen.list[[penalties]]
    loss.dt <- data.table(loss=loss.vec, complexity=seq_along(loss.vec)-1)
    print(loss.dt)
    selection.dt <- data.table(penaltyLearning::modelSelection(loss.dt))
    print(selection.dt[, .(min.lambda, max.lambda, loss, complexity)])
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
      sel.dt[]
    }
    status <- "optimal"
    vline.dt.list[[paste(models.selected, penalties)]] <- data.table(
      models.selected, penalties, L3,
      penalty=pen.vec,
      status,
      iteration=seq_along(pen.vec))
    m = new(penmap::penmap)
    for(iteration in seq_along(pen.vec)){
      it.pen <- pen.vec[[iteration]]
      it.selection <- select.at(it.pen)
      print(it.selection)
      it.selection[, m$insert(penalty, loss, complexity)]
      print(m$df())
      print(m$helpful())
      iteration.dt.list[[
        paste(models.selected, penalties, iteration)
        ]] <- data.table(
          L3, status,
          models.selected, penalties, iteration, m$df())
    }
  }
}
iteration.dt <- do.call(rbind, iteration.dt.list)
vline.dt <- do.call(rbind, vline.dt.list)

opt.color <- "red"
by.vec = c("models.selected", "L3", "iteration")
for(p in names(pen.list)){
  pen.iterations <- iteration.dt[penalties==p]
  pen.vlines <- vline.dt[penalties==p]
  pen.ablines = pen.iterations[
  , unique(rbind(
    data.table(loss=loss_on, size=size_on),
    data.table(loss=loss_after, size=size_after))),
    by=by.vec][0 <= size]
  pen.iterations[, next_pen := c(penalty[-1], NA), by=by.vec]
  opt.points <- data.table(pen.iterations, type="end")
  opt.points[
    size_on == -1,
    `:=`(loss_on=loss_after, size_on=size_after, type="break")]
  help.points <- data.table(pen.iterations, type="helpful")[size_after == -3]
  help.points[, penalty := loss_after]
  pen.points <- rbind(opt.points, help.points)
  pen.points[, cost := cost(loss_on, size_on, penalty)]
  pen.points[loss_on==Inf, cost := min(pen.points$cost, na.rm=TRUE)-1]
  (gg <- ggplot()+
     ggtitle(paste(p, "penalty selection"))+
     facet_grid(models.selected + L3 ~ iteration, labeller=label_both)+
     scale_size_manual(values=c(optimal=2))+
     coord_equal(xlim=c(0, 4.5))+
     xlab("penalty")+
     ylab("loss + penalty*model_size")+
     geom_vline(aes(
       xintercept=penalty, color=status),
       size=1,
       data=pen.vlines)+
     geom_segment(aes(
       penalty, cost(loss_after, size_after, penalty),
       color=status, size=status,
       xend=next_pen, yend=cost(loss_after, size_after, next_pen)),
       data=pen.iterations[0 <= size_after])+
     geom_abline(aes(
       slope=size,
       intercept=loss),
       data=pen.ablines)+
     geom_point(aes(
       penalty, cost,
       fill=type),
       shape=21,
       color="black",
       data=pen.points)+
     scale_color_manual(values=c(optimal=opt.color))+
     scale_fill_manual(values=c(
       "break"="black",
       end=opt.color,
       helpful="white"))
  )
  png(
    sprintf("figure-four-models-cpp-%s.png", p),
    width=10, height=6, units="in", res=100)
  print(gg)
  dev.off()
}


