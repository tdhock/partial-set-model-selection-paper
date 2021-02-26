source("packages.R")

PELT.lsignals.list <- readRDS("PELT.lsignals.rds")

dt.list <- list()
for(data.type in names(PELT.lsignals.list[[1]])){
  dt.list[[data.type]] <- do.call(rbind, lapply(
    PELT.lsignals.list, "[[", data.type))
}

models.wide <- dcast(
  dt.list[["models"]],
  sequenceID + N.data ~ models + method,
  value.var="count")

models.wide[all_penmap != selected_penmap]
models.wide[all_crops == selected_crops]

## We expect that all methods should produce the same number of
## selecte models, but here we see that sometimes penmap gets more,
## sometimes less, why?
models.wide[selected_pdpa < selected_penmap]
models.wide[selected_pdpa > selected_penmap]
gg <- ggplot()+
  ggtitle("penmap mostly consistent with pdpa")+
  geom_abline()+
  geom_point(aes(
    selected_pdpa, selected_penmap,
    color=factor(sign(selected_penmap-selected_pdpa))),
    data=models.wide)+
  scale_color_discrete("sign")+
  coord_equal()
png(
  "figure-PELT-lsignals-selected-penmap-pdpa.png",
  width=5, height=5, units="in", res=200)
print(gg)
dev.off()

## here we see that crops always selects fewer models than penmap.
models.wide[selected_penmap < selected_crops]
models.wide[selected_penmap > selected_crops]
gg <- ggplot()+
  geom_abline()+
  geom_point(aes(
    selected_crops, selected_penmap),
    data=models.wide)+
  coord_equal()
png(
  "figure-PELT-lsignals-selected-penmap-crops.png",
  width=5, height=5, units="in", res=200)
print(gg)
dev.off()

## here we see that crops always selects fewer models than pdpa.
models.wide[selected_pdpa < selected_crops]
models.wide[selected_pdpa > selected_crops]
gg <- ggplot()+
  geom_abline()+
  geom_point(aes(
    selected_crops, selected_pdpa),
    data=models.wide)+
  coord_equal()
png(
  "figure-PELT-lsignals-selected-pdpa-crops.png",
  width=5, height=5, units="in", res=200)
print(gg)
dev.off()

## sometimes number of selected models less than all for crops,
## because crops result can store the same model multiple times, if it
## was compued for more than one penalty value.
models.wide[all_crops > selected_crops]
models.wide[all_crops < selected_crops]

(sec.df <- dt.list[["seconds"]])
gg <- ggplot(sec.df)+
  geom_line(aes(
    N.data, median, color=method))+
  geom_ribbon(aes(
    N.data, ymin=q25, ymax=q75, fill=method),
    alpha=0.5)
png(
  "figure-PELT-lsignals.png",
  width=5, height=5, units="in", res=200)
print(gg)
dev.off()
