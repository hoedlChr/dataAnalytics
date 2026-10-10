library(dplyr)
library(ggplot2)
library(rstanarm)
library(daatools)

# https://data.graz.gv.at/graz/feuerwehreinsaetze-in-graz-3/
# https://data.graz.gv.at/graz/die-grazer-bevoelkerung-nach-bezirk-und-staatsangehoerigkeit/

bezirke = read.csv('graz-bezirksflaeche.csv')
einsaetze = read.csv("Graz-Einsaetze.csv")
df = left_join(einsaetze, bezirke, by="bezirk") |>
  mutate(
    bevoelkerung1000 = bevoelkerung / 1000,
    aut1000 = aut / 1000,
    eu1000 = eu /1000,
    andere1000 = andere / 1000,
    jahr2010 = jahr - 2010,
    bezirk = factor(bezirk),
    dichte = bevoelkerung1000 / flaeche) |>
  group_by(bezirk) |>
  mutate(
    einsaetze_mean = mean(einsaetze),
    bevoelkerung1000_mean = mean(bevoelkerung1000),
    aut1000_mean = mean(aut1000),
    eu1000_mean = mean(eu1000),
    andere1000_mean = mean(andere1000),
    bevoelkerung1000_within = bevoelkerung1000 - bevoelkerung1000_mean,
    aut1000_within = aut1000 - aut1000_mean,
    eu1000_within = eu1000 - eu1000_mean,
    andere1000_within = andere1000 - andere1000_mean
  ) |>
  ungroup()

hist(df$einsaetze)
hist(df$bevoelkerung)
table(df$jahr)

m1 = stan_glm(einsaetze ~ bevoelkerung1000, data=df)
m1
# in einem bezirk mit 0 einwohnern hat die ff im jahr 17 einsätze
# pro 1000 einwohner kommen 19 einsätze dazu

diagnose_density(m1) #dunkel blau verteilung der daten, hellblau wie unser model es beschreibt - keine gute beschreibung
mean(bayes_R2(m1))# in der verteilung ist unser modell nicht gut

m2 = stan_glm(einsaetze ~ bevoelkerung1000 + eu1000 + andere1000, data=df) #man darf nicht alle 4 aufnehmen, darum wird österreich nicht angführt
m2
#es kommen 10 einsätze dazu wenn, 1000 Österreicher dazu kommen, weil die anderen zwei 0 bleiben müssen, weil bev = eu + andere + aut - sprich wenn die anderen gleich bleiben und 1000 österreicher kommen sind 10 einsätze mehr

plotcoef(m2, "1000") # schwarze punkt ist das median von m2

#wir haben eine korrelation aber keine kausalität


m3 = stan_glm(einsaetze ~ bevoelkerung1000 + eu1000 +andere1000+bezirk+jahr2010, data=df)
m3
# intercept im ersten bezirk bei 0 Eiwohnern, im Jahr 2010

plothist(m3, "jahr2010")#es ist nicht eindeutig (da der strich von unter 0 bis über null gehen ob es über die jahre abnimmt)
plotcoef(m3, "bezirk")

plotcoef(m3,"1000")
plothist(m3, "andere1000", compVal=0) #das modell sagt, das der effekt auch negativ sein kann; die beste schätzung ist wo es am häufigsten es, es ist schöner wenn alles entweder unter oder über den punkt ist
