library(dplyr)
library(ggplot2)
library(rstanarm)
library(daatools)

df = read.csv("wohnungspreise.csv")

info(df) # - funktion daatools (neg0 - wieviele sind 0 oder negativ)
hist(df)

plot(df$wohnflaeche, df$preis) # eindeutiger zusammenhang

boxplot(preis~ zimmer, data=df) #je mehr zimmer desto teurer

m1 = stan_glm(preis~wohnflaeche, data=df)
m1
#intercept eine wohnung mit 0 qm kostet 92k€

df$wohnflaeche95 = df$wohnflaeche - 95 # sobald eine wohnfläche 95qm kostet es 
m2 = stan_glm(preis~wohnflaeche95, data=df)
m2
# eine wohnung mit 95qm kostest 38k€ weil der 0Punkt bei 95 ist (alle mit 95qm sind 0)
# für jeden qm mehr muss man 3k€ zahlen

plothist(m2, "wohnflaeche95") #qm preis, zwischen 

info(m2)
check_sampling(m2) # -daatools muss man noch installierne (neue version)


diagnose_density(m2)
diagnose_fit(m2)

diagnose_y_vs_yhat(m2)
diagnose_residual(m2)


m3 = stan_glm(preis~zimmer, data=df)
m3 # pro zimmer kostet die wohnung 30k€ mehr
# wohnung mit 0 zimmer kostet 245k€

info(df) # durchschnittliche zimmer anzahl 4,5

df$zimmer4 = df$zimmer -4
m4 = stan_glm(preis~zimmer4, data=df)
m4 #basis preis für 4 zimmer 368k€, jedes weitere 30k€

m5 = stan_glm(preis ~ wohnflaeche95 + zimmer4, data=df)
m5 # eine wohnung mit 95qm und 4 zimmer kostet ~400k€
# - jeder qm mehr kostet 3k€ 
# - jedes zimmer mehr macht die wohnung billiger (die zimmer werden kleiner) - zwei wohnungen mit der gleichen grundfläche aber unterschiedlicher zimmer anzhal 55qm mit 3 oder 6 zimmer

boxplot(preis ~ zimmer, data=filter(df, wohnflaeche >= 95, wohnflaeche <105)) # wir schauen usn jetzt den datensatz nur mit den wohnungsfläche von 95qm bis 105qm


