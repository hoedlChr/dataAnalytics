source("patel-utils.R")

daily = get_daily_data()

hist(daily$streams_m) # in den jahren 2017 bis 22 gab es 820 tage an denen 70 - 80 millionen streams waren

hist(daily$streams_m, breaks=30)

hist(daily$fatalities)
mean(daily$fatalities)
median(daily$fatalities)


plot(daily$streams_m, daily$fatalities) #wie hängen die streams mit den toten zusammen



#eigener datensatz
df = data.frame(x = rnorm(100))
df$y = 2*df$x

plot(df$x,df$y) # extrem starker zusammenhang, y ist da doppelte von x


#eigener datensatz
df2 = data.frame(x = rnorm(100))
df2$y = 2*df2$x + rnorm(100,0,3) # rnorm gibt random zahlen von 0 bis 100 dazu und abweichung 1 (3)

plot(df2$x,df2$y) # verrauscht kein so großer zusammenhang
abline(a=0, b=2, col="blue")
cor(df2$x, df2$y) # correlation = -1 bis 1 = erklärt die eine variable die andere = 0 ... kein zusammenhang, 1 ... liegen "übereinandner", -1... eines steigt eines fällt
# eigner datensatz aus

cor(daily$fatalities, daily$streams_m) # 0.04 ... keine Correlation

boxplot(fatalities ~ dow, data=daily) # option + n = ~ ; dow ... day of week, 0 ... sonntag, 1 ... Montag # Totesfälle sind am Wochenende am größten; 

boxplot(fatalities ~ week, data = daily) # im sommer passiert mehr als im winter

summary(daily)

# Regressionsmodel
library(rstanarm)

m1 = stan_glm(fatalities ~ dow, data=daily)
m1 # zeigt zb vom ref tag (sonntag ... 0) wie viele leute im median gestorben sind 

table(daily$dow) 


m2 = stan_glm(fatalities ~ dow + streams_m, data=daily) # wie viele todesfälle sind am tag der woche und der streams
print(m2, digits=4) #digits 4 gibt mehr komma stellen aus

# intercept ist der basis fall (bei uns sonntag), uns interessiert nur der median, am sonntag mit 100 Mil. streams sind 116 am montag mit 100 mil. streams 96

# in streams bleibt übrig was sich nicht der wochentage enspricht - sprich die wochentage sind "immer" gleich und grobe veränderungen werden streams zugeschrieben

# Erster Ansatz m1 - todesfälle hängen von wochentage ab (Sontag ist verantwortlich)
# Zweiter Ansatz m2 - todesfälle pro 100 millionen streams sterben 5 leute weniger
# über dow sagt man am we sind mehr tote, mit den streams sagt man je mehr streams es sind desto weniger tote (weil die zahl negativ ist)

mean(bayes_R2(m2)) # sagt wie viel von der varianz wird durch das model erklärt (33 Prozent werden durch 2 Variablen erklärt, 70 prozent kommt von irgendwo anders her)
# ^ wie gut erklärt unser model die realität; wenn wir 100 variablen hernehmen können wir nicht 100% erklären
# ab 50% oder 60% wirds interessant 


m3 = stan_glm(fatalities ~ dow + streams_m + holiday + year, data=daily)
print(m3, digits = 4)
# weil das jahr eine gruppe ist sieht man mehrere jahre, referenz (intercept): 100 Millionen Streams, Sonntag, im Jahr 2017 
# Intercept: 110
# Montag mit <100Mil. streams im jahr 2018 wäre: 110.3 - 21.7 - 2.2
  
  
  
plot(daily$date, daily$fatalities) 
#^wie schauts covid aus - hatte das auswirkungne

m4 = stan_glm(fatalities ~ dow + new_streams_m + old_streams_m + holiday + year, data=daily)
print(m4, digits= 4)
# zweite spalte (MAD_SD) sagt wie die wahrscheinlichkeit verteilt ist

library(daatools)

plotcoef(m4, "beta")
plothist(m4, "holiday")
plothist(m4, "new_streams_m", transform=function(x) x*100) # model im median (die mittlereschätzung) ist bei 17.6 bei 100 Millionen Streams, schätzung liegt bei 3.76 und 30.5 (um 95% intervall)
plothist(m4, "old_streams_m", transform=function(x) x*100) # sind nicht aussage kräftig (es geht von - 8 bis 7), wir können sagen, dass die daten konsistent mit keinem effekt, es wird nicht ausgeschlossen das es keinen effekt gibt


mean(bayes_R2(m4)) # das model erklärt 44% der todesfälle
