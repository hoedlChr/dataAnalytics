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

# in streams bleibt übrig was sich nicht der wochentage enspricht - sprich die wochentage sind "immer" gleich und grobe veränderungen werden streams zugeschrieben

# Erster Ansatz m1 - todesfälle hängen von wochentage ab (Sontag ist verantwortlich)
# Zweiter Ansatz m2 - todesfälle pro 100 millionen streams sterben 5 leute weniger

mean(bayes_R2(m2)) # sagt wie viel von der varianz wird durch das model erklärt (33 Prozent werden durch 2 Variablen erklärt, 70 pro)

  
  
  
  
  
  
  
  
  
  
  
  
  
  