Teebeutel <- read.csv2("C:/Users/Alexandra/Desktop/praktikum_2018/R/Teebeutel_R.csv")
Teebeutel
str(Teebeutel)

boxplot.Red <- boxplot(Teebeutel$Wt_Red ~ Teebeutel$Treatment)
boxplot.Green <- boxplot(Teebeutel$Wt_Green ~ Teebeutel$Treatment)

#### Auf signifikante Unterschiede Testen ####

qqnorm(Teebeutel$Wt_Red)
shapiro.test(Teebeutel$Wt_Red) ## Normalverteilt

qqnorm(Teebeutel$Wt_Green)
shapiro.test(Teebeutel$Wt_Green) ## Keine Normalverteilung, liegt nur an dem Ausreißer in Sample 7, nochmal überprüfen! Sonst vllt Messfehler der rausgenommen werden muss

var.test(Teebeutel$Wt_Red, Teebeutel$Wt_Green) ## Muss man nur machen wenn Beide Normalverteilt sind

wilcox.test(Teebeutel$Wt_Red, Teebeutel$Wt_Green, exact = F) ## p-value < 0.05 (signifikanzniveau) daher signifikanter Unterschied

## Wenn Normalverteilung doch gegeben sein sollte

t.test(Teebeutel$Wt_Red, Teebeutel$Wt_Green, var.equal = TRUE) ## var.equal = TRUE wenn var.test (Zeile 15 p > 0.05)

##Ausgangsgewicht vergleichen von Rot und Grün über die Treatments:

boxplot.Red <- boxplot(Teebeutel$Initial_W_Green ~ Teebeutel$Treatment)
boxplot.Green <- boxplot(Teebeutel$Initial_W_Red ~ Teebeutel$Treatment)


#nicht mehr über tratments:

boxplot.Red <- boxplot(Teebeutel$Initial_W_Green)
boxplot.Green <- boxplot(Teebeutel$Initial_W_Red)


library(tidyverse)

Teebeutel.boxplot <- Teebeutel %>% 
  gather("key", "value", 3:4)

boxplot(Teebeutel.boxplot$value ~ Teebeutel.boxplot$key)


