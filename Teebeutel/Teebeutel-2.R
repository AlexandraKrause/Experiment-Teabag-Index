#### Pakete Laden ####
library(HH)
library(tidyverse)
library(cowplot)
library(corrplot)
library(car)
library(scales)
library(boot)
library(nlme)


#f?r 8 cm:

## Vorher ein R-Projekt erstellen; dann kann man relative Dateipfade angeben über "./" 
## und dann mit shift die gewünschte Datei auswählen

Teebeutel <- read.csv2("./Dateneingabe/Teebeutel_R_8cm.csv")
Teebeutel

#oder f?r 20 cm:

Teebeutel<-read.csv2("./Dateneingabe/Teebeutel_R_20cm.csv")
Teebeutel

#oder f?r eine Testdatei:

Teebeutel<-read.csv2("./Dateneingabe/Teebeutel_sortiert.csv")
Teebeutel


#?r Teebeutel ausgegraben:

Teebeutel<-read.csv2("C:/Users/Alexandra/Desktop/praktikum_2018/R/Teebeutel_ausgegraben_csv.csv")
Teebeutel



#Anfang der Berechnungen, getrennt f?r die jeweiligen oben aufgef?hrten und eingelesenen Excel-Dateien:


str(Teebeutel)
boxplot.Red<- boxplot(ylab="weight", xlab="treatments",main="weight of red tea in different treatments",Teebeutel$Wt_Red ~ Teebeutel$Treatment)
boxplot.Green <- boxplot(ylab="weight", xlab="treatments",main="weight of green tea in different treatments",Teebeutel$Wt_Green ~ Teebeutel$Treatment)


#barplot und scatterplot

#barplot einfach:

install.packages("ggplot2")
library(ggplot2)

p<-ggplot(data=Teebeutel, aes(x=Treatment, y=Wt_Red)) +
  geom_bar(stat="identity")
p


#barplot mit mittelwert,median und beschriftung:
#1.mit einfacher Formel f?r:
#1a: f?r roten tee:

Teebeutel.mean.bar <- Teebeutel %>% 
  group_by(Treatment) %>% 
  summarise(Teebeutel.mean = mean(Teebeutel$Wt_Red))
      


barplot.1 <- Teebeutel.mean.bar %>% 
  ggplot(aes(x = Treatment,
             y = Teebeutel$Wt_Red,
             fill = Treatment)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(y = "Gewicht Teebeutel [g]",
       title = "Gewicht der Teebeutel in Zusammenhang mit der jewiligen D?ngung") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(Teebeutel.mean.bar$Teebeutel.mean), color = "Red") +
  geom_hline(yintercept = median(Teebeutel.mean.bar$Teebeutel.mean), color = "Blue") +
  geom_text(aes(x = "Ambient" , y = mean(Teebeutel.mean.bar$Teebeutel.mean) + 60, label = "Mittelwert")) +
  geom_text(aes(x = "Ambient" , y = median(Teebeutel.mean.bar$Teebeutel.mean) - 60, label = "Median"))

grid.newpage()
grid.draw(rbind(ggplotGrob(barplot.1), size = "last"))


#1b: f?r gr?nen tee



Teebeutel.mean.bar <- Teebeutel %>% 
  group_by(Treatment) %>% 
  summarise(Teebeutel.mean = mean(Teebeutel$Wt_Green))



barplot.1 <- Teebeutel.mean.bar %>% 
  ggplot(aes(x = Treatment,
             y = Teebeutel$Wt_Green,
             fill = Treatment)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(y = "Gewicht Teebeutel [g]",
       title = "Gewicht der Teebeutel in Zusammenhang mit der jewiligen D?ngung") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(Teebeutel.mean.bar$Teebeutel.mean), color = "Red") +
  geom_hline(yintercept = median(Teebeutel.mean.bar$Teebeutel.mean), color = "Blue") +
  geom_text(aes(x = "Ambient" , y = mean(Teebeutel.mean.bar$Teebeutel.mean) + 60, label = "Mittelwert")) +
  geom_text(aes(x = "Ambient" , y = median(Teebeutel.mean.bar$Teebeutel.mean) - 60, label = "Median"))

grid.newpage()
grid.draw(rbind(ggplotGrob(barplot.1), size = "last"))

#folgendes nicht tun:
#install.packages("car")
#install.packages("carData")
#library(carData)
#scatterplot<-plot(Teebeutel$Wt_Red ~ Teebeutel$Treatment)

#folgendes tun:
#scatterplot
plot(Teebeutel$Wt_Red,Teebeutel$Treatment)#mit tilde satt komma wird es ein boxplot, da treatment als faktor definiert wird. so ist es richtig und wird ein scatterplot





#tests mit der Teebeutelfarbe rot und gr?n (nur 2 Variablen):


#### Wt einfache Formel: Auf signifikante Unterschiede Testen ######

qqnorm(Teebeutel$Wt_Red)
shapiro.test(Teebeutel$Wt_Red) ## Normalverteilt

qqnorm(Teebeutel$Wt_Green)
shapiro.test(Teebeutel$Wt_Green) ## Keine Normalverteilung, liegt nur an dem Ausrei?er in Sample 7, nochmal ?berpr?fen! Sonst vllt Messfehler der rausgenommen werden muss

var.test(Teebeutel$Wt_Red, Teebeutel$Wt_Green) ## Muss man nur machen wenn Beide Normalverteilt sind

wilcox.test(Teebeutel$Wt_Red, Teebeutel$Wt_Green, exact = F) ## p-value < 0.05 (signifikanzniveau) daher signifikanter Unterschied

## Wenn Normalverteilung doch gegeben sein sollte

t.test(Teebeutel$Wt_Red, Teebeutel$Wt_Green, var.equal = TRUE) ## var.equal = TRUE wenn var.test (Zeile 15 p > 0.05)

####Ausgangsgewicht vergleichen von Rot und Gr?n ?ber die Treatments:########

boxplot.Red <- boxplot(ylab="weight", xlab="treatments",main=" initial weight of red tea in different treatments", Teebeutel$Initial_W_Green ~ Teebeutel$Treatment)
boxplot.Green <- boxplot(ylab="weight", xlab="treatments",main=" initial weight of green tea in different treatments",Teebeutel$Initial_W_Red ~ Teebeutel$Treatment)


#nicht mehr ?ber treatments:

boxplot.Red <- boxplot(Teebeutel$Initial_W_Green)
boxplot.Green <- boxplot(Teebeutel$Initial_W_Red)

#f?r _R:
library(tidyverse)

Teebeutel.boxplot <- Teebeutel %>% 
  gather("key", "value", 3:4)

boxplot(ylab="weight",xlab="type of tea", main="initial weight of red and green tea",Teebeutel.boxplot$value ~ Teebeutel.boxplot$key)


#####f?r_R_20cm und 8 cmm: Vergleich initial weight of red and green tea:#######

library(tidyverse)

Teebeutel.boxplot <- Teebeutel %>% 
  gather("key", "value", 12:13)

boxplot(ylab="weight",xlab="type of tea", main="initial weight of red and green tea",Teebeutel.boxplot$value ~ Teebeutel.boxplot$key)


#n?chster Punkt:
#signifikanzanalyse
#unterschiede im gewicht bei den verschiedenen treatments nachweisen! 
#test finden, der alle!! treatments ber?cksichtigt
#post hoc!
