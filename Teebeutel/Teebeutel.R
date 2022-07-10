#### Pakete Laden ####
library(HH)
library(tidyverse)
library(cowplot)
library(corrplot)
library(car)
library(scales)
library(boot)
library(nlme)


## Auswählen welche Datei man darstellen möchte, nur eine je Durchgang aktivieren, deswegen 20cm gerade "ausgeschaltet" da aur Formel nur auf 8cm funktioniert
Teebeutel <- read.csv2("./Datenausgabe/Teebeutel_R_8cm_bearbeitet.csv")
# Teebeutel <- read.csv2("./Datenausgabe/Teebeutel_R_20cm_bearbeitet.csv")


# Anfang der Berechnungen, getrennt f?r die jeweiligen oben aufgef?hrten und eingelesenen Excel-Dateien:


boxplot.Red<- boxplot(ylab="weight", xlab="treatments",main="weight of red tea in different treatments",Teebeutel$Wt_Red ~ Teebeutel$Treatment)
boxplot.Green <- boxplot(ylab="weight", xlab="treatments",main="weight of green tea in different treatments",Teebeutel$Wt_Green ~ Teebeutel$Treatment)


#barplot und scatterplot

#barplot mit mittelwert,median und beschriftung:
#1.mit einfacher Formel f?r:
#1a: f?r roten tee:

## Roter Tee
Teebeutel.mean.bar.red <- Teebeutel %>% 
  group_by(Treatment) %>% 
  summarise(Teebeutel.mean = mean(Wt_Red))
      
barplot.1 <- Teebeutel.mean.bar.red %>% 
  ggplot(aes(x = Treatment,
             y = Teebeutel.mean,
             fill = Treatment)) +
  geom_bar(stat = "identity") +
  labs(y = "Wt1 roter Tee",
       title = "Wt1 der Teebeutel in Zusammenhang mit der jewiligen D?ngung") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(Teebeutel.mean.bar.red$Teebeutel.mean), color = "Red") +
  geom_hline(yintercept = median(Teebeutel.mean.bar.red$Teebeutel.mean), color = "Blue") +
  geom_text(aes(x = "Ambient" , y = mean(Teebeutel.mean.bar.red$Teebeutel.mean) + 0.03, label = "Mittelwert")) +
  geom_text(aes(x = "Ambient" , y = median(Teebeutel.mean.bar.red$Teebeutel.mean)- 0.03, label = "Median"))

grid.newpage()
grid.draw(rbind(ggplotGrob(barplot.1), size = "last"))

ggsave(paste0("./Datenausgabe/Barplot_Wt1_Red.pdf"),
       width = 30,
       height = 18,
       units = "cm",
       dpi = 300)

## Grüner Tee

Teebeutel.mean.bar.green <- Teebeutel %>% 
  group_by(Treatment) %>% 
  summarise(Teebeutel.mean = mean(Wt_Green))

barplot.1 <- Teebeutel.mean.bar.green %>% 
  ggplot(aes(x = Treatment,
             y = Teebeutel.mean,
             fill = Treatment)) +
  geom_bar(stat = "identity") +
  labs(y = "Wt1 gruener Tee",
       title = "Wt1 der Teebeutel in Zusammenhang mit der jeweiligen Duengung") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(Teebeutel.mean.bar.green$Teebeutel.mean), color = "Red") +
  geom_hline(yintercept = median(Teebeutel.mean.bar.green$Teebeutel.mean), color = "Blue") +
  geom_text(aes(x = "Ambient" , y = mean(Teebeutel.mean.bar.green$Teebeutel.mean) + 0.03, label = "Mittelwert")) +
  geom_text(aes(x = "Ambient" , y = median(Teebeutel.mean.bar.green$Teebeutel.mean)- 0.03, label = "Median"))

grid.newpage()
grid.draw(rbind(ggplotGrob(barplot.1), size = "last"))

ggsave(paste0("./Datenausgabe/Barplot_Wt1_Green.pdf"),
       width = 30,
       height = 18,
       units = "cm",
       dpi = 300)


#folgendes nicht tun:
#install.packages("car")
#install.packages("carData")
#library(carData)
#scatterplot<-plot(Teebeutel$Wt_Red ~ Teebeutel$Treatment)

#folgendes tun:
#scatterplot
Teebeutel %>% 
  ggplot(aes(x = Treatment, y = Wt_Red, color = Treatment)) +
  geom_point() +
  scale_color_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  labs(y = "Wt1 roter Tee",
       title = "Wt1 der Teebeutel in Zusammenhang mit der jewiligen Duengung")

ggsave(paste0("./Datenausgabe/Scatterplot_Wt1_Red.pdf"),
       width = 30,
       height = 18,
       units = "cm",
       dpi = 300)

Teebeutel %>% 
  ggplot(aes(x = Treatment, y = Wt_Green, color = Treatment)) +
  geom_point() +
  scale_color_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  labs(y = "Wt1 gruener Tee",
       title = "Wt1 der Teebeutel in Zusammenhang mit der jewiligen Duengung")

ggsave(paste0("./Datenausgabe/Scatterplot_Wt1_Green.pdf"),
       width = 30,
       height = 18,
       units = "cm",
       dpi = 300)


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

Teebeutel %>% 
  ggplot(aes(x = Treatment, y = Initial_W_Red, color = Treatment)) +
  geom_boxplot() +
  theme(legend.position = "none")+
  labs(y = "Anfangsgewicht in Gramm")

ggsave(paste0("./Datenausgabe/boxplot_InitialWeight_Red.pdf"),
       width = 30,
       height = 18,
       units = "cm",
       dpi = 300)

Teebeutel %>% 
  ggplot(aes(x = Treatment, y = Initial_W_Green, color = Treatment)) +
  geom_boxplot() +
  theme(legend.position = "none")+
  labs(y = "Anfangsgewicht in Gramm")

ggsave(paste0("./Datenausgabe/boxplot_InitialWeight_Green.pdf"),
       width = 30,
       height = 18,
       units = "cm",
       dpi = 300)


#n?chster Punkt:
#signifikanzanalyse
#unterschiede im gewicht bei den verschiedenen treatments nachweisen! 
#test finden, der alle!! treatments ber?cksichtigt
#post hoc!


## Varianzhomogenität test, hier gegeben da p > 0.05
leveneTest(Teebeutel$Wt_Red, Teebeutel$Treatment, center = median)


## keine Signifikanten Unterschiede da p > 0.05, auch in den Plots zu erkennen
mod1 <- aov(Wt_Red ~ Treatment, data = Teebeutel)
summary(mod1)

## Keine Normalverteilung, anderer Test von Nöten. Wahrscheinlich logistisches Modell 
## oder aber den Ausreißer in Cut & Compost entfernen, da dieser das Ergebnis stark beeinflussen wird [Siehe Boxplot]
## Und dann wie folgt:
shapiro.test(residuals(mod1))

TukeyHSD(mod1)
## keine Signifkanten Unterschiede zwischen den Treatments, da p > 0.05

## Grüne Beutel
## Varianzhomogenität test, hier gegeben da p > 0.05
leveneTest(Teebeutel$Wt_Green, Teebeutel$Treatment, center = median)


## keine Signifikanten Unterschiede da p > 0.05, auch in den Plots zu erkennen
mod2 <- aov(Wt_Green ~ Treatment, data = Teebeutel)
summary(mod2)

## Keine Normalverteilung, anderer Test von Nöten. Wahrscheinlich logistisches Modell 
shapiro.test(residuals(mod2))

TukeyHSD(mod2)
## keine Signifkanten Unterschiede zwischen den Treatments, da p > 0.05
