#### Datenbearbeitung ####

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

Teebeutel_8cm <- read.csv2("./Dateneingabe/Teebeutel_R_8cm.csv")
Teebeutel

#oder f?r 20 cm:

Teebeutel_20cm <-read.csv2("./Dateneingabe/Teebeutel_R_20cm.csv")
Teebeutel

#oder f?r eine Testdatei:

Teebeutel_sortiert<-read.csv2("./Dateneingabe/Teebeutel_sortiert.csv")
Teebeutel


#?r Teebeutel ausgegraben:

Teebeutel<-read.csv2("C:/Users/Alexandra/Desktop/praktikum_2018/R/Teebeutel_ausgegraben_csv.csv")
Teebeutel

## Teebeutel Sortieren 


Teebeutel_sort_8cm_gruen <- Teebeutel_sortiert %>% 
  filter(Tiefe == "8cm" & Teebeutel_Farbe == "Gruen")

Teebeutel_sort_8cm_rot <- Teebeutel_sortiert %>% 
  filter(Tiefe == "8cm" & Teebeutel_Farbe == "Rot")

Teebeutel_sort_20cm_gruen <- Teebeutel_sortiert %>% 
  filter(Tiefe == "20cm" & Teebeutel_Farbe == "Gruen")

Teebeutel_sort_20cm_rot <- Teebeutel_sortiert %>% 
  filter(Tiefe == "20cm" & Teebeutel_Farbe == "Rot")

## Werte von den sortierten Dateien in die Gesamtdatei einfügen (Final Weight ergänzen)
Teebeutel_8cm <- Teebeutel_8cm %>% 
  mutate(incubation_time_red_and_green_tea_t = as.numeric(incubation_time_red_and_green_tea_t),
         Final_W_Green = Teebeutel_sort_8cm_gruen$Endgewicht ,
         Final_W_Red = Teebeutel_sort_8cm_rot$Endgewicht,
         Fraction_decomposed_green_tea_ag = 1-(Final_W_Green/Initial_W_Green),
         Wt_Green = Final_W_Green/Initial_W_Green ,
         Wt_Red = Final_W_Red/Initial_W_Red,
         S = 1 - (Fraction_decomposed_green_tea_ag/0.842),
         Predicted_labile_fraction_red_tea_ar = 0.552 * (1 - S),
         k = log(Predicted_labile_fraction_red_tea_ar/(Wt_Red-(1-Predicted_labile_fraction_red_tea_ar)))/incubation_time_red_and_green_tea_t,
         Wt2_Red= Predicted_labile_fraction_red_tea_ar * exp(-k * incubation_time_red_and_green_tea_t) + (1 - Predicted_labile_fraction_red_tea_ar),
         Wt2_Green = Fraction_decomposed_green_tea_ag * exp(-k * incubation_time_red_and_green_tea_t) + (1 - Fraction_decomposed_green_tea_ag)) %>% 
  select(-W.t._Red)

str(Teebeutel_8cm)

write.csv2(Teebeutel_8cm,
           "./Datenausgabe/Teebeutel_R_8cm_mitNA.csv")

## Löschen des NA Werts bei Final_W_Green. WO es kein Endgewicht gibt, wird zwar in Excel das Anfangsgewicht gelassen, aber da man es in R nicht stehen lassen kann, werden die Zeilen, bei denen kein Teebeutel erfolgreich ausgegraben werden konnte gelöscht.
## Andere NA Werte erstmal beibehalten um Fehler zu suchen!

Teebeutel_8cm <- Teebeutel_8cm %>% 
  filter(Final_W_Green != "NA")

write.csv2(Teebeutel_8cm,
           "./Datenausgabe/Teebeutel_R_8cm_bearbeitet.csv")

Teebeutel_20cm <- Teebeutel_20cm %>% 
  mutate(Final_W_Green = Teebeutel_sort_20cm_gruen$Endgewicht ,
         Final_W_Red = Teebeutel_sort_20cm_rot$Endgewicht)

Teebeutel_20cm <- na.omit(Teebeutel_20cm)

write.csv2(Teebeutel_20cm,
           "./Datenausgabe/Teebeutel_R_20cm_bearbeitet.csv")
