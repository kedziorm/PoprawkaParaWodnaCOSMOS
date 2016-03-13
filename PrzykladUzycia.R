# Wyczyszczenie wszystkich zmiennych:
rm(list=ls(all=TRUE))

## Wczytanie pliku z funkcjami:
source(file.path(getwd(), "COSMOSPoprawkaParaWodna.R"))

## Czytanie danych i wywołanie funkcji:
sciezka_do_pliku_MAT <- file.path(getwd(), "COSMOS_084.mat")
obliczone <- obliczPoprawke(sciezka_do_pliku_MAT)

# Eksport do Excela
pacman::p_load(xlsx)
write.xlsx(
  x = obliczone, file = "Obliczenia na podstawie danych w pliku COSMOS_084.mat.xls", 
  sheetName = "Na podstawie COSMOS_084.mat", row.names = FALSE
)


#Histogram
dt <- obliczone[,"(SM - LW) * rho_b"]
hist(dt, main="obliczone SM", col="blue",
     xlab = paste(
       "Obliczone SM, min:", round(min(dt, na.rm=TRUE),3), "maks: ", round(max(dt,na.rm=TRUE),3), "średnia: ", round(mean(dt,na.rm = TRUE),3)
     )
)

dt <- obliczone[,"SM (wzór 4) bez uwzględniania poprawki"]
hist(dt, main="SM bez uwzględniania poprawki", col="blue",
     xlab = paste(
       "SM bez popr, min:", round(min(dt, na.rm = TRUE),3), "maks: ", round(max(dt, na.rm = TRUE),3), "średnia: ", round(mean(dt, na.rm = TRUE),3)
     )
)


dt <- obliczone[,"CWV (Inf zastąpione 1)"]
hist(dt, main="CWV (Inf zastąpione 1)", col="blue",
     xlab = paste(
       "CWV, min:", round(min(dt, na.rm = TRUE),3), "maks: ", round(max(dt, na.rm = TRUE),3), "średnia: ", round(mean(dt, na.rm = TRUE),3)
     )
)
