# Wyczyszczenie wszystkich zmiennych:
rm(list=ls(all=TRUE))

##########################################################################################
###### Funkcje pomocnicze:
## Podstawowa funkcja pomocnicza:

obliczPoprawke <- function(mojeDane)
{
  # Funkcja oblicza poprawkę ze względu na wilgotność gleby dla danych COSMOS
  # zgodnie z http://cosmos.hwr.arizona.edu/Docs/rosolem13-effect-of-water-vapor.pdf
  # (APPENDIX - Computation of Absolute Humidity)
  
  timeVec <- mojeDane[,"TIME"]
  T0 <- mojeDane[,"TEM"]
  RH0 <- mojeDane[,"RH"]
  Nmeas <- mojeDane[,"CORR"]
  orgSM <- mojeDane[,"SOILM"]
  
  T0 <- as.double(T0)
  RH0 <- as.double(RH0)
  es0 <- paraWstanieNasycenia(T0)
  e0 <- cisnienieParyWodnej(RH0, es0)
  qv0 <- wilgotnoscBezwzgledna(e0,T0)
  qv0REF <- qv0REFDerlo(timeVec, qv0)
  print(paste("qv0REF =", qv0REF))
  #Wartość korekty Cwv powinna być rzedu 1+/-2% (czyli miedzy 0.98 a 1.02.)
  CWV_przedPopr <- wspSkalujacy(qv0,qv0REF)
  CWV <- wspSkalujacy(qv0,qv0REF)
  CWV[is.infinite(CWV)] <- 1
  NCORR <- poprawN(Nmeas, CWV)
  
  # na podstawie: http://cosmos.hwr.arizona.edu/Probes/StationDat/084/calib.php
  # phi_0 =	3145	cph	From COSMOS web page for Derlo
  # phi0	3145	cph	Computed using calibration function, no lattice water - the same as COSMOS web page (and above)
  phi0 = 3145
  print(paste("phi0 =", phi0))
  
  # Wzór 4 na stronie 4084 jest dla wody w glebie mierzonej w jednostkach g wody na g suchej gleby (czyli ang. gravimetric water content). 
  # Zawartosc wody w jednostkach objetosciowych (objetosc wody dzielona przez objetosc gleby) jest uzyskana przez pomnozenie gravimetric water content przez gestosc suchej gleby (dry bulk density), ktora dla Derla wynosi 1.45 g/cm3 (http://cosmos.hwr.arizona.edu/Probes/StationDat/084/calib.php).
  SM <- wodaWglebie(NCORR, phi0)
  
  #czyli dane bez poprawki:
  SMbezPopr <- wodaWglebie(Nmeas, phi0)
  
  #można porównać z mojeDane[,"SOILM"]
  
  dane <- cbind.data.frame(
    timeVec, T0, RH0, Nmeas, es0, e0, qv0, CWV_przedPopr, CWV, NCORR, SM, SMbezPopr, orgSM
  )
 
  colnames(dane) <- c(
    "TIME", "T0 - temperatura powietrza w stopniach C","RH0 - wilgotność względna powietrza",
    "CORR (Level2)", "es0 - ciśnienie pary wodnej w stanie nasycenia (hPa)", "e0", "qv0 - wilgotność bezwględna powietrza",
    "CWV", "CWV (Inf zastąpione 1)", "NCORR = CORR * CWV", "obliczone SM (wzór 4, strona 4084)", "SM (wzór 4) bez uwzględniania poprawki", "SOILM (Level2)"
    )
  
  return(dane)
  
}

## Inne funkcje pomocnicze:
sprawdz <- function(var1,var2)
{
  print(
    paste(
      "Obliczony zakres wartości zmiennej:", var2,
      "minimum: ", min(var1), "maksimum: ", max(var1), "średnia: ", mean(var1),sep = " - "
    )
  )
}


zwrocDaneDoObliczen <- function(sciezka_do_pliku)
{
  # Funkcja zwraca ramkę danych (data frame) R! zawierającą wszystkie wielkości potrzebne do
  # obliczenia poprawki dla danych COSMOS ze względu na parę wodną.
  # Parametry wejściowe: sciezka_do_pliku - ścieżka do pliku Matlaba (.mat)
  
  # Ładowanie pakietu pozwalającego na odczyt plików .mat
  # if (!require("pacman")) install.packages("pacman")
  pacman::p_load(R.matlab)
  
  dane <- readMat(sciezka_do_pliku)
  
  Level1 <- dane$Level1 # rownames(Level1): "TEM"   "UNMO"  "BATT"  "TIME"  "PRESS" "RH"    "MOD"
  Level2 <- dane$Level2 # rownames(Level2): "INTEN" "SCALE" "CORR"  "SANPE" "TIME"  "PRESS" "ERR"   "PROBE" "OTHER" "MOD"
  Level3 <- dane$Level3 # rownames(Level3): "DEP"   "SM12H" "SOILM" "D12"   "TIME" 
  
  doObliczen <- suppressWarnings(cbind(
    format(as.POSIXct(
      unlist(Level1[4])*86400 - 86400,tz = "UTC", origin="0000-01-01"),"%Y-%m-%d %H:%M"
    ), # "TIME" dates are stored as days since Jan-1-0000.
    # zgodnie z http://cosmos.hwr.arizona.edu/Probes/StationDat/084/counts.txt
    # zakres daty powinny zaczynać się 2013-04-15 15:51 (UTC) i kończyć 2016-02-21 07:12 (UTC),
    # więc odejmuję jeden dzień (86400)
    as.double(unlist(Level1[1])), # "TEM"
    as.double(unlist(Level1[6])), # "RH"
    as.double(unlist(Level2[3])), # "Nmeas (zmierzona intensywnosc neutronow), ktora jest w danych Level 2, jako CORR"
    as.double(unlist(Level3[3])) # "SOILM" - do porównania z obliczoną w tym skrypcie SM
  ))
  
  colnames(doObliczen) <- c("TIME", "TEM", "RH", "CORR", "SOILM")
  
  return(doObliczen)
  
}

# Dane pobrano z http://cosmos.hwr.arizona.edu/Probes/StationDat/084/index.php

paraWstanieNasycenia <- function(T0)
{
  # Funkcja zwraca es0 - ciśnienie pary wodnej w stanie nasycenia (w hPa)
  # Parametry wejściowe: T0 - temperatura powietrza w stopniach C
  es0 = 6.112*exp( (17.67 * T0) / (243.5 + T0) )
  
  sprawdz(es0,"es0")
  
  return(es0)
  
}

cisnienieParyWodnej <- function(RH0, es0)
{
  # Funkcja zwraca e0 - właściwe ciśnienie pary wodnej
  # Parametry wejściowe: RH0 - wilgotność względna powietrza, es0 - ciśnienie pary wodnej w stanie nasycenia
  e0 = RH0 * es0
  
  sprawdz(e0,"e0")
  
  return(e0)
}

wilgotnoscBezwzgledna <- function(e0, T0)
{
  # Funkcja zwraca qv0 - wilgotność bezwględną powietrza w kg/m3
  # Parametry wejściowe: e0 - właściwe ciśnienie pary wodnej, T0 - temperatura powietrza w stopniach C
  
  Rv <- 461.5 # Rv to stała gazowa dla pary wodnej
  qv0 = e0 / (Rv * T0)
  
  sprawdz(qv0,"qv0")
  
  return(qv0)
}

qv0REFDerlo <- function(timeVec, qv0)
{
  # Dla stacji Derło kalibracja odbyła się w dniu 2013-04-16 w godzinach 9-15
  # stąd wartość referencyjna to średnia qv0 w tym czasie
  
  qv0 <- as.double(qv0)
  
  min <- "2013-04-16 09:00"
  max <- "2013-04-16 15:00"
  
  return(
    mean(
    qv0[which(timeVec >= min & timeVec <= max)],
    na.rm = TRUE
    )
  )
}

wspSkalujacy <- function(qv0, qv0REF)
{
  # Funkcja zwraca CWV - "single scaling factor"
  # Parametry wejściowe:  qv0 - wilgotność bezwględną powietrza w kg/m3, qv0REF - wartość referencyjna
  # Dla stacji Derło kalibracja odbyła się w dniu 2013-04-16 w godzinach 9-15
  #Wartość korekty Cwv powinna być rzedu 1+/-2% (czyli miedzy 0.98 a 1.02).
  
  delta = as.double(qv0) - as.double(qv0REF)
  CWV = 1 + 0.0054 * delta
  
#   if ( any(is.infinite(CWV)) )
#   {
#     message("W danych były wartości CWV równe Inf. Zostaną zamienione na 1.")
#     CWV[is.infinite(CWV)] <- 1
#   }
  
  sprawdz(CWV,"CWV - single scaling factor ")
  
  return(CWV)
}

poprawN <- function(Nmeas,CWV)
{
  # Funkcja zwraca poprawioną wartość Nmeas, czyli "fast neutron intensity measured on a given day"
  
  return(as.double(Nmeas) * as.double(CWV))
}

wodaWglebie <- function(phi, phi0)
{
  # Funkcja zwraca wilgotność gleby (wzór 4, strona 4084 w M. Zreda et al.: COSMOS: the COsmic-ray Soil Moisture Observing System)
  # Parametry wejściowe: phi0 - "the neutron intensity in air above dry soil (obtained  by calibration, see Sect. 2.5)"
  # Ustawienie stałych parametrów:
  a0 = 0.0808 # a0	0.0808	g/g	Calibration constant (from Zreda et al., 2012 and Desilets et al., 2010)
  a1 = 0.372 # a1	0.372	-	Calibration constant (from Zreda et al., 2012 and Desilets et al., 2010)
  a2 = 0.115 # a2	0.115	g/g	Calibration constant (from Zreda et al., 2012 and Desilets et al., 2010)
  phi <- as.double(phi)
  phi0 <- as.double(phi0)
  
  SM = a0 / ( (phi/phi0) - a1 ) - a2
  
  return(SM)
}

##########################################################################################


## Czytanie danych i wywołanie funkcji:
sciezka_do_pliku <- file.path(getwd(), "COSMOS_084.mat")
mojeDane <- zwrocDaneDoObliczen(sciezka_do_pliku)
obliczone <- obliczPoprawke(mojeDane)

# Eksport do Excela
pacman::p_load(xlsx)
write.xlsx(
  x = obliczone, file = "Obliczenia na podstawie danych w pliku COSMOS_084.mat.xls", 
  sheetName = "Na podstawie COSMOS_084.mat", row.names = FALSE
           )


#Histogram
dt <- obliczone[,"obliczone SM (wzór 4, strona 4084)"]
hist(dt, main="obliczone SM", col="blue",
     xlab = paste(
       "Obliczone SM, min:", round(min(dt),3), "maks: ", round(max(dt),3), "średnia: ", round(mean(dt),3)
       )
     )

dt <- obliczone[,"SM (wzór 4) bez uwzględniania poprawki"]
hist(dt, main="SM bez uwzględniania poprawki", col="blue",
     xlab = paste(
       "SM bez popr, min:", round(min(dt),3), "maks: ", round(max(dt),3), "średnia: ", round(mean(dt),3)
     )
)


dt <- obliczone[,"CWV (Inf zastąpione 1)"]
hist(dt, main="CWV (Inf zastąpione 1)", col="blue",
     xlab = paste(
       "CWV, min:", round(min(dt),3), "maks: ", round(max(dt),3), "średnia: ", round(mean(dt),3)
     )
)
