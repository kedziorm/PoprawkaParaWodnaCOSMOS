## Podstawowa funkcja:

obliczPoprawke <- function(T0, RH0)
{
  # Funkcja oblicza poprawkę ze względu na wilgotność gleby dla danych COSMOS
  # zgodnie z http://cosmos.hwr.arizona.edu/Docs/rosolem13-effect-of-water-vapor.pdf
  # (APPENDIX - Computation of Absolute Humidity)
  T0 <- as.double(T0)
  RH0 <- as.double(RH0)
  es0 <- paraWstanieNasycenia(T0)
  e0 <- cisnienieParyWodnej(RH0, es0)
  qv0 <- wilgotnoscBezwzgledna(e0,T0)
  
  # ????!!!! Chyba samo obliczenie qv0 nie wystarcza, coś trzeba zrobić z tą wartością
  
  return(qv0)
}

## Funkcje pomocnicze:
zwrocDaneDoObliczen <- function(sciezka_do_pliku)
{
  # Funkcja zwraca ramkę danych (data frame) R! zawierającą wszystkie wielkości potrzebne do
  # obliczenia poprawki dla danych COSMOS ze względu na parę wodną.
  # Parametry wejściowe: sciezka_do_pliku - ścieżka do pliku Matlaba (.mat)
  
  # Ładowanie pakietu pozwalającego na odczyt plików .mat
  # if (!require("pacman")) install.packages("pacman")
  pacman::p_load(R.matlab)
  
  dane <- readMat(sciezka_do_pliku)
  
  Level1 <- dane$Level1
  Level2 <- dane$Level2
  
  doObliczen <- cbind(
    format(as.POSIXct(
      unlist(Level1[4])*86400,origin="0000-01-01"),"%Y-%m-%d %H:%M"
    ), # "TIME" dates are stored as days since Jan-1-0000.
    as.double(unlist(Level1[1])), # "TEM"
    as.double(unlist(Level1[6])) # "RH"
  ) 
  
  colnames(doObliczen) <- c("Time", "Tem", "RH")
  
  return(doObliczen)
  
}

paraWstanieNasycenia <- function(T0)
{
  # Funkcja zwraca es0 - ciśnienie pary wodnej w stanie nasycenia (w hPa)
  # Parametry wejściowe: T0 - temperatura powietrza w stopniach C
  es0 = 6.112*exp( (17.67 * T0) / (243.5 + T0) )
  return(es0)
  
}

cisnienieParyWodnej <- function(RH0, es0)
{
  # Funkcja zwraca e0 - właściwe ciśnienie pary wodnej
  # Parametry wejściowe: RH0 - wilogtność względna, es0 - ciśnienie pary wodnej w stanie nasycenia
  e0 = RH0 * es0
  return(e0)
}

wilgotnoscBezwzgledna <- function(e0, T0)
{
  # Funkcja zwraca qv0 - wilgotność bezwględną w kg/m3
  # Parametry wejściowe: e0 - właściwe ciśnienie pary wodnej, T0 - temperatura powietrza w stopniach C
  
  Rv <- 461.5 # Rv to stała gazowa dla pary wodnej
  qv0 = e0 / (Rv * T0)
  return(qv0)
}

