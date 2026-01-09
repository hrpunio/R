library('readxl')
library('tidyverse')
##
## Rozdziały ICD wg powiatów i lat
## 
## Pobierz wszystkie pliki ze strony 
## https://ezdrowie.gov.pl/portal/home/badania-i-dane/zdrowe-dane/monitorowanie/podstawowa-opieka-zdrowotna
## do katalogu określonego przez dir_name
## uruchom skrypt z katalogu nadrzędnego (lub zmodyfikuj skrypt)
## wynik będzie w postaci trzech plików
## raporty_poz.csv = tylko kody bez nazw
## icd_codes.csv   = kody i nazwy ICD
## rozpoznanie_codes.csv = kody i nazwy rozpoznania
##
## żeby mieć z powrotem kody i nazwy użyj left_join
##

### POZ ###
dir_name <- './poz/' ## katalog z plikami xlsx

wszystkie_dane <- data.frame()
pliki <- list.files(path = dir_name, pattern = "\\.xlsx$", full.names = TRUE)

for (plik in pliki) {
  print (plik)
  yrmon <- sub(".*POZ_", "", plik)
  yrmon <- sub(".xlsx", "", yrmon)
  ##
  ##print (yrmon)
  ##
  yymm_ <- strsplit(yrmon, "_")[[1]]
  yy <- as.integer(yymm_[1])
  mm <- as.integer(yymm_[2])
  
  if (yy > 2020 | (yy == 2020 & mm == 12)) {
    
    df0 <- read_excel(plik, sheet = "Tabela 3", 
                 na='NA', ## domyślnie jest NA
                 skip=4,
                 ## jakiś geniusz wprowadził <5 od 2020 roku
                 col_types = c('text', 'text', 'text', 'text', 'text', 'text', 'text',
                               'text', 'text', 'text', 'text', 'text', 'text', 'text'),
                 col_names = c('icd', 'woj', 'powiat', 'teryt', 'porady', 'pacjenci',
                               's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8' ) ) 
    teryt_codes <- df0 |> select(woj, powiat, teryt)
  }
  else {##
    
    df0 <- read_excel(plik, sheet = "Tabela 3", 
                      na='NA', ## domyślnie jest NA
                      skip=4,
                      ## jakiś geniusz wprowadził <5 od 2020 roku
                      col_types = c('text', 'text', 'text', 'text', 'text', 'text',
                                    'text', 'text', 'text', 'text', 'text', 'text', 'text'),
                      col_names = c('icd', 'woj', 'powiat', 'porady', 'pacjenci',
                                    's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8' ))
  }
  
  
  df <- df0 |>
  ## Dodaj datę
  mutate (ym = yrmon ) |>
  ## zamiń NA na zero
    mutate(porady = as.numeric(parse_number(porady)),
           pacjenci = as.numeric(parse_number(pacjenci))) |>
  ##
  mutate(across(c(porady, pacjenci), ~replace_na(.x, 0))) |>
  ##
  ## Podziel kolumny icd i rozpoznanie
  mutate(icd = sub(" ", "@", icd )) |>
  separate(icd, into = c("icd_code", "icd_name"), sep = "@", extra = "merge") |>
  ##
  separate(ym, into = c("year", "month"), sep = "_", extra = "merge") |>
  ##
  select (year, month, icd_code, icd_name, woj, powiat, porady, pacjenci)

  wszystkie_dane <- bind_rows(wszystkie_dane, df) 
  
}

## 
teryt_codes_uniqe <- unique(teryt_codes) |> arrange(teryt)

#teryt_ <- teryt_codes |> select(teryt) |> unique()
# <- teryt_codes |> select(powiat) |> unique()

wszystkie_dane_short <- wszystkie_dane |>
  select (year, month, icd_code, woj, powiat, porady, pacjenci) |>
  left_join(teryt_codes_uniqe, by=c('woj', 'powiat')) |>
  select (-c(woj, powiat))

write.table(wszystkie_dane_short, "raporty_powiaty_poz.csv", sep=';', 
            quote = F,
            row.names = F)
write.table(teryt_codes_uniqe, 
            "powiaty_teryt.csv", sep=';', quote = F, row.names = F)
##
