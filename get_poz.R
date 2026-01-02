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
  ##print (paste("==>", yrmon))
  
  df <- read_excel(plik, sheet = "Tabela 1", 
                 na='NA', ## domyślnie jest NA
                 skip=4,
                 ## jakiś geniusz wprowadził <5 od 2020 roku
                 col_types = c('text', 'text', 'text', 'text', 'text', 'text', 'text', 'text',
                               'text', 'text', 'text', 'text'),
                 col_names = c('icd', 'rozpoznanie', 'liczba', 'wartosc',
                               'skip1', 's2', 's3',  's4', 's5', 's6', 's7', 's8' )
  ) |>
  ## Dodaj datę
  mutate (ym = yrmon ) |>
  ## zamiń NA na zero
    mutate(liczba = as.numeric(parse_number(liczba)),
           wartosc = as.numeric(parse_number(wartosc))) |>
  ##
  mutate(across(c(liczba, wartosc), ~replace_na(.x, 0))) |>
  ##
  ## Podziel kolumny icd i rozpoznanie
  mutate(icd = sub(" ", "@", icd )) |>
  mutate(rozpoznanie = sub(" ", "@", rozpoznanie )) |>
  separate(icd, into = c("icd_code", "icd_name"), sep = "@", extra = "merge") |>
  separate(rozpoznanie, into = c("r_code", "r_name"), sep = "@", extra = "merge") |>
  ##
  separate(ym, into = c("year", "month"), sep = "_", extra = "merge") |>
  ##
  select (year, month, icd_code, icd_name, r_code, r_name, liczba, wartosc)

  wszystkie_dane <- bind_rows(wszystkie_dane, df) 
  
}

icd <- unique(wszystkie_dane |> arrange(icd_code) |>
                select (icd_code, icd_name))
rozpoznanie <- unique(wszystkie_dane |> select (r_code, r_name))

## 

wszystkie_dane_short <- wszystkie_dane |>
  select (year, month, icd_code, r_code, liczba, wartosc)

write.table(wszystkie_dane_short, "raporty_poz.csv", sep=';', 
            quote = F,
            row.names = F)

write.table(icd, "icd_codes.csv", sep=';',  quote = TRUE, row.names = F)

write.table(rozpoznanie, "rozpoznanie_codes.csv", sep=';',  quote = TRUE, row.names = F)
