## --R--
##
## Przykład pobrania i analizy danych z bazy WHO
## pn Global Health Observatory
## https://www.who.int/data/gho
##
##
##devtools::install_github("aphp/rgho")
##
library('rgho')
library('tidyverse')
## Podaj wymiary danych

## Wszystkie wkaźniki ze słowem `obesity`
result.prevalence <- gho.values.gho |> 
  filter (str_detect(str_to_lower(Title), "obesity"))
##
## Prevalence of obesity (Częstość występowania otyłości)

prev.obesity <- get_gho_data( code='NCD_BMI_30C',
   filter = list (COUNTRY = 'POL')
   ##filter = list (COUNTRY = 'POL', YEAR = 2020)
) 

## Dane są ściągane jako napisy
## Dlatego warto je zamienić na odpowiednie typy, np rok na liczbę
prev.obesity.0 <- prev.obesity |>
  select (p=NumericValue, year=YEAR, sex=SEX, AGEGROUP, ParentLocationCode) |>
  mutate (p = as.numeric(p), year = as.numeric(year)) |>
  ## zamiana kształtu ramki z `węższej' na `szerszą'
  ## wartości zmiennej sex stają się nazwami kolumn
  ## kolumna p zawiera wartości
  pivot_wider(names_from = 'sex', values_from = 'p') |>
  arrange(year) |>
  select (rok=year, bmio=BTSX, bmik=FMLE, bmim=MLE)
  
min(prev.obesity.0$rok)
max(prev.obesity.0$rok)

## AGEGROUP i ParentLocationCode są niepotrzebne

## Średnie tempo wzrostu wg wzoru:
## stw = (p_t/p_1)^{1/(t-1) -1}, gdzie
## p_t ostatnia obserwacja
## p_1 pierwsza obserwacja
## ^ oznacza potęgowanie, wykładnikiem potęgi jest 1/(t-1)
##
stm <- prev.obesity.0  |>
  summarise(
    ## średnie temp w %
    srednie_tempo_o = ((last(bmio) / first(bmio))^(1 / (n() - 1)) - 1 ) * 100,
    srednie_tempo_k = ((last(bmik) / first(bmik))^(1 / (n() - 1)) - 1 ) * 100,
    srednie_tempo_m = ((last(bmim) / first(bmim))^(1 / (n() - 1)) - 1 ) * 100,
  ) |>
  ungroup()

##
## KONIEC