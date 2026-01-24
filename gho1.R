## --R--
##
## Przykład pobrania i analizy danych z bazy WHO
## pn Global Health Observatory
## https://www.who.int/data/gho
##
## Instalowanie jak nie ma
##devtools::install_github("aphp/rgho")
##
library('rgho')
library('tidyverse')
## Podaj wymiary danych
dimensions.who <- get_gho_dimensions()

## Przykłady wymiarów
## regiony świata
gho.regions <- get_gho_values(dimension = "REGION")
## wskaźniki
gho.values.gho <- get_gho_values(dimension = "GHO") 
## nazwy krajów
gho.countries <- get_gho_values(dimension = "COUNTRY")

## Pobranie danych przykłady
#result <- get_gho_data(
#  code = "MDG_0000000001"
#)
## Pobranie danych z filtrem
## EUR dla roku 2015
#result <- get_gho_data(
#  code = "MDG_0000000001",
#  filter = list(
#    REGION = "EUR",
#    YEAR = 2015
#  )
#)

##
## Prevalence of diabets (Częstość występowania cukrzycy)
## Dane wg wszystkich wymiarów
prev.diabets <- get_gho_data( code='NCD_DIABETES_PREVALENCE_AGESTD') 
##
## dla pewności :-)
## Jakie wartości ma wymiar SEX?
gho.sex <- get_gho_values(dimension = "SEX")

## Dane są ściągane jako napisy
## Dlatego warto je zamienić na odpowiednie typy, np rok na liczbę
## wyrażenie |> funkcja -- operator potoku, przekaż wynik wyrażenia jako argument funkcji
##
prev.diabets.0 <- prev.diabets |>
  select (p=NumericValue, COUNTRY, year=YEAR, sex=SEX, AGEGROUP, ParentLocationCode) |>
  mutate (p = as.numeric(p), 
          year = as.numeric(year))

## Lista krajów
## ramka$kolumna
## levels -- funkcja zwracająca zbiór wartości zmiennej typu `factor'
levels(as.factor(prev.diabets.0$COUNTRY))

## Filtrowanie
## Tylko Polska
prev.diabets.pl <- prev.diabets.0 |> filter (COUNTRY == 'POL')
##
## Polska rok 2022
prev.diabets.pl.22 <- prev.diabets.pl |> filter (YEAR == '2022')

## Złączenie wszystkich krajów z wymiarem COUNTRY
## (żeby w ramce mieć nazwy krajów a nie tylko kody)
prev.diabets.1 <- left_join(prev.diabets.0, gho.countries, by=c('COUNTRY'='Code'))

## Przykłady innych zapytań
##
## Bez rozróżniania wielkości liter ile jest wskaźników
## ze słowem `prevalence` w Tytule 
##
## result.prevalence <- gho.values.gho |> 
##   filter (str_detect(str_to_lower(Title), "prevalence"))
##

## Analiza krajów Europejskich
## dla grupy wiekowej 30+
## ---------------------------
## Tylko kraje europejskie
## Tylko lata 2000, 2010 oraz 2022
## Tylko łącznie (Płeć)
## Tylko grupa wiekowa 30+
prev.diabets.eur <- prev.diabets.0 |> 
  filter (ParentLocationCode == 'EUR') |> ## Europa
  filter (year == 2000 | year == 2010 | year == 2022) |> ## rok
  filter (sex == 'BTSX') |> ## płeć
  filter (AGEGROUP == 'YEARS30-PLUS') ## grupa wiekowa

## wartości sumaryczne
## grupujemy wg lat
## obliczamy średnią, medianę oraz odchylenie standardowe
prev.diabets.eur.summ <- prev.diabets.eur |>
  group_by(year) |>
  summarise(mean = mean(p, na.rm = TRUE),
            median = median (p),
            sd = sd (p, na.rm = TRUE)
            )
prev.diabets.eur.summ
##
## albo ładniejszy wydruk
library('knitr')
knitr::kable(prev.diabets.eur.summ)

# na wykresie
# box-plot
prev.diabets.pl <- prev.diabets.eur |> filter (COUNTRY == 'POL')

## wykres podełkowy
pic1 <- prev.diabets.eur |> 
  ggplot(aes(y=p, x=as.factor(year) )) +
  geom_boxplot() +
  ylab("#") +
  ggtitle("??") +
  xlab('') +
  ## opcjonalnie
  ## położenie Polski
  geom_point(
    data = prev.diabets.pl,
    aes(x = as.factor(year), y = p ),
    color = "red",
    size = 3
  )
pic1

## wykres punktowy
pic2 <- prev.diabets.eur |> 
  ggplot(aes(y=p, x=as.factor(year) )) +
  geom_jitter(width = 0.1, alpha=.5) +
  ylab("#") +
  ggtitle("??") +
  xlab('') +
  ## Polska
  geom_point(
    data = prev.diabets.pl,
    aes(x = as.factor(year), y = p ),
    color = "red",
    size = 2
  )
pic2

## Porównanie w czasie
## chatGPT
prev.diabets.eur.all <- prev.diabets.0 |> 
  filter (ParentLocationCode == 'EUR') |>
  filter (sex == 'BTSX') |>
  filter (AGEGROUP == 'YEARS30-PLUS') |>
  ## dla pewności
  arrange(COUNTRY, year)

## Średnie tempo wzrostu wg wzoru:
## stw = (p_t/p_1)^{1/(t-1) -1}, gdzie
## p_t ostatnia obserwacja
## p_1 pierwsza obserwacja
## ^ oznacza potęgowanie, wykładnikiem potęgi jest 1/(t-1)
##
prev.diabets.eur.all.sum <-  prev.diabets.eur.all |>
 group_by(COUNTRY) |>
   summarise(
     ## średnie temp w %
     srednie_tempo = ((last(p) / first(p))^(1 / (n() - 1)) - 1 ) * 100,
   ) |>
  ungroup()
knitr::kable(prev.diabets.eur.all.sum)

## kraje o tempie wzrostu > 2.2
high.diabets <- prev.diabets.eur.all.sum |> filter (srednie_tempo > 2.2)
knitr::kable(high.diabets)
## kraje o zmniejszającym się odsetku diabetyków
## **praca domowa :-) **

## Porownanie w czasie dla wybranych krajów
prev.diabets.eur.pid <- prev.diabets.eur.all |>
  filter (COUNTRY == 'POL' | COUNTRY == 'ITA' | COUNTRY == 'DEU')

pic3 <- prev.diabets.eur.pid |> 
  ggplot(aes(y=p, x=year, color=COUNTRY )) +
  geom_line() +
  ylab("#") +
  ggtitle("??") +
  xlab('')
pic3

## Wszystkie kraje o liczebności > 25 mln
## Nie ma liczeby ludności w bazie WHO
## bierzemy z bazy Banku Światowego
library('WDI')
help('WDI')

pop <- WDI(
  country = "all",
  indicator = "SP.POP.TOTL",
  ##start = 1960,
  ##end = NULL,
  extra = TRUE
)

## Ile jest krajów w Europie
eur.countries <- levels(as.factor(prev.diabets.eur.all$COUNTRY))
length(eur.countries) ## 52 kraje

eur.countries
## Pytanie do chataGPS
## Dlaczego w bazie WDI i WHO, region europejski obejmuje np 
## Uzbekistan i Kazachstan?
## Short answer:
## Ponieważ WDI i WHO stosują instytucjonalny, a nie geograficzny 
## podział świata, w którym państwa 
## Azji Centralnej (np. Kazachstan i Uzbekistan) historycznie 
## należą do Regionu Europejskiego WHO, 
## głównie z powodu przynależności do struktur postsowieckich 
## i dla zachowania spójności danych.
## czytaj:
## Bo były kiedyś częścią ZSRR
##

pop.eur <- pop |> filter (iso3c %in% eur.countries) |>
  filter (year == 2024)

pop.eur.big <- pop.eur |> filter (SP.POP.TOTL > 25000000)
eur.countries.big <- levels(as.factor(pop.eur.big$iso3c))
                        
prev.diabets.eur.big <- prev.diabets.eur.all |> 
  filter (COUNTRY %in% eur.countries.big)

pic4 <- prev.diabets.eur.big |> 
  ggplot(aes(y=p, x=year )) +
  geom_line() +
  facet_wrap(~ COUNTRY) +
  ylab("#") +
  ggtitle("??") +
  xlab('')
pic4

###
### KONIEC ####
###
