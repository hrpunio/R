## -- R --
## Przekształcanie danych
library("tidyverse")
## Zestawienia tabelaryczne
library("knitr")
## Eurostat API
library("eurostat")
##
##
ex <- c('CH', 'LU', 'ME', 'MK', 'MT', 'RS', 'CY')

## ChatGPT: struktura identyfikatorów w systemie NUTS
## System NUTS (Nomenclature of Territorial Units for Statistics) to hierarchiczna 
## klasyfikacja jednostek terytorialnych stosowana w UE do celów statystycznych. 
## Każda jednostka ma jednoznaczny identyfikator (kod NUTS) 
## o określonej strukturze:
##  CCMRP
##  gdzie CC -- państwo; M -- makroregion; R -- region; P -- podregion, np
##  PL = Polska, PL2 M. południowy, PL21 Małopolskie, PL213 podregion krakowski
##  (NUTS0)     (NUTS1)             (NUTS2)           (NUTS3)
##  region = województwo (prawie, mazowieckie jest dzielone na dwa)

fert_0 <- get_eurostat("tgs00100",  stringsAsFactors = FALSE) |>
  select (geo, TIME_PERIOD, values) |>
  mutate (
    year = as.numeric(substr(TIME_PERIOD, 1, 4))) |>
  ## kolumna member = pierwsze dwa znaki z kolumny geo (zawierającej 4 znakowy
  ## identyfikator NUTS)
  mutate (member = substr(geo, 1, 2)) |>
  ## pomiń kraje `egozytyczne'
  filter (! member %in% ex )

year.min <- min(fert_0$year)
year.max <- max(fert_0$year)

## Tylko polska
fert_0_pl <- fert_0 |> filter (member == 'PL') 

## Roku 2023 (najnowsze dane)
fert_2023 <- fert_0 |> filter (year == 2023)

median.fert <- median(fert_2023$values)

p0 <- ggplot(fert_2023, aes(x=member, y=values )) + 
  geom_boxplot( color='deeppink')  + 
  xlab("kraj") + 
  ylab("tfr") +
  ggtitle("Fertility rate by NUTS2 regions (2023)", subtitle="Eurostat: tgs00100 table") +
  geom_hline(yintercept = 2.15, color="navyblue", alpha=.25, linewidth=1.2) +
  geom_hline(yintercept = median.fert, color="forestgreen", alpha=.25, linewidth=1.2)

p0
## zapisanie na dysk w formacie PNG
ggsave(p0, file="eurofert.png", width=10)

## Polska dla wybranych lat
fert_0_pl_xx <- fert_0_pl |> 
  filter (year == 2012 | year == 2018 | year == 2023)

## Wykres punktowy
p2 <- fert_0 |> 
  filter (year == 2012 | year == 2018 | year == 2023) |>
  ##
  ggplot(aes(y=values, x=as.factor(year) )) +
    geom_jitter(width = 0.1, alpha=.25) +
  ylab("#") +
  ggtitle("??") +
  xlab('') +
  ## Polska
  geom_point(
    data = fert_0_pl_xx,
    aes(x = as.factor(year), y = values ),
    color = "red",
    alpha = .5,
    size = 2
  ) +
  geom_hline(yintercept = median.fert, color="forestgreen", alpha=.25, linewidth=1.2)
p2

## wybrane kraje
## DE PL IT ES CZ
my.members <- c('DE', 'PL', 'IT', 'ES', 'CZ', 'EU27_2020')
 
p3 <- fert_0 |> 
  ggplot(aes(y=values, x=year, color=member )) +
  geom_line() +
  ylab("#") +
  ggtitle("??") +
  xlab('')
p3

## źle nie ten poziom
## ------------------
p3 <- fert_0 |> filter (member %in% my.members) |>
  ggplot(aes(y=values, x=year, color=member )) +
  geom_line() +
  ylab("#") +
  ggtitle("??") +
  xlab('')
p3

## Jaka tabela zawiera TFR wg krajów NUTS0 w bazier Eurostat?
## demo_find
## albo tps00199
## 

fert_1 <- get_eurostat("demo_find",  stringsAsFactors = FALSE) |>
  ##select (geo, TIME_PERIOD, values) |>
  mutate (year = as.numeric(substr(TIME_PERIOD, 1, 4))) 

##levels(as.factor(fert_1$indic_de))
##[NMARPCT] = Proportion of live births outside marriage
##[TOTFERRT] = Total fertility rate

fert_2 <- fert_1 |>
  filter (indic_de == 'TOTFERRT') |>
  select (geo, tfr=values, year)

p3b <- fert_2 |> filter (geo %in% my.members) |>
ggplot(aes(y=tfr, x=year, color=geo )) +
  geom_line() +
  ##geom_smooth(se=FALSE) +
  ylab("#") +
  ggtitle("Współczynnik dzietności dla wybranych krajów UE",
          subtitle = 'w latach 1960--2023') +
  xlab('')
p3b

## Zauważmy dużo dłuższy horyzont czasowy
year.min <- min(fert_2$year)
year.max <- max(fert_2$year)

p3c <- fert_2 |> filter (geo %in% my.members) |>
  ggplot(aes(y=tfr, x=year, color=geo )) +
  geom_line() +
  ##geom_smooth(se=FALSE) +
  ylab("#") +
  ggtitle("Współczynnik dzietności dla wybranych krajów UE",
          subtitle = sprintf('w latach %i--%i', year.min, year.max)) +
  xlab('')
p3c

## rok
p3d <- fert_2 |> filter (geo %in% my.members) |>
  filter (year >= 2000) |>
  ggplot(aes(y=tfr, x=year, color=geo )) +
  geom_line() +
  ##geom_smooth(se=FALSE) +
  ylab("#") +
  ggtitle("Współczynnik dzietności dla wybranych krajów UE",
          subtitle = sprintf('w latach %i--%i', year.min, year.max)) +
  xlab('')
p3d

##

p3z <- fert_2 |> ##filter (geo %in% my.members) |>
  ggplot(aes(y=tfr, x=year, color=geo )) +
  geom_line() +
  ##geom_smooth(se=FALSE) +
  ylab("#") +
  ggtitle("Współczynnik dzietności dla wybranych krajów UE",
          subtitle = sprintf('w latach %i--%i', year.min, year.max)) +
  xlab('')
p3z
