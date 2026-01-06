## --R--
##
## Przykład pobrania i analizy danych z bazy Banku Światowego
## https://data.worldbank.org/
##
##
## install.packages("WDI")
##

library('WDI')
library('tidyverse')
library('knitr')
#help('WDI')

df <- WDI(
  country = "all",
  ## EG.USE.ELEC.KH.PC = electric power consumption KwH per capita
  ## NY.GDP.PCAP.CD = GDP current USD
  ## NY.GDP.PCAP.KD = GDP constant USD
  indicator = c("EG.USE.ELEC.KH.PC", 'NY.GDP.PCAP.CD', 'NY.GDP.PCAP.KD'),
  ##start = 1960,
  ##end = NULL,
  extra = TRUE
)

## Dla wielu krajów brak danych

df1 <- df |> group_by(iso3c) |>
  arrange(iso3c, year) |>
  ## usuń wiersze zawierające NA
  na.omit() |>
  slice_tail(n = 1) %>%
  ungroup() |>
  filter (year > 2021)

df1 |> filter (EG.USE.ELEC.KH.PC > 15000)

df1 |> filter (NY.GDP.PCAP.CD > 100000)
df1

## Najnowsze dane dla każdego kraju

p1 <- df1 |>
  filter (EG.USE.ELEC.KH.PC < 15000) |>
  filter (NY.GDP.PCAP.CD > 3000) |>
  ggplot(aes(y=NY.GDP.PCAP.CD, x=EG.USE.ELEC.KH.PC)) +
  geom_point()
p1
