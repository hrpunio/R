## -- R --
library("tidyverse")
library("ggplot2")
## install.packages('WDI')
library('WDI')

start.yr <- 1960
countries <- c('CN','US', 'RU', 'IN', 'JP', 'PL', 'DE')

kraje <- WDI::WDI_data$country
szeregi <- WDI::WDI_data$series

## ile krajów?
nrow(kraje)
## tyle nie ma na świecie

kraje_iso3 <- kraje %>%
  filter(region != "Aggregates") %>%
  pull(iso3c)
length(kraje_iso3)

note <- 'World Bank: SP.DYN.TFRT.IN'

my.indicators <- c('SP.DYN.TFRT.IN')

tfr0 <- WDI(indicator=my.indicators, country=countries, start= start.yr )
## ile krajów
levels(as.factor(tfr0$iso3c))

##
## This is big 
tfr0 <- WDI(indicator=my.indicators, start= start.yr ) |>
  filter (iso3c %in% kraje_iso3)
## ile levelów :-)
nlevels(as.factor(tfr0$iso3c))

## Population + GDP constant prices
## GDP: jaki ma symbol zmienna zawierająca GDP w cenach stałych na głowę w bazie banku światowego?
my.indicators <- c('SP.DYN.TFRT.IN', 'SP.POP.TOTL', 'NY.GDP.PCAP.KD')

tfr0 <- WDI(indicator=my.indicators, start= start.yr ) |>
  filter (iso3c %in% kraje_iso3) |>
  select (country, iso3c, year, tfr=SP.DYN.TFRT.IN, pop=SP.POP.TOTL, gdp=NY.GDP.PCAP.KD)

## kraje o liczbie ludności > 5mln w 2024 roku
pop_over5 <-tfr0 |> filter (year == 2024) |> 
  filter (pop > 5000000) |>
  pull (iso3c)
length(pop_over5)

## Poniższe nie uwzględnia NA w roku 2024;
gdp_over10 <-tfr0 |> filter (year == 2024) |> 
  filter (gdp > 10000) |>
  pull (iso3c)
length(gdp_over10)

## Tylko w miarę bogate kraje o liczbie ludności > 5
tfr1 <- tfr0 |>
  ## tylko kraje o liczbie ludności > 5 mln
  filter(iso3c %in% pop_over5) |>
  filter(iso3c %in% gdp_over10)


###

## Polska dla wybranych lat
fert_1_pl <- tfr1 |>
  filter (year == 1980 | year == 1990 | year == 2000 | year == 2010 | year == 2023 ) |>
  filter (iso3c == 'POL')
  

p2 <- tfr1 |>
  filter (year == 1980 | year == 1990 | year == 2000 | year == 2010 | year == 2023) |>
  ggplot(aes(y=tfr, x=as.factor(year) )) +
  geom_jitter(width = 0.1, alpha=.25) +
  ylab("#") +
  ggtitle("TFR w wybranych krajach świata") +
  xlab('') +
  ## Polska
  geom_point(
    data = fert_1_pl,
    aes(x = as.factor(year), y = tfr ),
    color = "red",
    alpha = .5,
    size = 2
  ) 
  #geom_hline(yintercept = median.fert, color="forestgreen", alpha=.25, linewidth=1.2)
p2

## Zależność pomiędzy TFR a GDP/pc

p3 <- tfr1 |>
  filter (year == 2023) |>
  ggplot(aes(y=tfr, x=gdp )) +
  geom_point(color='navy', alpha=.25)

p3

## Policzenie współczynnika korelacji
tfr2023 <- tfr1 |>filter (year == 2023) 

r <- cor(tfr2023$tfr, tfr2023$gdp, method = 'pearson')
rs <- cor(tfr2023$tfr, tfr2023$gdp, method = 'spearman')
rs
## Ujemna korelacja pomiędzy TFR a GDPpc
##