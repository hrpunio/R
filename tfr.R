# TFR, mediana wieku matki, udział kobiet 15--49 lat
library("bdl")
library("tidyverse")
#
options(bdl.api_private_key ='????????????????????????????????????????????')
#
p.vars <- c( ## P2346: TFR 
             '59049', ## tfr
             ## P2137: Ludność wg grup wieku i płci
             '72295', ## kobiety ogółem
             ## grupy wiekowe 15--49 lat (kobiety):
             '72299', '47738', '47696', '47695', '47716', '47698', '47727',
             ## P2167: Urodzenia żywe wg pojedynczych roczników wieku matki
             '59868', ## ogółem
             '59888', ## 12 lat i mniej
             '59862', ## 13 lat itd...
             '59882',
             '59879',
             '59869',
             '59865',
             '59861',
             '59856',
             '59883',
             '59872',
             '59858',
             '59875',
             '59855',
             '59864',
             '59863',
             '59870',
             '59887',
             '59884',
             '59893',
             '59889',
             '59866',
             '59859',
             '59876',
             '59890',
             '59892',
             '59871',
             '59873',
             '59860',
             '59886',
             '59880',
             '59891',
             '59867',
             '59881',
             '59885',
             '59878',
             '59857',
             '59877',
             '59874',
             '59854' ## 50 i więcej
             )
##
## unitLevel=4 (subregion)
d0 <- get_data_by_variable(p.vars, unitParentId=NULL, unitLevel=4)

## p = odsetek kobiet w danej grupie wiekowej (% wszystkich kobiet)
d2 <- d0 |>
  ## p = odsetek kobiet w wieku 15-49 
  mutate ( p = (val_72299 + val_47738 + val_47696 + val_47695 + val_47716 + val_47698 + val_47727)/val_72295 ) |>
  select(id, name, year, tfr=val_59049, p,
         ogolem_wiek=val_59868,
         ## wm?? = liczba matek w danym wieku
         wm12=val_59888,
         wm13=val_59862,
         wm14=val_59882,
         wm15=val_59879,
         wm16=val_59869,
         wm17=val_59865,
         wm18=val_59861,
         wm19=val_59856,
         wm20=val_59883,
         wm21=val_59872,
         wm22=val_59858,
         wm23=val_59875,
         wm24=val_59855,
         wm25=val_59864,
         wm26=val_59863,
         wm27=val_59870,
         wm28=val_59887,
         wm29=val_59884,
         wm30=val_59893,
         wm31=val_59889,
         wm32=val_59866,
         wm33=val_59859,
         wm34=val_59876,
         wm35=val_59890,
         wm36=val_59892,
         wm37=val_59871,
         wm38=val_59873,
         wm39=val_59860,
         wm40=val_59886,
         wm41=val_59880,
         wm42=val_59891,
         wm43=val_59867,
         wm44=val_59881,
         wm45=val_59885,
         wm46=val_59878,
         wm47=val_59857,
         wm48=val_59877,
         wm49=val_59874,
         wm50=val_59854,
         ) |>
  mutate (
    ## pogrupuj wiek matek:
	 gwm15 = wm12+wm13+wm14+wm15,
	 gwm20 = wm16+wm17+wm18+wm19+wm20,
	 gwm25 = wm21+wm22+wm23+wm24+wm25,
	 gwm30 = wm26+wm27+wm28+wm29+wm30,
	 gwm35 = wm31+wm32+wm33+wm34+wm35,
	 gwm40 = wm36+wm37+wm38+wm39+wm40,
	 gwm45 = wm41+wm42+wm43+wm44+wm45,
	 gwm50 = wm46+wm47+wm48+wm49+wm50,
  ) |>
  ## udziały zamiast liczebności
  mutate(across(wm12:wm50, ~ .x / ogolem_wiek * 100,
                .names = "{.col}_p")) |>
  mutate(across(gwm15:gwm50, ~ .x / ogolem_wiek * 100,
                .names = "{.col}_p")) |>
  ## usuń słowo podregion
  mutate(name = str_remove(name, "PODREGION ")) |>
  mutate(name = str_replace_all(name, "MIASTO", "m.")) |>
  mutate(name = str_to_lower(name))

## zapisz na przyszłość żeby nie ściągać z BDL
write.table(dplyr::mutate(d2, dplyr::across(where(is.numeric), ~ round(.x, 3))),
              file='tfr_PL_subregion.csv', row.names = F, sep=';')


## obliczamy medianę wieku matki na podstawie kolumn wm'cośtam'
d3 <- d2 |>
  pivot_longer(
    cols = starts_with("wm"),
    names_to = "medianAge",
    values_to = "freq"
  ) %>%
  group_by(across(c(id, year))) %>%  # 
  mutate(
    cum_freq = cumsum(freq),
    N = sum(freq)
  ) %>%
  filter(cum_freq >= N / 2) %>%
  slice(1) %>%
  ungroup() %>%
  select(id, name, year, medianAge) |>
  mutate(medianAge = parse_number(medianAge))

d4 <- d2 |> select (id, year, tfr, p, ogolem_wiek) |>
  left_join(d3, by=c('id', 'year'))

###
d4_count <- d4 %>% filter (year == 2024) |>
  group_by(medianAge) %>%
  summarise(n = n())

p1 <- d4 |> 
  filter (year == 2024) |>
  ggplot(aes(y=tfr, x=as.factor(medianAge) )) +
  geom_boxplot() +
  geom_text(
    data = d4_count,
    aes(x = as.factor(medianAge), y = 1.5, label = n ),
    color='red', size=3,
    inherit.aes = FALSE
  ) +
  ylab("Wsp. dzietności") +
  ggtitle("Wsp. dzietności w podregionach wg mediany wieku matki",
          subtitle = 'Liczby na górze wykresu oznaczają liczebności') +
  xlab('Mediana wieku') +
  labs(caption = "Źródło: GUS/BDL; podgrupy P2346, P2167, P2137")
p1

##
p2 <- d4 |> 
  filter (year == 2024 ) |>
  ggplot(aes(x=reorder(name, tfr), y=tfr)) +
  geom_bar(fill='navyblue', stat="identity") +
  ggtitle('Współczynnik TFR wg podregionów w 2024 roku',
          subtitle='mediana wieku matki, udział kobiet w wieku 15--49 lat') +
  theme( axis.text.x = element_text(size = 4.5),
         axis.text.y = element_text(size = 4.5)) +
  ylab("TFR") + xlab('Podregion') +
  geom_text(aes(label = medianAge), hjust = 1.3, size = 2, color='white') + 
  geom_text(aes(label = sprintf("%.1f", p * 100 )), hjust = -0.5, size = 2, 
            color='red', alpha=.4) + 
  labs(caption = "Źródło: GUS/BDL; podgrupy P2346, P2167, P2137") +
  coord_flip()

p2
ggsave(plot=p2, file='tfr_subregion.png', height = 8)

p3 <- d4 |>
  filter (year == 2024 | year == 2014 | year == 2004) |>
  ggplot(aes(x = year, y = tfr)) +
  ylab("TFR") + xlab('rok') +
  ggtitle('Współczynnik TFR wg podregionów w wybranych latach') +
  geom_jitter(width = 0.07, size = 1, alpha = 0.7) +
  labs(caption = "Źródło: GUS/BDL; podgrupy P2346, P2167, P2137")

p3


###
r_names <- c('siedlecki', 'gdański', 'jeleniogórski', 'm. poznań', 
             'm. wrocław', 'wałbrzyski', 'trójmiejski', 'nyski')
d_siedlce <- d2 |> filter (year == 2024 &  name %in% r_names )

## long format
d_siedlece_l <- d_siedlce %>%
  pivot_longer(
    cols = gwm15_p:gwm50_p,
    names_to = "zmienna",
    values_to = "wartosc"
  ) |> as.data.frame()
str(d_siedlece_l)

p_siedlece_1 <- d_siedlece_l |>
  ggplot(aes(x = zmienna, y = wartosc)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = NULL, y = "Wartość") +
  facet_wrap(~ as.factor(name), scales = "free_y") 

p_siedlece_1

d2024 <- d2 %>% filter (year == 2024 ) |>
  select(name, p, tfr) |>
  as.data.frame()
  #filter (tfr > 1.3 | tfr < 1)


hej_hop <- d2024 %>% select(tfr, p) 
str(hej_hop)
base::summary(hej_hop)

p_tfr_vs_p <- d2024 %>%
  filter (year == 2024 & (tfr > 1.2 | tfr < 1))  |>
  #filter (year == 2024 )  |>
ggplot(aes(x = tfr, y = p)) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "tfr",
    y = "p"
  )

p_tfr_vs_p



m1 <- lm(tfr ~ p, data=d2024)
summary(m1)


##
  
## sprawdzenie
test <- d2 %>%
  filter (id == '011212000000' & year == 2004) |>
  pivot_longer(
    cols = starts_with("wm"),  # wszystkie kolumny zaczynające się na "wm"
    names_to = "wm",           # nowa kolumna z nazwami kolumn
    values_to = "freq"         # nowa kolumna z wartościami liczebności
  ) |>
  mutate(cum_freq = cumsum(freq), owh = ogolem_wiek / 2)
  
##

##

p20 <- d4 |> 
  filter (year == 2024) |>
  ggplot(aes(x=reorder(name, medianAge), y=medianAge)) +
  geom_bar(fill='skyblue', stat="identity") +
  ggtitle("MedianAge") +
  theme( axis.text.x = element_text(size = 4),
         axis.text.y = element_text(size = 4)) +
  coord_flip()

p20

p20r <- d4 |> 
  filter (year == 2024) |>
  ggplot(aes(x=reorder(name, ogolem_wiek), y=ogolem_wiek)) +
  geom_bar(fill='skyblue', stat="identity") +
  ggtitle("Liczba urodzeń") +
  ylab("Podregion") + xlab('Liczba urodzeń') +
  theme( axis.text.x = element_text(size = 4),
         axis.text.y = element_text(size = 4)) +
  coord_flip()

p20r
ggsave(plot=p20r, file='liczba_urodzen_subregion.png', height = 6)

p21 <- d4 |> 
  filter (year == 2024) |>
  ggplot(aes(y=tfr, x=as.factor(medianAge) )) +
  geom_boxplot() +
  ylab("Wsp. dzietności") +
  ggtitle("Wsp. dzietności w podregionach wg mediany wieku matki") +
  xlab('Mediana wieku')
p21

d4s <- d4 |> 
  filter (year == 2024) |>
  group_by(medianAge) |>
  summarise(n = n(), tfr = mean(tfr))

###

p1 <- d4 |> 
  filter (year == 2024 | year == 2014 | year == 2004) |>
  ggplot(aes(y=tfr, x=as.factor(year) )) +
  geom_boxplot() +
  ylab("#") +
  ggtitle("Wsp. dzietności w podregionach w wybranych latach") +
  xlab('')
p1
##
##

d1 <- d2 |> filter ( year == 2024)

p2 <- d4 |> 
  filter (year == 2024 ) |>
  ggplot(aes(x=reorder(name, tfr), y=tfr)) +
  geom_bar(fill='navyblue', stat="identity") +
  ggtitle('Współczynnik TFR wg podregionów w 2024 roku',
          subtitle='mediana wieku matki, udział kobiet w wieku 15--49 lat') +
  theme( axis.text.x = element_text(size = 4.5),
         axis.text.y = element_text(size = 4.5)) +
  ylab("TFR") + xlab('Podregion') +
  geom_text(aes(label = medianAge), hjust = 1.3, size = 2, color='white') + 
  geom_text(aes(label = sprintf("%.1f", p * 100 )), hjust = -0.5, size = 2, color='red') + 
  labs(caption = "Źródło: GUS/BDL; podgrupy P2346, P2167, P2137") +
  coord_flip()

p2
ggsave(plot=p2, file='tfr_subregion.png', height = 8)

p3 <- d4 |>
  filter (year == 2024 | year == 2014 | year == 2004) |>
  ggplot(aes(x = year, y = tfr)) +
  geom_jitter(width = 0.07, size = 1, alpha = 0.7)
p3
