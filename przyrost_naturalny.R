library("bdl")
library("tidyverse")
##
options(bdl.api_private_key ='????')

### Przyrost naturalny, tj. różnica między urodzeniami a zgonami
### Ogółem Mężczyźni Kobiety
p.vars <- c('68', '69', '70',
    ## ludność ogółem: O M K
            '72305', '72300', '72295'
            )
z0 <- get_data_by_variable(p.vars, unitParentId=NULL, unitLevel=5)

z2024 <- z0 |> filter (year == 2024) |>
  mutate (p1 = val_68 / val_72305 * 1000) |>
  select (name, val_68, val_72305, p1)

p3 <- z2024 |>
  ggplot(aes(x = "", y = p1)) +
  ylab("") + xlab('rok') +
  ggtitle('przyrost naturalny na 1000 ludności') +
  geom_jitter(width = 0.15, size = 1, alpha = 0.4) +
  labs(caption = "Źródło: GUS/BDL; podgrupy P1873 oraz P2137")

p3

z2024_2 <- z2024 |>
  mutate (p1 = val_68 / val_72305 * 1000)

z0_PL <- get_data_by_variable(p.vars, unitParentId=NULL, unitLevel=0)
z2024_PL <- z0_PL # |> filter (year == 2024)
z2024_2_PL <- z2024_PL |>
  mutate (p1 = val_68 / val_72305 * 1000) |>
  select (name, val_68, val_72305, p1)

