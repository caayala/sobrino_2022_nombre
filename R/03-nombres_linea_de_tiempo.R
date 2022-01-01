library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(guaguas)

nom_interes <- c('^Cristi(a|á)n$', '^Andr(é|e)s$', '^Javier$', '^Arturo$', 
                 '^Catalina$', '^Josefina$', '^Leonor$')


df_name <- guaguas::guaguas %>% 
  filter(str_detect(nombre, pattern = str_c(nom_interes, collapse = '|')))

# Normalizar nombres
df_name$nombre <- stringi::stri_trans_general(df_name$nombre, 'Latin-ASCII')

head(df_name)

df_name <- df_name %>% 
  count(nombre, anio, wt = n)
  
df_name_full <- df_name %>% 
  tidyr::expand(nombre, anio)

df_name_full <- left_join(df_name_full,
                          df_name, 
                          by = c('nombre', 'anio')) %>% 
  replace_na(replace = list(n = 0))

df_name_full %>% 
  ggplot(aes(x = anio, y = n)) +
  geom_line(aes(colour = nombre, group = nombre)) +
  geom_text(data = ~filter(., anio == 2020),
            aes(label = nombre,
                colour = nombre),
            hjust = 0) +
  guides(colour = 'none') +
  coord_cartesian(clip = 'off')

df_name_full %>% 
  tidyr::pivot_wider(names_from = anio, values_from = n)
