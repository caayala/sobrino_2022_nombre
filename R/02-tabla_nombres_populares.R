library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(guaguas)

rango_anios_interes <- 2015:2020

df_name <- guaguas::guaguas_frecuentes %>% 
  filter(sexo == 'M')


table(df_name$anio)

df_name_resiente <- df_name %>% 
  filter(anio %in% rango_anios_interes)

# Lista de total de nombres en el rango de años de interés-
df_name_resiente_tot <- df_name_resiente %>% 
  count(nombre, wt = n, name = 'n_total')


# Tabla por nombre wide ----

df_name_resiente_wide <- df_name_resiente %>% 
  select(!proporcion) %>% 
  pivot_wider(names_from = anio, values_from = n)

# Agregar datos totales
df_name_resiente_wide_tot <- left_join(df_name_resiente_wide, 
                                       df_name_resiente_tot,
                                       by = 'nombre')

# Ordenar de más a menos popular.
df_name_resiente_wide_tot <- df_name_resiente_wide_tot %>% 
  arrange(-n_total)


# Output ----

df_name_resiente_wide_tot %>% 
  readr::write_excel_csv2('output/02-df_name_resiente_wide_tot.csv')


# Distribución de nombres ----

# Factor con orden de nombres según orden en que aparecen los chr en la data.frame.
df_name_resiente_wide_tot <- df_name_resiente_wide_tot %>% 
  mutate(nombre = as_factor(nombre))

df_name_resiente_wide_tot %>% 
  ggplot(aes(x = nombre, y = n_total)) +
  geom_col() +
  guides(x = guide_axis(angle = 90, check.overlap = TRUE, n.dodge = 2))
  
