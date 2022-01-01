library(dplyr)
library(ggplot2)
library(stringr)
library(guaguas)

nom_posibles <- c('Samuel', 'Damián', 'Emilio',
                  'Aníbal', 'Domingo', 'Antonio',
                  'Arturo')

# 'Lucas'

df_name <- guaguas::guaguas_frecuentes %>% 
  filter(sexo == 'M') %>% 
  arrange(-n)
  
head(df_name)
range(df_name$anio)

df_name %>% 
  filter(nombre %in% nom_posibles) %>% 
  ggplot(aes(x = anio, y = n)) +
  geom_line(aes(colour = nombre)) +
  geom_text(data = ~filter(., anio == 2020),
            aes(label = nombre,
                colour = nombre),
            hjust = 0) +
  guides(colour = 'none') +
  coord_cartesian(clip = 'off')

df_name %>% 
  filter(anio >= 1937, anio <= 1960) %>% 
  group_by(anio) %>% 
  slice_head(n = 10) %>% 
  mutate(pos = 1:n(), .after = anio) %>% 
  as.data.frame()


df_name %>% 
  filter(anio >= 1937, anio <= 1960) %>% 
  group_by(anio) %>% 
  slice_head(n = 10) %>% 
  mutate(pos = 1:n(), .after = anio) %>% 
  as.data.frame()

df_name %>% 
  group_by(anio) %>% 
  mutate(mediana = median(n),
         diff = n - mediana) %>% 
  arrange(anio)
  
df_name %>% 
  filter(anio >= 2000, anio <= 2020) %>% 
  group_by(anio) %>% 
  arrange(n) %>% 
  mutate(n_corte = quantile(n, .65),
         diff = n - n_corte) %>% 
  filter(n > n_corte) %>% 
  slice_head(n = 8) %>% 
  as.data.frame()

quantile(0:100, .6)

