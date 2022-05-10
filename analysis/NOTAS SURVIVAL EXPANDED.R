
# 1 Seleccionar qué población quería
# 2 Transfroma


#' Ahora
#'
#' MEGAbase de datos
#' * Ltables para todas las personas (sexo, año, estado) (están en LONG)
#' * Tener estas Ltables en WIDE
#' * data_table ltable wide
#'



library(demogmx)
library(dplyr)

# incluir la tasa de mortalidad como una columna extra (death_rate)
df_mortrate_state_age_sex


df_drate <- df_mortrate_state_age_sex %>%
  mutate(death_rate = deaths / population) %>%
  filter(sex == "Total")


write.csv(x = df_drate, file = "data/df_drate.csv")

# el df está en long.
#'
#' Generar una nueva bdd que tiene la death rate
#' 1970, aguas, para cada año de edad, Su ltable se le pegue en formato wide
#'
#'
#' Si agarro a alguien de 4 a;os, las primeras 4 columnas (0-3) NO TIENEN NA, tienen cero
#'
#'
#'
#' Para cada fila sumar los valores
#' Cada renglon del 0 al 100 entre la suma de la fila
#'
#' cada elemento del renglón dividirlo entre la suma de todos los valores
#' (para obtener el death rate proporcional en relación a todas las edades)
#'
#'
#' Checar lifetables
#' La muerte en adelante es condicional a no haber muerto antes.
#' (es lo mismom que condicionar que no morir hacia delante)







#' 1
#' Transponer la

# F(a) función de años is the CDF of age to death
#

# visuaizar la CDF, que representa la mortalidad de las personas
plot(df_drate_x$v_Ft)


df_drate_x <- df_drate %>%
  filter(year == 1970,
         state == "Aguascalientes") %>%
  mutate(death_prob_cum = 1 - exp(-cumsum(death_rate)),
         death_prob = c(death_prob_cum[1], diff(death_prob_cum)),
         death_prob_alt = diff(c(0, death_prob_cum))) %>%
  group_by(year, state, sex)


plot(df_drate_x$death_prob_cum) # CDF
plot(df_drate_x$death_prob)     # PDF


v_ages <- 0:109
v_ages_expand <- rep(v_ages, each = length(v_ages))
df_upper_tri <- as.data.frame(upper.tri(diag(length(v_ages)), diag = TRUE)*1)




df_death_prob_survival
#' guardar aparte
#' broken stick
#'
#' Libro de Lee capítulo de survial para checar mates de líneas 70 y 70
#'




# Calcular la CDF
v_Ft <- 1 - exp(-cumsum(death_rate))
# Calcular la PDF
# dos casos, agregar
v_ft <- c(v_Ft[1], diff(v_Ft))
v_ft <- diff(c(0, v_Ft))




# no olvidar NA's
# Si hay NA'S poner cero
df_mortrate <- df_mortrate_state_age_sex %>%
  mutate(death_rate = deaths/population) %>%
  group_by(year, state, sex) %>%
  mutate(id = cur_group_id()) %>%
  mutate(death_prob_cum = 1 - exp(-cumsum(death_rate))) %>%
  mutate(death_prob = c(death_prob_cum[1], diff(death_prob_cum))) %>%
  arrange(year, state, sex) # %>% ungroup()



