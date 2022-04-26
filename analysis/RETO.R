
#' Dos años
#' Dos sexos
#' Todas las edades



library(demogmx)


head(df_deaths)
# Crear
df_deaths <- get_deaths(v_state = "National",
                        v_year = c(2010, 2011),
                        v_sex = c("Male", "Female"),
                        v_age = seq(0, 109),
                        age_groups = FALSE)

# Queremos que regrese las death probabilities (PDF)
df_deaths <- df_deaths %>%
  mutate(death_prob = 1 - exp(-death_rate)) %>%
  group_by(year, state, sex) %>%
  mutate(death_prob_cum = cumsum(death_prob)) %>%
  ungroup()

age <- 15
v_prob_1 <- rep(x = 0, times = age)
v_prob_2 <- get_deaths(v_state = "National",
                       v_year = 2005,
                       v_sex = "Male",
                       v_age = seq(age, 109),
                       age_groups = FALSE) %>%
  mutate(death_prob = 1 - exp(-death_rate)) %>%
  group_by(year, state, sex) %>%
  mutate(death_prob_cum = cumsum(death_prob)) %>%
  ungroup() %>%
  select(death_prob) %>%
  t() %>%
  as.numeric()


v_prob <- c(v_prob_1, v_prob_2)

length(v_prob)



get_col <- function(year, state, sex, age) {

  v_prob_1 <- rep(x = 0, times = age)
  v_prob_2 <- get_deaths(v_state = state,
                         v_year = year,
                         v_sex = sex,
                         v_age = seq(age, 109),
                         age_groups = FALSE) %>%
    mutate(death_prob = 1 - exp(-death_rate)) %>%
    group_by(year, state, sex) %>%
    mutate(death_prob_cum = cumsum(death_prob)) %>%
    ungroup() %>%
    select(death_prob) %>%
    t() %>%
    as.numeric()

  v_prob <- c(v_prob_1, v_prob_2)


  return(v_prob)
}

aa <- get_col(year = 2005, state = "National", sex = "Male", age = 15)
length(aa)


#' Tenemos su tasa de mortalidad
#'
#' me dan una matriz que tiene la edad, el sexo, y el año de individuos
#'
# df_example <- data.frame(year = c(2010, 2010, 2011, 2011, 2010),
#                          sex  = c("Male", "Female", "Male", "Male", "Female"),
#                          age  = c(0, 10, 15, 26, 37))

n_pop <- 100
df_example <- data.frame(year = sample(x = c(1990:2010), size = n_pop, replace = TRUE),
                         sex  = sample(x = c("Female", "Male"), size = n_pop, replace = TRUE),
                         age  = sample(x = c(0:109), size = n_pop, replace = TRUE))


filtrar_df_1 <- function(year, sex, age) {

  df_example[(df_example$year == year) &
                     (df_example$sex == sex) &
                     (df_example$age == age)
                   , ]
}


filtrar_df_1(year = 2003, sex = "Male", age = 7)

microbenchmark::microbenchmark(filtrar_df_1(year = 2003, sex = "Male", age = 7),
                               filtrar_df(Year = 2003, Sex = "Male", Age = 7),
                               times = 1000)

# n_pop <- 10000000
# df_demog <- data.frame(id = 1:n_pop,
#                        sex = sample(x = c("Male", "Female"),
#                                     size = n_pop,
#                                     prob = c(0.5, 0.5),
#                                     replace = TRUE),
#                        Year = sample(x = c(2010, 2011, 2012),
#                                      size = n_pop,
#                                      prob = c(0.4, 0.2, 0.4),
#                                      replace = TRUE),
#                        Age = sample(x = c(0:109),
#                                     size = n_pop,
#                                     prob = rep(1, 110),
#                                     replace = TRUE))

# Discrete event simulation

#' Reto:
#' Que se le generen columnas a la derecha para que cada columna tenga las
#' death rates desde 0 hasta 109 años de edad
#'
#' year    sex age
#' 2010   Male   0
#'
#' Que se genere un vector y que para esas personas salgan todas las
#'

#' Pasos
#' 1 Crear la base de donde sacaré directamente los datos
#' 2 Crear las columnas con los datos específicos necesarios
#' 3 Crear la función para filtrar los datos
#' 4 Usar la función para filtrar los datos
#' 5 Vectorizar la función
#' 6 (usar data.table? dtplyr?)


library(data.table)

df_mortrate <- df_mortrate_state_age_sex %>%
  mutate(death_rate = deaths/population) %>%
  mutate(death_prob = 1 - exp(-death_rate)) %>%
  group_by(year, state, sex) %>%
  mutate(death_prob_cum = cumsum(death_prob)) %>%
  ungroup()
  # as.data.table()

n_pop <- 100
df_example <- data.frame(year = sample(x = c(1990:2010), size = n_pop, replace = TRUE),
                         sex  = sample(x = c("Female", "Male"), size = n_pop, replace = TRUE),
                         age  = sample(x = c(0:109), size = n_pop, replace = TRUE))


get_col <- function(year, state, sex, age) {

  v_prob_1 <- rep(x = 0, times = age)

  df_prob <- df_mortrate[ (df_mortrate$year == year) &
                           (df_mortrate$state == state) &
                           (df_mortrate$sex == sex) &
                           (df_mortrate$age >= age)
                         , ]

  v_prob_2 <- as.numeric(t(df_prob$death_prob))
  v_prob <- c(v_prob_1, v_prob_2)

  return(v_prob)
}

df_mortrate %>%
  filter(year == 2005,
         state == "National",
         sex == "Male",
         age >= 15) %>%
  select(death_prob) %>%
  t() %>%
  as.numeric()

aa <- get_col(year = 2005, state = "National", sex = "Male", age = 15)


apply(X = df_example, MARGIN = 1,
      FUN = get_col,
      year = df_example$year,
      state = "National",
      sex = df_example$sex,
      age = df_example$age)


# El bueno
hh <- mapply(FUN = get_col,
       year = df_example$year,
       state = rep("National", 100),
       sex = df_example$sex,
       age = df_example$age)

dimnames(hh) <- list(c(0:109),
                     paste0("Ind_", seq(100)))

paste0("hola_", seq(15))

# Edades <- renglones
# Individuos <- Columnas
dim(hh)

head(df_example)
# year    sex age
# 1 2009   Male  16
# 2 1996   Male  80
# 3 1992 Female   4
# 4 2009   Male  92
# 5 1996 Female  36
# 6 1997 Female  59



# Primer saque ------------------------------------------------------------

library(dplyr)
library(demogmx)

n_pop <- 1000
df_example <- data.frame(year = sample(x = c(1990:2010), size = n_pop, replace = TRUE),
                         sex  = sample(x = c("Female", "Male"), size = n_pop, replace = TRUE),
                         age  = sample(x = c(0:109), size = n_pop, replace = TRUE))

df_mortrate <- df_mortrate_state_age_sex %>%
  mutate(death_rate = deaths/population) %>%
  mutate(death_prob = 1 - exp(-death_rate)) %>%
  group_by(year, state, sex) %>%
  mutate(death_prob_cum = cumsum(death_prob)) %>%
  ungroup()


get_col <- function(year, state, sex, age) {
  v_prob_1 <- rep(x = 0, times = age)

  v_prob_2 <- t(df_mortrate[(df_mortrate$year == year) &
                              (df_mortrate$state == state) &
                              (df_mortrate$sex == sex) &
                              (df_mortrate$age >= age),
                            "death_prob"])

  v_prob <- c(v_prob_1, v_prob_2)

  return(v_prob)
}

aa <- Sys.time()
hh <- mapply(FUN = get_col,
             year = df_example$year,
             state = rep("National", n_pop),
             sex = df_example$sex,
             age = df_example$age)
bb <- Sys.time()


# Data table --------------------------------------------------------------

library(data.table)
library(dplyr)
library(demogmx)

n_pop <- 1000
df_example <- data.frame(year = sample(x = c(1990:2010), size = n_pop, replace = TRUE),
                         sex  = sample(x = c("Female", "Male"), size = n_pop, replace = TRUE),
                         age  = sample(x = c(0:109), size = n_pop, replace = TRUE))



#' df_mortrate tiene NA's en death_prob y death_prob_cum
#' Esto pasa por varias causas:
#' - hay más muertes que población
#' -
df_mortrate <- df_mortrate_state_age_sex %>%
  mutate(death_rate = deaths/population) %>%
  mutate(death_prob = 1 - exp(-death_rate)) %>%
  group_by(year, state, sex) %>%
  mutate(death_prob_cum = 1 - exp(-cumsum(death_rate))) %>%
  ungroup()
# Ver el número de NA's
colSums(is.na(df_mortrate))

# df_mortrate %>%
#   filter(is.na(death_prob_cum)) %>%
#   View()

# Convert to data.table
dt_mortrate <- as.data.table(x = df_mortrate)
# Set keys
setkey(x = dt_mortrate, year, state, sex, age)


get_col_dt <- function(Year, State, Sex, Age) {
  # IMPORTANT:
  # The name of the fun parameters do not have to match the colnames of
  # dt_mortrate, otherwise the subsetting will not work

  v_prob_1 <- rep(x = 0, times = Age)
  v_prob_2 <- dt_mortrate[.(Year, State, Sex, c(Age:109)), death_prob]

  v_prob <- c(v_prob_1, v_prob_2)

  return(v_prob)
}

# # Test fun
# get_col_dt(Year = 2006, State = "National", Sex = "Male", Age = 8)

# Use the fun in a vectorized way
aa <- Sys.time()
hh <- mapply(FUN = get_col_dt,
             Year = df_example$year,
             State = rep("National", times = n_pop),
             Sex = df_example$sex,
             Age = df_example$age)
bb <- Sys.time()
bb-aa


View(hh)

dim(hh)

sum(colSums(is.na(hh)))

m_prueba <- hh[, 1:10]
head(m_prueba, 20)
df_example$age[1:20]


df_example[2,]
# year    sex age
# 2 1991 Female  14
