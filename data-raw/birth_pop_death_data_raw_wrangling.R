#*****************************************************************************#
# This script loads and formats the data from CONAPO's "Proyecciones de la    #
# Población de México y de las Entidades Federativas, 2016-2050" Downloaded   #
# from https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050 #
# accessed on 02/14/2020                                                      #
#                                                                             #
# Depends on:                                                                 #
# Author: Fernando Alarid-Escudero                                            #
# Author: Hirvin Azael Diaz Zepeda                                            #
# Author: David Garibay-Treviño                                               #
# E-mail: fernando.alarid@cide.edu                                            #
# E-mail: david.garibay@cide.edu                                              #
#*****************************************************************************#

rm(list = ls()) # to clean the workspace

# 01 Load packages and functions ---------------------------------------
## 01.01 Load packages -------------------------------------------------
library(data.table)
library(dplyr)
library(readxl)
library(tidyr)

## 01.02 Load functions ------------------------------------------------
# no functions required

## 01.02 Load required data ---------------------------------------------
# Vector of state names
v_names_states <- c("Aguascalientes", "Baja California",
                    "Baja California Sur", "Campeche", "Coahuila",
                    "Colima", "Chiapas", "Chihuahua",
                    "Mexico City", "Durango", "Guanajuato",
                    "Guerrero", "Hidalgo", "Jalisco",
                    "State of Mexico", "Michoacan", "Morelos",
                    "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                    "Queretaro", "Quintana Roo", "San Luis Potosi",
                    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas",
                    "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas",
                    "National")

# 02 Population data ---------------------------------------------------
## 02.01 Load data -----------------------------------------------------

### Load data with Spanish encoding
df_pop_state_age_sex <- data.table::fread(input = "data-raw/pob_ini_proyecciones.csv",
                                          encoding = "Latin-1")

## 02.02 Data manipulation ------------------------------------------------
### Rename variables and select obs above 1970
df_pop_state_age_sex <- df_pop_state_age_sex %>%
  rename(year = AÑO,
         state = ENTIDAD,
         age = EDAD,
         population = POBLACION) %>%
  filter(year >= 1970)

### Rename and recode sex
df_pop_state_age_sex <- df_pop_state_age_sex %>%
  mutate(SEXO = factor(SEXO)) %>%
  rename(sex = SEXO)

levels(df_pop_state_age_sex$sex)[levels(df_pop_state_age_sex$sex) == "Hombres"] = "Male"
levels(df_pop_state_age_sex$sex)[levels(df_pop_state_age_sex$sex) == "Mujeres"] = "Female"

### Recode States
df_pop_state_age_sex <- df_pop_state_age_sex %>%
  mutate(state = replace(state, state == "Aguascalientes", "01"),
         state = replace(state, state == "Baja California", "02"),
         state = replace(state, state == "Baja California Sur", "03"),
         state = replace(state, state == "Campeche", "04"),
         state = replace(state, state == "Coahuila", "05"),
         state = replace(state, state == "Colima", "06"),
         state = replace(state, state == "Chiapas", "07"),
         state = replace(state, state == "Chihuahua", "08"),
         state = replace(state, state == "Ciudad de México", "09"),
         state = replace(state, state == "Durango", "10"),
         state = replace(state, state == "Guanajuato", "11"),
         state = replace(state, state == "Guerrero", "12"),
         state = replace(state, state == "Hidalgo", "13"),
         state = replace(state, state == "Jalisco", "14"),
         state = replace(state, state == "México", "15"),
         state = replace(state, state == "Michoacán", "16"),
         state = replace(state, state == "Morelos", "17"),
         state = replace(state, state == "Nayarit", "18"),
         state = replace(state, state == "Nuevo León", "19"),
         state = replace(state, state == "Oaxaca", "20"),
         state = replace(state, state == "Puebla", "21"),
         state = replace(state, state == "Querétaro", "22"),
         state = replace(state, state == "Quintana Roo", "23"),
         state = replace(state, state == "San Luis Potosí", "24"),
         state = replace(state, state == "Sinaloa", "25"),
         state = replace(state, state == "Sonora", "26"),
         state = replace(state, state == "Tabasco", "27"),
         state = replace(state, state == "Tamaulipas", "28"),
         state = replace(state, state == "Tlaxcala", "29"),
         state = replace(state, state == "Veracruz", "30"),
         state = replace(state, state == "Yucatán", "31"),
         state = replace(state, state == "Zacatecas", "32"),
         state = replace(state, state == "República Mexicana", "33"))

df_pop_state_age_sex <- df_pop_state_age_sex %>%
  mutate(state = as.factor(state)) %>%
  select(-RENGLON)

## Label states
levels(df_pop_state_age_sex$state) <- v_names_states

#### Adding MCMA in year 2020 AFTER compute 01_data_wrangling_states,
#### 02_demographic_rates_states, and 01_01_data_wrangling_counties
#df_pop_state_age_sex <- bind_rows(df_pop_state_age_sex,
#                                  pop_MCMA) %>%
#  arrange(year,state,sex,age)

### Compute general estimates NOT by sex
df_pop_state_age <- df_pop_state_age_sex %>%
  group_by(year, state, CVE_GEO, age) %>%
  summarise(population = sum(population)) %>%
  ungroup()

### Compute general estimates NOT by age
df_pop_state <- df_pop_state_age %>%
  group_by(year, state, CVE_GEO) %>%
  summarise(population = sum(population)) %>%
  ungroup()

## 02.03 Save population projections data.frames --------------------------

### Save Population Projections data.frames
# usethis::use_data(df_pop_state_age_sex, overwrite = TRUE)

# write.csv(df_pop_state_age_sex, "data/df_pop_state_age_sex.csv")



# 03 Birth projections data -----------------------------------------------
## 03.01 Load data --------------------------------------------------------

### Load data with Spanish encoding
df_birth_state_agegrp <- data.table::fread(input = "data-raw/tef_nac_proyecciones.csv",
                                           encoding ="Latin-1")


## 03.02 Data manipulation ------------------------------------------------
### Rename variables
df_birth_state_agegrp <- df_birth_state_agegrp %>%
  rename(year = AÑO,
         state = ENTIDAD,
         age_group = GPO_EDAD,
         births = NACIMIENTOS,
         ferty_rate = `TASAS (1)`) %>%
  arrange(year,state) %>%
  select(c(2:7)) %>%
  filter(year>= 1970)

### Recode States
df_birth_state_agegrp <- df_birth_state_agegrp %>%
  mutate(state = replace(state, state == "Aguascalientes", "01"),
         state = replace(state, state == "Baja California", "02"),
         state = replace(state, state == "Baja California Sur", "03"),
         state = replace(state, state == "Campeche", "04"),
         state = replace(state, state == "Coahuila", "05"),
         state = replace(state, state == "Colima", "06"),
         state = replace(state, state == "Chiapas", "07"),
         state = replace(state, state == "Chihuahua", "08"),
         state = replace(state, state == "Ciudad de México", "09"),
         state = replace(state, state == "Durango", "10"),
         state = replace(state, state == "Guanajuato", "11"),
         state = replace(state, state == "Guerrero", "12"),
         state = replace(state, state == "Hidalgo", "13"),
         state = replace(state, state == "Jalisco", "14"),
         state = replace(state, state == "México", "15"),
         state = replace(state, state == "Michoacán", "16"),
         state = replace(state, state == "Morelos", "17"),
         state = replace(state, state == "Nayarit", "18"),
         state = replace(state, state == "Nuevo León", "19"),
         state = replace(state, state == "Oaxaca", "20"),
         state = replace(state, state == "Puebla", "21"),
         state = replace(state, state == "Querétaro", "22"),
         state = replace(state, state == "Quintana Roo", "23"),
         state = replace(state, state == "San Luis Potosí", "24"),
         state = replace(state, state == "Sinaloa", "25"),
         state = replace(state, state == "Sonora", "26"),
         state = replace(state, state == "Tabasco", "27"),
         state = replace(state, state == "Tamaulipas", "28"),
         state = replace(state, state == "Tlaxcala", "29"),
         state = replace(state, state == "Veracruz", "30"),
         state = replace(state, state == "Yucatán", "31"),
         state = replace(state, state == "Zacatecas", "32"),
         state = replace(state, state == "República Mexicana", "33"))

df_birth_state_agegrp <- df_birth_state_agegrp %>%
  mutate(state = as.factor(state))

levels(df_birth_state_agegrp$state) <- v_names_states

# ### Compute general estimates by age
# df_birth_state_age <- df_birth_state_agegrp %>%
#   rename(births_grouped = "births") %>%
#   mutate(births = births_grouped/5) %>%
#   rename(frate_grouped = "ferty_rate") %>%
#   mutate(ferty_rate = frate_grouped/5) %>%
#   tidyr::uncount(5) %>%
#   mutate(age = rep(15:49,2673)) %>%
#   select(year,state,CVE_GEO,age,births_grouped,births,ferty_rate)

### Compute general estimates NOT by age
df_birth_state <- df_birth_state_agegrp %>%
  group_by(year, state, CVE_GEO) %>%
  summarise(births = sum(births)) %>%
  ungroup()

## 03.03 Save birth projections data.frames -------------------------------
# usethis::use_data(df_birth_state, overwrite = TRUE)

# write.csv(df_birth_state, "data/df_birth_state.csv")


# 04 Mortality projections data ----------------------------------------
## 04.01 Load data -----------------------------------------------------

### Load data with Spanish encoding
df_mort_state_age_sex <- data.table::fread(input = "data-raw/def_edad_proyecciones_n.csv",
                                           encoding="UTF-8")

## 04.02 Data manipulation ------------------------------------------------
### Rename variables
df_mort_state_age_sex <- df_mort_state_age_sex %>%
  rename(year = AÑO,
         state = ENTIDAD,
         age = EDAD,
         deaths = DEFUNCIONES) %>%
  filter(year>= 1970) %>%
  select(c(2:7))

### Rename and recode sex
df_mort_state_age_sex <- df_mort_state_age_sex %>%
  mutate(SEXO = factor(SEXO)) %>%
  rename(sex = SEXO)

levels(df_mort_state_age_sex$sex)[levels(df_mort_state_age_sex$sex) == "Hombres"] = "Male"
levels(df_mort_state_age_sex$sex)[levels(df_mort_state_age_sex$sex) == "Mujeres"] = "Female"

### Recode States
df_mort_state_age_sex <- df_mort_state_age_sex %>%
  mutate(state = replace(state, state == "Aguascalientes", "01"),
         state = replace(state, state == "Baja California", "02"),
         state = replace(state, state == "Baja California Sur", "03"),
         state = replace(state, state == "Campeche", "04"),
         state = replace(state, state == "Coahuila", "05"),
         state = replace(state, state == "Colima", "06"),
         state = replace(state, state == "Chiapas", "07"),
         state = replace(state, state == "Chihuahua", "08"),
         state = replace(state, state == "Ciudad de México", "09"),
         state = replace(state, state == "Durango", "10"),
         state = replace(state, state == "Guanajuato", "11"),
         state = replace(state, state == "Guerrero", "12"),
         state = replace(state, state == "Hidalgo", "13"),
         state = replace(state, state == "Jalisco", "14"),
         state = replace(state, state == "México", "15"),
         state = replace(state, state == "Michoacán", "16"),
         state = replace(state, state == "Morelos", "17"),
         state = replace(state, state == "Nayarit", "18"),
         state = replace(state, state == "Nuevo León", "19"),
         state = replace(state, state == "Oaxaca", "20"),
         state = replace(state, state == "Puebla", "21"),
         state = replace(state, state == "Querétaro", "22"),
         state = replace(state, state == "Quintana Roo", "23"),
         state = replace(state, state == "San Luis Potosí", "24"),
         state = replace(state, state == "Sinaloa", "25"),
         state = replace(state, state == "Sonora", "26"),
         state = replace(state, state == "Tabasco", "27"),
         state = replace(state, state == "Tamaulipas", "28"),
         state = replace(state, state == "Tlaxcala", "29"),
         state = replace(state, state == "Veracruz", "30"),
         state = replace(state, state == "Yucatán", "31"),
         state = replace(state, state == "Zacatecas", "32"),
         state = replace(state, state == "República Mexicana", "33"))

df_mort_state_age_sex <- df_mort_state_age_sex %>%
  mutate(state = as.factor(state))

levels(df_mort_state_age_sex$state) <- v_names_states

#### Adding MCMA in year 2020 AFTER compute 01_data_wrangling_states,
#### 02_demographic_rates_states, and 01_01_data_wrangling_counties
#df_mort_state_age_sex <- bind_rows(df_mort_state_age_sex ,
#                                  deaths_MCMA) %>%
#  arrange(year,state,sex,age)

# ### Compute general estimates NOT by sex
# df_mort_state_age <- df_mort_state_age_sex %>%
#   group_by(year, state, CVE_GEO, age) %>%
#   summarise(deaths = sum(deaths))

# ### Compute general estimates NOT by age
# df_mort_state <- df_mort_state_age %>%
#   group_by(year, state, CVE_GEO) %>%
#   summarise(deaths = sum(deaths))

## 04.03 Save mortality projections data.frames -------------------------------
### Save Mortality Projections data.frames
usethis::use_data(df_mort_state_age_sex, overwrite = TRUE)

# write.csv(df_mort_state_age_sex, "data/df_mort_state_age_sex.csv")




# 05 Create mortality rate data -------------------------------------------

## 05.01 Data manipulation -------------------------------------------------
# Create Total in sex variable
## In population dataframe
df_pop_state_age_sex_1 <- df_pop_state_age_sex %>%
  group_by(year, state, CVE_GEO, age) %>%
  summarise(sex = "Total",
            population = sum(population)) %>%
  ungroup() %>%
  bind_rows(df_pop_state_age_sex) %>%
  arrange(year, state, age, sex)

## In mortality dataframe
df_mort_state_age_sex_1 <- df_mort_state_age_sex %>%
  group_by(year, state, CVE_GEO, age) %>%
  summarise(sex = "Total",
            deaths = sum(deaths)) %>%
  ungroup() %>%
  bind_rows(df_mort_state_age_sex) %>%
  arrange(year, state, age, sex)

# Join population and mortality dataframes
df_mortrate_state_age_sex <- df_pop_state_age_sex_1 %>%
  left_join(df_mort_state_age_sex_1,
            by = c("year", "state", "CVE_GEO", "age", "sex")) %>%
  filter(year <= 2050) %>%
  select(year, state, CVE_GEO, age, sex, population, deaths) %>%
  mutate(death_rate = deaths/population)

## 05.02 Save data --------------------------------------------------------
# usethis::use_data(df_mortrate_state_age_sex, overwrite = TRUE)


# 06 Create birth rate data -----------------------------------------------
## 06.01 Data manipulation -------------------------------------------------
# Obtain all the population of each state by year
df_pop_state_age_sex_2 <- df_pop_state_age_sex %>%
  group_by(year, state, CVE_GEO) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  arrange(year, state) %>%
  filter(year <= 2050)

# Join annual population and annual births dataframes
df_birth_pop_states <- df_pop_state_age_sex_2 %>%
  left_join(df_birth_state,
            by = c("year", "state", "CVE_GEO"))

## 06.02 Save data --------------------------------------------------------
# usethis::use_data(df_birth_pop_states, overwrite = TRUE)

