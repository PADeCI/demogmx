#*****************************************************************************#
# This script loads the formated data of migration projections from CONAPO    #
# and computes different  by age groups, states and sex, such as:             #
#  - Inter-state migration                                                    #
#  - International migration                                                  #
#  - Total migration                                                          #
#                                                                             #
# Author: Fernando Alarid-Escudero                                            #
# Author: David Garibay-Treviño                                               #
# E-mail: fernando.alarid@cide.edu                                            #
# E-mail: david.garibay@cide.edu                                              #
#*****************************************************************************#

rm(list = ls()) # to clean the workspace

# 01 Load packages and functions ---------------------------------------
## 01.01 Load packages -------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)

## 01.02 Load functions ------------------------------------------------
# no functions required


# 02 Migration data ----------------------------------------------------
## 02.01 Load data -----------------------------------------------------

## Inter-state migration
df_state_mig_base <- data.table::fread(input = "data-raw/mig_interestatal_quin_proyecciones.csv",
                                  encoding = "Latin-1")

## International migration
df_inter_mig_base <- data.table::fread(input = "data-raw/mig_internacional_quin_proyecciones.csv",
                                  encoding = "Latin-1")

## Label states
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

## 02.02 Rename variables and select observations above 1970 -----------
### 02.02.01 Inter-state migration -------------------------------------
df_state_mig_aux <- df_state_mig_base %>%
  rename(year       = AÑO,
         state      = ENTIDAD,
         age        = EDAD,
         sex        = SEXO,
         emigrants  = EMIGRANTES,
         immigrants = INMIGRANTES) %>%
  select(-RENGLON) %>%
  mutate(sex = factor(sex),
         sex = recode_factor(sex,
                             Hombres = "Male",
                             Mujeres = "Female"))


### 02.02.02 International migration ----------------------------------
df_inter_mig_aux <- df_inter_mig_base %>%
  rename(year       = AÑO,
         state      = ENTIDAD,
         age        = EDAD,
         sex        = SEXO,
         emigrants  = EMIGRANTES,
         immigrants = INMIGRANTES) %>%
  select(-RENGLON) %>%
  mutate(sex = factor(sex),
         sex = recode_factor(sex,
                             Hombres = "Male",
                             Mujeres = "Female")) %>%
  filter(!(year %in% c("1950-1955", "1955-1960", "1960-1965", "1965-1970"))) %>%
  mutate(age = str_replace(string = age, pattern = "--", replacement = "-"))

## 02.03 Create factor "Total" in sex variable -------------------------
### 02.03.01 Inter-state migration -------------------------------------
df_state_mig_aux2 <- df_state_mig_aux %>%
  bind_rows(df_state_mig_aux %>% # Add Total to sex variable
            group_by(year, state, age) %>%
            summarise(emigrants  = sum(emigrants),
                      immigrants = sum(immigrants),
                      CVE_GEO    = unique(CVE_GEO)) %>%
            mutate(sex = "Total")) %>%
  arrange(year, state, age, sex)

### 02.03.02 International migration -----------------------------------
df_inter_mig_aux2 <- df_inter_mig_aux %>%
  bind_rows(df_inter_mig_aux %>% # Add Total to sex variable
              group_by(year, state, age) %>%
              summarise(emigrants = sum(emigrants),
                        immigrants = sum(immigrants),
                        CVE_GEO = unique(CVE_GEO)) %>%
              mutate(sex = "Total")) %>%
  arrange(year, state, age, sex)

## 02.04 Rename elements to English in state variable ------------------
### 02.04.01 Inter-state migration -------------------------------------
## CVE_GEO codes
df_mig_states_cve <- df_state_mig_aux2 %>%
  mutate(CVE_GEO = replace(CVE_GEO, state == "Aguascalientes"     , "01"),
         CVE_GEO = replace(CVE_GEO, state == "Baja California"    , "02"),
         CVE_GEO = replace(CVE_GEO, state == "Baja California Sur", "03"),
         CVE_GEO = replace(CVE_GEO, state == "Campeche"           , "04"),
         CVE_GEO = replace(CVE_GEO, state == "Coahuila"           , "05"),
         CVE_GEO = replace(CVE_GEO, state == "Colima"             , "06"),
         CVE_GEO = replace(CVE_GEO, state == "Chiapas"            , "07"),
         CVE_GEO = replace(CVE_GEO, state == "Chihuahua"          , "08"),
         CVE_GEO = replace(CVE_GEO, state == "Ciudad de México"   , "09"),
         CVE_GEO = replace(CVE_GEO, state == "Durango"            , "10"),
         CVE_GEO = replace(CVE_GEO, state == "Guanajuato"         , "11"),
         CVE_GEO = replace(CVE_GEO, state == "Guerrero"           , "12"),
         CVE_GEO = replace(CVE_GEO, state == "Hidalgo"            , "13"),
         CVE_GEO = replace(CVE_GEO, state == "Jalisco"            , "14"),
         CVE_GEO = replace(CVE_GEO, state == "México"             , "15"),
         CVE_GEO = replace(CVE_GEO, state == "Michoacán"          , "16"),
         CVE_GEO = replace(CVE_GEO, state == "Morelos"            , "17"),
         CVE_GEO = replace(CVE_GEO, state == "Nayarit"            , "18"),
         CVE_GEO = replace(CVE_GEO, state == "Nuevo León"         , "19"),
         CVE_GEO = replace(CVE_GEO, state == "Oaxaca"             , "20"),
         CVE_GEO = replace(CVE_GEO, state == "Puebla"             , "21"),
         CVE_GEO = replace(CVE_GEO, state == "Querétaro"          , "22"),
         CVE_GEO = replace(CVE_GEO, state == "Quintana Roo"       , "23"),
         CVE_GEO = replace(CVE_GEO, state == "San Luis Potosí"    , "24"),
         CVE_GEO = replace(CVE_GEO, state == "Sinaloa"            , "25"),
         CVE_GEO = replace(CVE_GEO, state == "Sonora"             , "26"),
         CVE_GEO = replace(CVE_GEO, state == "Tabasco"            , "27"),
         CVE_GEO = replace(CVE_GEO, state == "Tamaulipas"         , "28"),
         CVE_GEO = replace(CVE_GEO, state == "Tlaxcala"           , "29"),
         CVE_GEO = replace(CVE_GEO, state == "Veracruz"           , "30"),
         CVE_GEO = replace(CVE_GEO, state == "Yucatán"            , "31"),
         CVE_GEO = replace(CVE_GEO, state == "Zacatecas"          , "32"),
         CVE_GEO = replace(CVE_GEO, state == "República Mexicana" , "00")) %>%
  mutate(CVE_GEO = as.numeric(CVE_GEO))

## state names
df_mig_states <- df_mig_states_cve %>%
  mutate(state = replace(state, state == "Aguascalientes"     , "01"),
         state = replace(state, state == "Baja California"    , "02"),
         state = replace(state, state == "Baja California Sur", "03"),
         state = replace(state, state == "Campeche"           , "04"),
         state = replace(state, state == "Coahuila"           , "05"),
         state = replace(state, state == "Colima"             , "06"),
         state = replace(state, state == "Chiapas"            , "07"),
         state = replace(state, state == "Chihuahua"          , "08"),
         state = replace(state, state == "Ciudad de México"   , "09"),
         state = replace(state, state == "Durango"            , "10"),
         state = replace(state, state == "Guanajuato"         , "11"),
         state = replace(state, state == "Guerrero"           , "12"),
         state = replace(state, state == "Hidalgo"            , "13"),
         state = replace(state, state == "Jalisco"            , "14"),
         state = replace(state, state == "México"             , "15"),
         state = replace(state, state == "Michoacán"          , "16"),
         state = replace(state, state == "Morelos"            , "17"),
         state = replace(state, state == "Nayarit"            , "18"),
         state = replace(state, state == "Nuevo León"         , "19"),
         state = replace(state, state == "Oaxaca"             , "20"),
         state = replace(state, state == "Puebla"             , "21"),
         state = replace(state, state == "Querétaro"          , "22"),
         state = replace(state, state == "Quintana Roo"       , "23"),
         state = replace(state, state == "San Luis Potosí"    , "24"),
         state = replace(state, state == "Sinaloa"            , "25"),
         state = replace(state, state == "Sonora"             , "26"),
         state = replace(state, state == "Tabasco"            , "27"),
         state = replace(state, state == "Tamaulipas"         , "28"),
         state = replace(state, state == "Tlaxcala"           , "29"),
         state = replace(state, state == "Veracruz"           , "30"),
         state = replace(state, state == "Yucatán"            , "31"),
         state = replace(state, state == "Zacatecas"          , "32"),
         state = replace(state, state == "República Mexicana" , "33")) %>%
  mutate(state = as.factor(state),
         sex   = as.factor(sex))

# Assign English level names in state variable
levels(df_mig_states$state) <- v_names_states

### 02.04.02 International migration --------------------------------------
## CVE_GEO codes
df_mig_inter_cve <- df_inter_mig_aux2 %>%
  mutate(CVE_GEO = replace(CVE_GEO, state == "Aguascalientes"     , "01"),
         CVE_GEO = replace(CVE_GEO, state == "Baja California"    , "02"),
         CVE_GEO = replace(CVE_GEO, state == "Baja California Sur", "03"),
         CVE_GEO = replace(CVE_GEO, state == "Campeche"           , "04"),
         CVE_GEO = replace(CVE_GEO, state == "Coahuila"           , "05"),
         CVE_GEO = replace(CVE_GEO, state == "Colima"             , "06"),
         CVE_GEO = replace(CVE_GEO, state == "Chiapas"            , "07"),
         CVE_GEO = replace(CVE_GEO, state == "Chihuahua"          , "08"),
         CVE_GEO = replace(CVE_GEO, state == "Ciudad de México"   , "09"),
         CVE_GEO = replace(CVE_GEO, state == "Durango"            , "10"),
         CVE_GEO = replace(CVE_GEO, state == "Guanajuato"         , "11"),
         CVE_GEO = replace(CVE_GEO, state == "Guerrero"           , "12"),
         CVE_GEO = replace(CVE_GEO, state == "Hidalgo"            , "13"),
         CVE_GEO = replace(CVE_GEO, state == "Jalisco"            , "14"),
         CVE_GEO = replace(CVE_GEO, state == "México"             , "15"),
         CVE_GEO = replace(CVE_GEO, state == "Michoacán"          , "16"),
         CVE_GEO = replace(CVE_GEO, state == "Morelos"            , "17"),
         CVE_GEO = replace(CVE_GEO, state == "Nayarit"            , "18"),
         CVE_GEO = replace(CVE_GEO, state == "Nuevo León"         , "19"),
         CVE_GEO = replace(CVE_GEO, state == "Oaxaca"             , "20"),
         CVE_GEO = replace(CVE_GEO, state == "Puebla"             , "21"),
         CVE_GEO = replace(CVE_GEO, state == "Querétaro"          , "22"),
         CVE_GEO = replace(CVE_GEO, state == "Quintana Roo"       , "23"),
         CVE_GEO = replace(CVE_GEO, state == "San Luis Potosí"    , "24"),
         CVE_GEO = replace(CVE_GEO, state == "Sinaloa"            , "25"),
         CVE_GEO = replace(CVE_GEO, state == "Sonora"             , "26"),
         CVE_GEO = replace(CVE_GEO, state == "Tabasco"            , "27"),
         CVE_GEO = replace(CVE_GEO, state == "Tamaulipas"         , "28"),
         CVE_GEO = replace(CVE_GEO, state == "Tlaxcala"           , "29"),
         CVE_GEO = replace(CVE_GEO, state == "Veracruz"           , "30"),
         CVE_GEO = replace(CVE_GEO, state == "Yucatán"            , "31"),
         CVE_GEO = replace(CVE_GEO, state == "Zacatecas"          , "32"),
         CVE_GEO = replace(CVE_GEO, state == "República Mexicana" , "00")) %>%
  mutate(CVE_GEO = as.numeric(CVE_GEO))

## state names
df_mig_inter <- df_mig_inter_cve %>%
  mutate(state = replace(state, state == "Aguascalientes"     , "01"),
         state = replace(state, state == "Baja California"    , "02"),
         state = replace(state, state == "Baja California Sur", "03"),
         state = replace(state, state == "Campeche"           , "04"),
         state = replace(state, state == "Coahuila"           , "05"),
         state = replace(state, state == "Colima"             , "06"),
         state = replace(state, state == "Chiapas"            , "07"),
         state = replace(state, state == "Chihuahua"          , "08"),
         state = replace(state, state == "Ciudad de México"   , "09"),
         state = replace(state, state == "Durango"            , "10"),
         state = replace(state, state == "Guanajuato"         , "11"),
         state = replace(state, state == "Guerrero"           , "12"),
         state = replace(state, state == "Hidalgo"            , "13"),
         state = replace(state, state == "Jalisco"            , "14"),
         state = replace(state, state == "México"             , "15"),
         state = replace(state, state == "Michoacán"          , "16"),
         state = replace(state, state == "Morelos"            , "17"),
         state = replace(state, state == "Nayarit"            , "18"),
         state = replace(state, state == "Nuevo León"         , "19"),
         state = replace(state, state == "Oaxaca"             , "20"),
         state = replace(state, state == "Puebla"             , "21"),
         state = replace(state, state == "Querétaro"          , "22"),
         state = replace(state, state == "Quintana Roo"       , "23"),
         state = replace(state, state == "San Luis Potosí"    , "24"),
         state = replace(state, state == "Sinaloa"            , "25"),
         state = replace(state, state == "Sonora"             , "26"),
         state = replace(state, state == "Tabasco"            , "27"),
         state = replace(state, state == "Tamaulipas"         , "28"),
         state = replace(state, state == "Tlaxcala"           , "29"),
         state = replace(state, state == "Veracruz"           , "30"),
         state = replace(state, state == "Yucatán"            , "31"),
         state = replace(state, state == "Zacatecas"          , "32"),
         state = replace(state, state == "República Mexicana" , "33")) %>%
  mutate(state = as.factor(state),
         sex   = as.factor(sex))

# Assign English level names in state variable
levels(df_mig_inter$state) <- v_names_states

# General migration

## 02.05 General migration category in colum "type" -----------------------
df_mig_inter_2 <- df_mig_inter %>%
  mutate(type = "International") %>%
  bind_rows(df_mig_states) %>%
  mutate(type = if_else(is.na(type), "Interstate", type))

df_migration <- df_mig_inter_2 %>%
  bind_rows(df_mig_inter_2 %>% # create and append Total category
              group_by(year, state, age, sex) %>%
              summarise(emigrants = sum(emigrants),
                        immigrants = sum(immigrants),
                        CVE_GEO = unique(CVE_GEO)) %>%
              mutate(type = "Total")) %>%
  arrange(year, state, age, sex, type) %>%
  tibble::as_tibble()

# 03 Save data ---------------------------------------------------------
# usethis::use_data(df_mig_states, overwrite = TRUE)
# usethis::use_data(df_mig_inter,  overwrite = TRUE)
usethis::use_data(df_migration,  overwrite = TRUE)

# write.csv(df_mig_states, file = "data/df_mig_states.csv")
# write.csv(df_mig_inter, file = "data/df_inter_mig.csv")

