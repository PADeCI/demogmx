#*****************************************************************************#
# This script loads the unformatted data of mortality and population          #
# projections from CONAPO from 1970 to 2050, having specific information by   #
# state, age and sex. We also add an homologated state code (CVE_GEO)         #
#                                                                             #
# Author: Fernando Alarid-Escudero                                            #
# Author: David Garibay-Treviño                                               #
# E-mail: fernando.alarid@cide.edu                                            #
# E-mail: david.garibay@cide.edu                                              #
#*****************************************************************************#

rm(list = ls()) # to clean the workspace

# 01 Load packages and functions ---------------------------------------
## 01.01 Load packages -------------------------------------------------
library(dplyr)

## 01.02 Load functions ------------------------------------------------
# no functions required

# 02 Import data ----------------------------------------------------------
load("data-raw/df_mortrate_state_age_sex.Rdata")

# 03 Manipulate data ------------------------------------------------------
df_mortrate_state_age_sex <- df_mortrate_state_age_sex %>%
  mutate(CVE_GEO = replace(CVE_GEO, state == "Aguascalientes"     , 01),
         CVE_GEO = replace(CVE_GEO, state == "Baja California"    , 02),
         CVE_GEO = replace(CVE_GEO, state == "Baja California Sur", 03),
         CVE_GEO = replace(CVE_GEO, state == "Campeche"           , 04),
         CVE_GEO = replace(CVE_GEO, state == "Coahuila"           , 05),
         CVE_GEO = replace(CVE_GEO, state == "Colima"             , 06),
         CVE_GEO = replace(CVE_GEO, state == "Chiapas"            , 07),
         CVE_GEO = replace(CVE_GEO, state == "Chihuahua"          , 08),
         CVE_GEO = replace(CVE_GEO, state == "Mexico City"        , 09),
         CVE_GEO = replace(CVE_GEO, state == "Durango"            , 10),
         CVE_GEO = replace(CVE_GEO, state == "Guanajuato"         , 11),
         CVE_GEO = replace(CVE_GEO, state == "Guerrero"           , 12),
         CVE_GEO = replace(CVE_GEO, state == "Hidalgo"            , 13),
         CVE_GEO = replace(CVE_GEO, state == "Jalisco"            , 14),
         CVE_GEO = replace(CVE_GEO, state == "State of Mexico"    , 15),
         CVE_GEO = replace(CVE_GEO, state == "Michoacan"          , 16),
         CVE_GEO = replace(CVE_GEO, state == "Morelos"            , 17),
         CVE_GEO = replace(CVE_GEO, state == "Nayarit"            , 18),
         CVE_GEO = replace(CVE_GEO, state == "Nuevo Leon"         , 19),
         CVE_GEO = replace(CVE_GEO, state == "Oaxaca"             , 20),
         CVE_GEO = replace(CVE_GEO, state == "Puebla"             , 21),
         CVE_GEO = replace(CVE_GEO, state == "Queretaro"          , 22),
         CVE_GEO = replace(CVE_GEO, state == "Quintana Roo"       , 23),
         CVE_GEO = replace(CVE_GEO, state == "San Luis Potosi"    , 24),
         CVE_GEO = replace(CVE_GEO, state == "Sinaloa"            , 25),
         CVE_GEO = replace(CVE_GEO, state == "Sonora"             , 26),
         CVE_GEO = replace(CVE_GEO, state == "Tabasco"            , 27),
         CVE_GEO = replace(CVE_GEO, state == "Tamaulipas"         , 28),
         CVE_GEO = replace(CVE_GEO, state == "Tlaxcala"           , 29),
         CVE_GEO = replace(CVE_GEO, state == "Veracruz"           , 30),
         CVE_GEO = replace(CVE_GEO, state == "Yucatan"            , 31),
         CVE_GEO = replace(CVE_GEO, state == "Zacatecas"          , 32),
         CVE_GEO = replace(CVE_GEO, state == "National"           , 00)) %>%
  mutate(CVE_GEO = as.numeric(CVE_GEO)) %>%
  as_tibble()

# 04 Save data ------------------------------------------------------------
save(df_mortrate_state_age_sex, file = "data/df_mortrate_state_age_sex.rda")

