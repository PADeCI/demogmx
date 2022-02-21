#*****************************************************************************#
# This script loads the formated data of migration projections from CONAPO    #
# and expands it based on year and age information. It is assumed that:       #
# - The migration is equally distributed in each year within each year group. #
# - The migration assumed to be proportional to the distribution population.  #
#                                                                             #
# Author: Fernando Alarid-Escudero                                            #
# Author: David Garibay-Treviño                                               #
# E-mail: fernando.alarid@cide.edu                                            #
# E-mail: david.garibay@cide.edu                                              #
#*****************************************************************************#

rm(list = ls()) # to clean the workspace

# 01 Load packages and functions ---------------------------------------
## 01.01 Load packages -------------------------------------------------
library(demogmx)
library(tidyr)
library(dplyr)

## 01.02 Load functions ------------------------------------------------
# no functions required

# 02 Import base data -----------------------------------------------------
# Migration data
load("data/df_migration.Rdata")

# Vectors
## Of available years
v_year <- seq(1970, 2050)
## Of available sexes
v_sex <- c("Female", "Male", "Total")
## Of available states, excluding MCMA
v_state <- as.character(unique(df_migration$state))
## Of available ages (in migration data)
v_age <- seq(0, 89)
## Of all types of migration
v_type <- c("International", "Interstate", "Total")


# Population data
df_pop <- get_population(v_state   = v_state,
                         v_year     = v_year,
                         v_sex      = v_sex,
                         v_age      = v_age,
                         age_groups = FALSE)

# 03 Sanity Checks --------------------------------------------------------
# Check if selected state(s) is(are) part of the available options set
if (!all(v_state %in% unique(df_migration$state))) {
  stop("v_state must be a character element or vector containing at least one of the next names: \n \n",
       paste(levels(df_migration$state), collapse = ", "))
}
# Check if selected sex is part of the available options set
if (!all(v_sex %in% unique(df_migration$sex))) {
  stop(stop("v_sex must be a character element or vector containing at least one of the next names: ",
            paste(unique(df_mortrate_state_age_sex$sex), collapse = ", ")))
}
# Check selected year
if (!all(v_year %in% seq(1970, 2050))) {
  stop("v_year must be a numeric value or vector with values between 1970 and 2050")
}
# Check selected age(s)
if (!all(v_age %in% seq(0,89))) {
  stop("v_age must be a numeric value or vector with values between 0 and 89")
}
# Check type of migration
if (!all(v_type %in% c("Interstate", "International", "Total"))) {
  stop("v_age must be a numeric value or vector with values between 0 and 109")
}

# 04 Filter data based on parameters --------------------------------------
## Obtain age groups
l_age <- split(0:89, cut(x = 0:89, breaks = seq(0,90, 5),
                         include.lowest = T, right = F))
aa_age <- sapply(l_age, `%in%`, x = c(min(v_age), max(v_age)))
bb_age <- which(aa_age, arr.ind = TRUE)
cc_age <- unique(bb_age[,2])
dd_age <- unique(df_migration$age)[cc_age[1]:cc_age[2]]

## Obtain year groups
l_year <- split(1970:2050, cut(x = 1970:2050, breaks = seq(1970, 2050, 5),
                               include.lowest = T, right = F))
aa_year <- sapply(l_year, `%in%`, x = c(min(v_year), max(v_year)))
bb_year <- which(aa_year, arr.ind = TRUE)
cc_year <- unique(bb_year[,2])
dd_year <- unique(df_migration$year)[cc_year[1]:cc_year[2]]

## Filter migration dataframe based on age and year groups
df_migration_aux <- df_migration %>%
  filter(state %in% v_state,
         year  %in% dd_year,
         age   %in% dd_age,
         sex   %in% v_sex,
         type  %in% v_type)

# 05 Expand migration dataset ---------------------------------------------
# Expand by years
df_migration_aux2 <- df_migration_aux %>%
  group_by(year, state, age, sex, type) %>%
  tidyr::separate(col = year, sep = "-", into = c("a", "b")) %>%
  mutate(a = as.numeric(a),
         b = as.numeric(b)) %>%
  slice(rep(row_number(), 5)) %>% # Repeat each row 5 times
  ungroup() %>%
  group_by(a, b, state, age, sex, type) %>%
  mutate(year = seq(from = a[1], # Fill years column from a to b-1 in each group
                    to = b[1]-1)) %>%
  arrange(a, b, state, age, sex, type) %>%
  ungroup() %>%
  mutate(emigrants  = emigrants/5,
         immigrants = immigrants/5) %>%
  relocate(year, .before = state) %>%
  arrange(age, year, state, sex, type) %>%
  select(-a, -b)

# Expand by age
df_migration_aux3 <- df_migration_aux2 %>%
  group_by(year, state, age, sex, type) %>%
  tidyr::separate(col = age, sep = "-", into = c("age_1", "age_2")) %>%
  ungroup() %>%
  slice(rep(row_number(), each = 5)) %>% # Repeat each row 5 times
  group_by(year, state, age_1, age_2, sex, type) %>%
  mutate(age = seq(from = age_1[1], to = age_2[1])) %>% # Fill age column
  ungroup() %>%
  select(-age_1, -age_2) %>%
  relocate(age, .before = CVE_GEO) %>%
  arrange(year, state, age, sex, type)

# 06 Filter the expanded migration dataset --------------------------------
df_migration_aux3 <- df_migration_aux3 %>%
  filter(year %in% seq(min(v_year), max(v_year)),
         age %in% seq(min(v_age), max(v_age)))

# 07 Create population proportions based on age group ---------------------
df_pop_cut <- df_pop %>%
  mutate(age_grp = cut(x = age, breaks = seq(0, 90, 5),
                       include.lowest = TRUE, right = FALSE)) %>%
  group_by(year, state, sex, age_grp) %>%
  # Obtain proportion of each age within the age group they belong
  mutate(proportion = prop.table(population)) %>%
  ungroup()

# 08 Apply proportions based on population data ---------------------------
df_migration_expanded <- df_migration_aux3 %>%
  left_join(y = df_pop_cut,
            by = c("year", "state", "CVE_GEO", "sex", "age")) %>%
  mutate(emigrants = round(emigrants*proportion),
         immigrants = round(immigrants*proportion))

# # SANITY CHECK -- Compare with df_migration_aux2, they should be the same or very similar
# df_mm <- df_migration_expanded %>%
#   group_by(year, state, sex, age_grp, type) %>%
#   summarise(emig = sum(emigrants),
#             immig = sum(immigrants)) %>%
#   arrange(age_grp, year, state, sex, type)

# 09 Save data ------------------------------------------------------------
save(df_migration_expanded, file = "data/df_migration_expanded.Rdata")

# write.csv(df_migration_expanded, file = "data/df_migration_expanded.csv")




