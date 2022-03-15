


library(usethis)
library(devtools)


# get_migration()
# get_births_INEGI()
# get_deaths()
# get_population()
# get_death_population()

#' Out of those who were alive in 2010
#'
#' Include those who came
#' Include those who lived
#'
#' Exclude those who emigrated
#' Exclude those who died


# age_group = FALSE ------------------------------------------------------
## Params ------------------------------------------------------------------
state <- "National"
year <- "2010"
sex <- "Total"
age <- 90
mig_type <- "Total"
age_grp <- FALSE
year_grp <- FALSE


## Base datasets -----------------------------------------------------------
df_births <- get_births_INEGI(v_state = state, v_year = year, v_sex = sex,
                              year_groups = year_grp)

df_pop <- get_population(v_state = state, v_year = year, v_sex = sex,
                         v_age = age, age_groups = age_grp)

df_mort <- get_deaths(v_state = state, v_year = year, v_sex = sex, v_age = age,
                      age_groups = age_grp)

df_mig <- get_migration(v_state = state, v_year = year, v_sex = sex,
                        v_age = age, v_type = mig_type, age_groups = age_grp)


## Manipulate data ---------------------------------------------------------
str_age_grp <- ifelse(test = age_grp, yes = "age_group", no = "age")

## merge pop, mort, and mig dataframes
df_info_1 <- df_pop %>%
  left_join(df_mort, by = c("year", "state", "CVE_GEO", "sex", str_age_grp))
  left_join(x = ., y = df_mig,  by = c("year", "state", "CVE_GEO", "sex", str_age_grp))

df_info_1 <- df_pop %>%
  left_join(x = ., y = df_mort, by = c("year", "state", "CVE_GEO", "sex", str_age_grp)) %>%
  left_join(x = ., y = df_mig,  by = c("year", "state", "CVE_GEO", "sex", str_age_grp))

df_info_2 <- df_info_1 %>%
  mutate(n_pop = population + emigrants - immigrants - deaths) %>%
  mutate(aging_rate = n_pop/population) %>%
  select(-proportion, -em_rate, -im_rate, -type)

df_info_2$aging_rate



# year_group = TRUE ------------------------------------------------------
## Params ------------------------------------------------------------------

age_grp <- TRUE
year_grp <- FALSE

# get_aging_rate <- with no groups

## Base datasets -----------------------------------------------------------
df_births <- get_births_INEGI(v_state = state, v_year = year, v_sex = sex,
                              year_groups = year_grp)

df_pop <- get_population(v_state = state, v_year = year, v_sex = sex,
                         v_age = age,
                         age_groups = age_grp)

df_mort <- get_deaths(v_state = state, v_year = year, v_sex = sex, v_age = age,
                      age_groups = age_grp)

df_mig <- get_migration(v_state = state, v_year = year, v_sex = sex,
                        v_age = age, v_type = mig_type,
                        age_groups = age_grp)


## Manipulate data ---------------------------------------------------------
str_age_grp <- ifelse(test = age_grp, yes = "age_group", no = "age")

## merge pop, mort, and mig dataframes
df_info_1 <- df_pop %>%
  left_join(x = ., y = df_mort, by = c("year", "state", "CVE_GEO", "sex", str_age_grp)) %>%
  left_join(x = ., y = df_mig,  by = c("year", "state", "CVE_GEO", "sex", str_age_grp))

# df_info_2 <- df_info_1 %>%
#   mutate(n_pop = population + emigrants - immigrants - deaths) %>%
#   mutate(aging_rate = n_pop/population) %>%
#   select(-proportion, -em_rate, -im_rate, -type)
#
# df_info_2$aging_rate


df_info_2 <- df_info_1 %>%
  mutate(aging_pop = population + emigrants - immigrants - deaths) %>%
  mutate(aging_rate = aging_pop/population) %>%
  select(year, state, CVE_GEO, sex, all_of(str_age_grp), aging_pop, aging_rate)



