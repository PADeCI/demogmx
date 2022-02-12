##************************************************************************
## Script Name: get_migration
## Purpose:
##
## Created: February, 2022
## Authors:
## - David Garibay-Treviño, MSc
##
## GitHub:
## - du-gartre
##
##************************************************************************

#' Get migration
#'
#' \code{get_migration} Function that allows user to get a demographic dataset
#' regarding migration information based on the parameter set given by the
#' user
#'
#' @param v_state State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#'  2050.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param v_age Specifies the age bins to aggregate and return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#' @import tidyr
#' @return A demographic dataset based on specified parameters.
#' @param export
#'
#' @examples
#' get_population(v_states =  "Chiapas", v_year = 2015, v_sex = "Total",
#' v_age = c(0, 15, 45, 75), age_groups = FALSE)
#'
#' get_population(v_states = c("Aguascalientes", "Campeche"),
#' v_year = c(2010, 2021), v_sex = "Male", v_age = c(0, 15, 45, 75),
#' age_groups = TRUE)
#'
#' @export
get_migration <- function(v_states   = "Aguascalientes",
                          # v_year   = "2020-2025",
                          v_year     = c(2000, 2010, 2025),
                          v_sex      = c("Female", "Male", "Total"),
                          # v_age    = "20-24",
                          v_age      = c(0, 15, 24, 36),
                          v_type     = c("Interstate", "International", "Total"),
                          age_groups = TRUE) {

  # Import base data --------------------------------------------------------
  # Migration data
  load("data/df_migration.Rdata")

  # Population data
  df_pop <- get_population(v_states   = v_states,
                           v_year     = seq(min(v_year), max(v_year)),
                           v_sex      = v_sex,
                           v_age      = seq(min(v_age), max(v_age)),
                           age_groups = FALSE)

  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_states %in% unique(df_migration$state))) {
    stop("v_states must be a character element or vector containing at least one of the next names: \n \n",
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
    stop("v_age must be a numeric value or vector with values between 0 and 109")
  }
  # Check type of migration
  if (!all(v_type %in% c("Interstate", "International", "Total"))) {
    stop("v_age must be a numeric value or vector with values between 0 and 109")
  }

  # Filter data based on parameters -----------------------------------------
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
    filter(state %in% v_states,
           year  %in% dd_year,
           age   %in% dd_age,
           sex   %in% v_sex,
           type  %in% v_type)

  # Expand migration dataset ------------------------------------------------
  # Expand by years
  df_migration_aux2 <- df_migration_aux %>%
    group_by(year, state, age, sex, type) %>%
    tidyr::separate(col = year, sep = "-", into = c("a", "b")) %>%
    mutate(a = as.numeric(a),
           b = as.numeric(b)) %>%
    slice(rep(row_number(), 5)) %>%
    ungroup() %>%
    group_by(a, b, state, age, sex, type) %>%
    mutate(year = seq(from = a[1],
                      to = b[1]-1)) %>%
    arrange(a, b, state, age, sex, type) %>%
    ungroup() %>%
    mutate(emigrants  = emigrants/5,
           immigrants = immigrants/5) %>%
    select(-a, -b) %>%
    relocate(year, .before = state) %>%
    arrange(age, year, state, sex, type)

  # Expand by age
  df_migration_aux3 <- df_migration_aux2 %>%
    group_by(year, state, age, sex, type) %>%
    tidyr::separate(col = age, sep = "-", into = c("age_1", "age_2")) %>%
    ungroup() %>%
    slice(rep(row_number(), each = 5)) %>%
    group_by(year, state, age_1, age_2, sex, type) %>%
    mutate(age = seq(from = age_1[1], to = age_2[1])) %>%
    ungroup() %>%
    select(-age_1, -age_2) %>%
    relocate(age, .before = CVE_GEO) %>%
    arrange(year, state, age, sex, type)

  # Filter the expanded migration dataset -----------------------------------
  df_migration_aux3 <- df_migration_aux3 %>%
    filter(year %in% seq(min(v_year), max(v_year)),
           age %in% seq(min(v_age), max(v_age)))

  # Create population proportions based on age group ------------------------
  df_pop_cut <- df_pop %>%
    mutate(age_grp = cut(x = age, breaks = seq(0, 90, 5),
                         include.lowest = TRUE, right = FALSE)) %>%
    group_by(year, state, sex, age_grp) %>%
    mutate(proportion = prop.table(population)) %>%
    ungroup()


  # Apply proportions based on population data ------------------------------
  df_migration_aux4 <- df_migration_aux3 %>%
    left_join(df_pop_cut, by = c("year", "state", "CVE_GEO", "sex", "age")) %>%
    mutate(emigrants = round(emigrants*proportion),
           immigrants = round(immigrants*proportion)) %>%
    mutate()

  # # SANITY CHECK -- Compare with df_migration_aux2
  # df_migration_aux4 %>%
  #   group_by(year, state, sex, age_grp, type) %>%
  #   summarise(emig = sum(est_emig),
  #             immig = sum(est_immig)) %>%
  #   arrange(age_grp, year, state, sex, type)

  # Whether to group data by age --------------------------------------------
  if (age_groups == FALSE) {
    df_outcome <- df_migration_aux4 %>%
      filter(age  %in% v_age,
             year %in% v_year) %>%
      select(year, state, CVE_GEO, sex, age, emigrants,
             immigrants, type, population)
  } else {
    df_outcome <- df_migration_aux4 %>%
      mutate(age_group = cut(x = age, breaks = c(v_age, Inf),
                             include.lowest = TRUE)) %>%
      filter(year %in% v_year,
             complete.cases(.)) %>%
      # group_by(year, state, CVE_GEO, sex, age_group) %>%
      group_by(year, state, CVE_GEO, sex, age_group, type) %>%
      summarise(emigrants  = sum(emigrants),
                immigrants = sum(immigrants),
                population = sum(population))
  }
  return(df_outcome)
}


# Test set
v_states   = "Aguascalientes"
v_year     = c(2000, 2010, 2025)
v_sex      = "Total"
v_age      = c(0, 15, 24, 36)
v_type     = c("Interstate", "International", "Total")
age_groups = FALSE
