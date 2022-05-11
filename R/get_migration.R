##************************************************************************
## Script Name: get_migration
##
## Created: February, 2022
## Authors:
## - David Garibay-Treviño, MSc
##
## GitHub:
## - du-gartre
##
##************************************************************************

#' Load migration data
#'
#' \code{get_migration} is a function that allows the user to get a demographic
#' dataset regarding migration based on the given parameters.
#'
#' @param v_state State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#'  2050.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param v_age Specifies the age bins to aggregate and return.
#' @param v_type Indicates whether type of migration to include. It can be
#' Interstate migration, International migration or Total migration.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#'
#' @return A demographic dataset containing the selected year, the state, the
#' state code (CVE_GEO), the age (age group, when \code{age_groups = TRUE}), the
#' type of migration, the number of emigrants, the number of immigrants,
#' migration balance (immigrants - emigrants), and the rate of
#' emigration and immigration, respectively.
#'
#' @import dplyr
#'
#' @examples
#'
#' get_migration(v_state = c("Yucatan", "Sonora"),
#'               v_year = 2015,
#'               v_sex = "Total",
#'               v_age = c(0, 25, 35, 45),
#'               v_type = "International",
#'               age_groups = TRUE)
#'
#' get_migration(v_state = "Mexico City",
#'               v_year = c(2000, 2010),
#'               v_sex = "Female",
#'               v_age = c(0, 15, 35, 45, 75),
#'               v_type = c("Interstate", "International", "Total"),
#'               age_groups = FALSE)
#'
#' @export
get_migration <- function(v_state    = "National",
                          v_year     = c(2000, 2010, 2025),
                          v_sex      = c("Female", "Male", "Total"),
                          v_age      = c(0, 15, 24, 36),
                          v_type     = c("Interstate", "International", "Total"),
                          age_groups = TRUE) {

  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_state %in% unique(df_migration_expanded$state))) {
    stop("v_state must be a character element or vector containing at least one of the next names:\n\n",
         paste(unique(df_migration$state), collapse = ", "))
  }
  # Check if selected sex is part of the available options set
  if (!all(v_sex %in% unique(df_migration_expanded$sex))) {
    stop("v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")
  }
  # Check selected year
  if (!all(v_year %in% seq(1970, 2050))) {
    stop("v_year must be a integer value or vector with values between 1970 and 2050")
  }
  # Check selected age(s)
  if (!all(v_age %in% seq(0,89))) {
    stop("v_age must be a integer value or vector with values between 0 and 89")
  }
  # Check type of migration
  if (!all(v_type %in% c("Interstate", "International", "Total"))) {
    stop("v_type must be a character element or vector containing at least one of the next names: 'Interstate', 'International', 'Total'")
  }
  # Check age groups option
  if (!is.logical(age_groups)) {
    stop("age_groups must be a logical value (TRUE / FALSE)")
  }


  # Whether to group data by age --------------------------------------------
  if (age_groups) {
    df_outcome <- df_migration_expanded %>%
      filter(year  %in% v_year,
             state %in% v_state,
             sex   %in% v_sex,
             type  %in% v_type,
             complete.cases(.)) %>%
      mutate(age_group = cut(x = age, breaks = c(v_age, Inf),
                             include.lowest = TRUE)) %>%
      group_by(year, state, CVE_GEO, sex, age_group, type) %>%
      summarise(emigrants  = sum(emigrants),
                immigrants = sum(immigrants),
                population = sum(population)) %>%
      ungroup() %>%
      mutate(net_migration = immigrants - emigrants,
             em_rate = emigrants/population,
             im_rate = immigrants/population,
             nm_rate = net_migration/population) %>%
      select(-population) %>%
      filter(complete.cases(.))
  } else {
    df_outcome <- df_migration_expanded %>%
      filter(year  %in% v_year,
             state %in% v_state,
             age   %in% v_age,
             sex   %in% v_sex,
             type  %in% v_type,
             complete.cases(.)) %>%
      select(year, state, CVE_GEO, sex, age, emigrants,
             immigrants, type, population) %>%
      mutate(net_migration = immigrants - emigrants,
             em_rate = emigrants/population,
             im_rate = immigrants/population,
             nm_rate = net_migration/population) %>%
      select(-population) %>%
      filter(complete.cases(.))
  }
  return(df_outcome)
}
