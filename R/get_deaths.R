##************************************************************************
## Script Name: get_deaths
##
## Created: February, 2022
## Authors:
## - Mariana Fernandez
## - David Garibay-Treviño, MSc
##
## GitHub:
## - marianafdz465
## - du-gartre
##
##************************************************************************


#' Load death data disaggregated by year, state, sex and age.
#'
#' \code{get_deaths} is a function that allows the user to get a demographic
#' dataset regarding deaths and death rates, based on the given paramaters.
#'
#' @param v_state Character vector specifying the state(s) that the function
#' will return
#' @param v_year Numeric vector that specifies the year(s) to return. Must have
#' numbers between 1970 and 2050.
#' @param v_sex Character vector that selects sex. Options: Female, Male and
#' Total.
#' @param v_age Numeric vector that specifies the age(s) to return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#'
#' @return A demographic dataset based on the selected parameters
#'
#' @examples
#' get_deaths(v_state =  "Chiapas",
#'            v_year = 2015,
#'            v_sex = "Total",
#'            v_age = c(0, 15, 45, 75),
#'            age_groups = FALSE)
#'
#' get_deaths(v_state = c("Aguascalientes", "Campeche"),
#'            v_year = c(2010, 2021),
#'            v_sex = "Male",
#'            v_age = c(0, 15, 45, 75),
#'            age_groups = TRUE)
#'
#' @export
get_deaths <- function(v_state   = "National",
                       v_year     = 2021,
                       v_sex      = "Total",
                       v_age      = c(0,5,15,25,45, 55,65,70),
                       age_groups = TRUE) {

  # Execute auxiliary function ----------------------------------------------
  df_death_aux <- get_death_population(v_state   = v_state,
                                       v_year     = v_year,
                                       v_sex      = v_sex,
                                       v_age      = v_age,
                                       age_groups = age_groups)

  # Obtain death rate and remove population column --------------------------
  df_death <-  df_death_aux %>%
    mutate(death_rate = deaths / population) %>%
    select(-population)

  return(df_death)
}

