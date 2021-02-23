##************************************************************************
## Script Name: Count Population (Deaths)
## Purpose:
##
##
## Created:
## Authors: Mariana Fernandez
##
## GitHub: marianafdz465
##
##
##************************************************************************


#'Get death population
#'
#'\code{get_death_population} get function that allows user to
#'get a dataset with the subset of different params
#'
#'@param v_state State(s) of desired data
#'@param v_year Year(s) of desired data
#'@param select_sex Sex (Female/Male/Total)
#'@param v_init_age_grps Specifies the age bins to aggregate and return
#'@param age_grps (default is True)
#'
#'@return A dataset with the data select
#'
#'@param export
#'
#'@examples
#'get_death_population(v_state = "Aguascalientes", v_year = "2021", select_sex = "Total",
#'v_init_age_grps = c("0","5","15","25","45", "55","65","70"), age_grps= T)
#'get_death_population(v_state = "Aguascalientes", v_year = c("2021", "2022"), select_sex = "Female",
#'v_init_age_grps = c("21", "22", "23"), age_grps= F)
get_death_population <- function( v_state = "National",
                            v_year  = "2021",
                            select_sex    = unique(df_mortrate_state_age_sex$sex),
                            v_init_age_grps = c("0","5","15","25","45", "55","65","70"),
                            age_grps = T) {
  require(tidyverse)

  appDir <- system.file( package = "demogmx")
  GLOBAL_MX_POPULATION_DEATH_FILE <- paste0(appDir, "/data", "/df_mortrate_state_age_sex.Rdata")
  load(GLOBAL_MX_POPULATION_DEATH_FILE)
  #df_deaths_state_age_sex <- read.fst("data/df_deaths_state_age_sex.fst")

  # Filter Data Outcome -----------------------------------------------------
  df_mort_outcome <- df_mortrate_state_age_sex[df_mortrate_state_age_sex$state %in% v_state,]
  df_mort_outcome <- df_mort_outcome[df_mort_outcome$year %in% v_year,]
  df_mort_outcome <- df_mort_outcome[df_mort_outcome$sex %in% select_sex,]
  #df_mort_outcome <- df_mort_outcome[df_mort_outcome$age %in% select_age,]

  if(age_grps == F){
    df <- df_mort_outcome[df_mort_outcome$age %in% v_init_age_grps,]
  }else{
    df <- df_mort_outcome %>%
      mutate(AgeGrp = cut(age, breaks = c(v_init_age_grps, Inf),
                          include.lowest = TRUE, right = FALSE)) %>%
      group_by( year,state, CVE_GEO,sex, AgeGrp ) %>%
      summarise(population = sum(population), # Population by age group
                deaths     = sum(deaths))    # Total number of deaths
  }

  assign("df_deaths_age_sex_outcome", df, envir = .GlobalEnv)

  return(df)
}
