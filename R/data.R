

#' Birth data of Mexico, between 1970 to 2050
#'
#' A dataset containing the number of births estimated by CONAPO between 1970
#' and 2050. These estimations are disaggregated by year and state but not by
#' sex of the child.
#'
#' @format A data frame with 2,673 rows and 5 variables:
#' \describe{
#' \item{year} {Year of the data}
#' \item{state} {State where the births occurred}
#' \item{CVE_GEO} {Geographical code of the state of occurrence}
#' \item{population} {Number of people living in that year and state}
#' \item{births} {Number of births that occurred in that year and state}
#' }
#'
#' @source \url{https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/284ef503-6c57-4a5c-8247-e13241ad4cee}
"df_birth_pop_states"


#' Birth data of Mexico, between 1985 to 2020, disaggregated by sex
#'
#' A dataset containing the number of births registered by INEGI between 1985
#' and 2020. These estimations are disaggregated by year and state and sex
#'
#' @format A data frame with 3,564 rows and 8 variables:
#' \describe{
#' \item{year} {Year of the data}
#' \item{state} {State where the births occurred}
#' \item{sex} {Sex of the child}
#' \item{CVE_GEO} {Geographical code of the state of occurrence}
#' \item{births} {Number of births that occurred in that year, state and sex}
#' \item{birth_prop} {Proportion of births in relation to the sum of both sexes}
#' \item{population} {Number of people living in that year and state}
#' \item{birth_rate} {Number of births divided by the population with the same year, state and sex.}
#' }
#'
#' @source \url{https://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/natalidad/nacimientos.asp}
"df_births_INEGI"


#' Migration data of Mexico, from 1970 to 2050 aggregated by ages.
#'
#' A dataset containing the estimated migration by CONAPO between 1970 and 2050.
#' These estimations are disaggregated by year, state, sex and type of migration
#' (immigration or emigration). This dataset includes emigration rate estimates.
#' This dataset groups the information by groups of ages that increases by 5
#' years.
#'
#' @format A data frame with 84,672 rows and 8 variables:
#' \describe{
#' \item{year} {Year of the data}
#' \item{state} {State where the migration occurred}
#' \item{CVE_GEO} {Geographical code of the state of occurrence}
#' \item{sex} {Sex of the migrants}
#' \item{age_group} {Age group to which belongs the data}
#' \item{type} {Type of migration: Interstate, International or Total}
#' \item{emigrants} {Number of emigrants}
#' \item{immigrants} {Number of immigrants}
#' \item{migratoin} {Net migration (immigrants - emigrants)}
#' \item{em_rate} {emigration rate}
#' \item{im_rate} {immigration rate}
#' }
#'
#' @source \url{ https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/81802225-00c4-49b4-ab49-a491334062b4,
#'  https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/3f9c8392-153d-4299-a027-9f1b179f6edf}
"df_migration"


#' Migration data of Mexico, from 1970 to 2050.
#'
#' A dataset containing the estimated migration by CONAPO between 1970 and 2050.
#' These estimations are disaggregated by year, state, sex and type of migration
#' (immigration or emigration).
#'
#' @format A data frame with 2,116,800 rows and 8 variables:
#' \describe{
#' \item{year} {Year of the data}
#' \item{state} {State where the migration occurred}
#' \item{age} {Age to which belongs the data}
#' \item{CVE_GEO} {Geographical code of the state of occurrence}
#' \item{sex} {Sex of the migrants}
#' \item{emigrants} {Number of emigrants}
#' \item{immigrants} {Number of immigrants}
#' \item{type} {Type of migration: Interstate, International or Total}
#' }
"df_migration_expanded"


#' Population and deaths data of Mexico, from 1970 to 2050.
#'
#' A dataset containing the estimated population and deaths by CONAPO
#' between 1970 and 2050. These estimations are disaggregated by year, state
#' and sex
#'
#' @format A data frame with 882,420 rows and 7 variables:
#' \describe{
#' \item{year} {Year of the data}
#' \item{state} {State of the data}
#' \item{sex} {Sex of the data}
#' \item{CVE_GEO} {Geographical code of the state of the data}
#' \item{age} {Age of the data}
#' \item{population} {Estimated population by CONAPO}
#' \item{deaths} {Estimated deaths by CONAPO}
#' }
"df_mortrate_state_age_sex"

