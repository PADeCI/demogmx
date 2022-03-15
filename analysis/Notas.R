
#' Documentos técnicos de CONAPO
#' https://www.gob.mx/conapo/acciones-y-programas/conciliacion-demografica-de-mexico-1950-2015-y-proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050
#' https://www.gob.mx/cms/uploads/attachment/file/390813/Publicaci_n__Conciliaci_n_demogr_fica_de_M_xico_WEB-compressed.pdf




# split(1970:2050, cut(x = 1970:2050, breaks = seq(1970, 2050, 5),
#                        include.lowest = F, right = T))

## Issues

#' Fecha del corte de la bdd de migración
#' ------ Los años
#' ACTUALMENTE:-------------------------------------------
# $`[1970,1975)`
# [1] 1970 1971 1972 1973 1974
#
# $`[1975,1980)`
# [1] 1975 1976 1977 1978 1979
#'
#' TAMBIÉN PUEDE SER ESTA OPCIÓN -------------------------
# $`(1970,1975]`
# [1] 1971 1972 1973 1974 1975
#
# $`(1975,1980]`
# [1] 1976 1977 1978 1979 1980
#
#'
#' ------ Las edades:
#'  "00-04" "05-09" "10-14" "15-19" "20-24" "25-29" "30-34" "35-39" "40-44"
#'  "45-49" "50-54" "55-59" "60-64" "65-69" "70-74" "75-79" "80-84" "85-89"
#'
# $`[0,5)`
# [1] 0 1 2 3 4
#
# $`[5,10)`
# [1] 5 6 7 8 9
#
# $`[10,15)`
# [1] 10 11 12 13 14
#
# ...
#
# $`[85,90]`
# [1] 85 86 87 88 89
#'
#'
#' Los nacimientos de CONAPO no diferencían entre sexos. INEGI tiene la base de
#' datos de nacimientos registrados y ocurridos en México de 1985 a 2020
#' - Nueva función <- get_births_INEGI
#' - Hice estimaciones de las proporciones por sexo
#' - ¿cuál usar?
#'
#' En get_population las proporciones se obtienen de agrupar año, estado y sexo
#' - ej. todos los hombres de Aguascalientes en 1970 suman 1 en su proporción
#' - Si sumáramos la proporción de hombres, mujeres y total, eso daría 3
#'
#'
#'
#'
#'
