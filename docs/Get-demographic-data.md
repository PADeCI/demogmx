Get demographic data
================
PADeCI
April 2022

This document demonstrates the use of the functions contained in the
`demogmx` package. This package includes functions to obtain demographic
information regarding births, population, migration (immigration and
emigration), mortality, and aging rate in Mexico. All these functions
are very similar in the way they work, and once you become familiar with
using one, using the rest becomes very intuitive.

To start, first we load the package:

``` r
# Load libraries
library(demogmx)
```

To obtain information from the functions, it is important to know the
characteristics of the data. The available information is disaggregated
by year, state, sex, and age. In some cases, there are other
disaggregation variables like the type of migration in the
`get_migration()` function. To obtain information at a state level, we
have to introduce their name in English. The list of state names
available in the data-sets is:

    ##  [1] "Aguascalientes"      "Baja California"     "Baja California Sur"
    ##  [4] "Campeche"            "Coahuila"            "Colima"             
    ##  [7] "Chiapas"             "Chihuahua"           "Mexico City"        
    ## [10] "Durango"             "Guanajuato"          "Guerrero"           
    ## [13] "Hidalgo"             "Jalisco"             "State of Mexico"    
    ## [16] "Michoacan"           "Morelos"             "Nayarit"            
    ## [19] "Nuevo Leon"          "Oaxaca"              "Puebla"             
    ## [22] "Queretaro"           "Quintana Roo"        "San Luis Potosi"    
    ## [25] "Sinaloa"             "Sonora"              "Tabasco"            
    ## [28] "Tamaulipas"          "Tlaxcala"            "Veracruz"           
    ## [31] "Yucatan"             "Zacatecas"           "National"

As we can see, the last element in the list is *National,* with it we
can obtain the aggregated information of all the states in the country.

The list of sex names that are accepted in the functions is the
following:

    ## [1] "Female" "Male"   "Total"

In the present version of the package, the functions can vary the range
of ages that are accepted as parameter inputs. In some cases they accept
ages from 0 to 109 years, as is the case with `get_population()` or, in
other cases, from 0 to 89, if we use `get_migration()`.

Most of the functions have the option `age_groups` that only accepts
logical elements as inputs. When `age_groups = FALSE` the data-sets will
only return the data of the ages that are inside the `v_ages` numeric
vector. When `age_groups = TRUE`, the output data will be grouped by age
groups bounded by the elements inside the age vector. Suppose that we
have the next numeric vector representing the ages that we are looking
into the data:

``` r
v_ages <- c(0, 10, 15, 20)
```

-   If `age_groups = FALSE`, the output will be a data-set containing
    the information of the people with 0, 10, 15, and 20 years of age.

-   If `age_groups = TRUE`, the data will be grouped in the following
    blocks:

    -   The group of people between 0 and 10 years of age.

    -   The group of people between 11 and 15 years of age.

    -   The group of people between 16 and 20 years of age.

    -   The group of people 20 years old and older. In the data-sets,
        the last age block will have an `Inf` indicating that the upper
        bound of the block is an infinite positive number.

    -   We will notice how the age groups have been made looking at the
        `age_group` column. This column is made with the interval
        notation.

Now let’s have a closer look at each of the `demogmx` functions.

# Births

This function gives a dataset with the number and the rate of births in
the years, states, and sexes indicated by the user.

In the present version of the package there are two functions to obtain
birth information from Mexico. One gives the information with sex
disaggregation with data from 1985 to 2020 and the other does not have
sex disaggregation but has projections between1970 and 2050.

In this function there is no `age_groups` option, instead there is the
`year_groups` option that aggregates the information by age in the same
way as we explained `age_groups` before.

## General births from 1970 to 2050

This function gets the [number of births estimated by the National
Council of
Population](https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/284ef503-6c57-4a5c-8247-e13241ad4cee)
(CONAPO by its acronym in spanish), additionally it provides birth rate
information based on the population information that can be accessed
with `get_population()`. This function has birth projections from 1970
to 2050 for each state but does not have sex disaggregation.

**`Problema:`**
`en demog-mx no vienen los scripts de cómo se manipularon las bases de datos originales, quizás sea necesario volverlo a hacer aquí para que pueda ser replicable. En el caso específico de los nacimientos de la CONAPO, hay que procesar la base de datos original para obtener los datos obtenidos en data-raw.`

This function contains the next parameters:

-   `v_state` requires an element or a vector of characters containing
    the names of the desired states.

-   `v_year` needs a numeric element or vector with the years of the
    information.

-   `year_groups` needs a logical argument that indicates if data will
    be grouped by years.

Ahead there are some examples of how this function can be used. First we
obtain the data without aggregating it by years.

``` r
# Get data without grouping by year range
get_births(v_state = c("Guerrero", "Zacatecas", "National"),
           v_year = c(1970, 2000, 2020),
           year_groups = FALSE) 
```

    ## # A tibble: 9 x 5
    ##    year state     CVE_GEO  births birth_rate
    ##   <int> <chr>       <dbl>   <int>      <dbl>
    ## 1  1970 Guerrero       12   85806     0.0511
    ## 2  2000 Guerrero       12   84012     0.0270
    ## 3  2020 Guerrero       12   68085     0.0186
    ## 4  1970 National        0 2222585     0.0444
    ## 5  2000 National        0 2322025     0.0237
    ## 6  2020 National        0 2151358     0.0169
    ## 7  1970 Zacatecas      32   43853     0.0447
    ## 8  2000 Zacatecas      32   35136     0.0257
    ## 9  2020 Zacatecas      32   31168     0.0188

Now we can see what happens if the output is aggregated.

``` r
# Get data grouping by year range
get_births(v_state = c("Michoacan", "Guerrero", "Zacatecas", "National"),
           v_year = c(1970, 2000, 2020),
           year_groups = TRUE)
```

    ## # A tibble: 12 x 5
    ##    year_group  state     CVE_GEO   births birth_rate
    ##    <fct>       <chr>       <dbl>    <int>      <dbl>
    ##  1 [1970,2000] Guerrero       12  2773431     0.0370
    ##  2 [1970,2000] Michoacan      16  3580985     0.0352
    ##  3 [1970,2000] National        0 73396334     0.0317
    ##  4 [1970,2000] Zacatecas      32  1286995     0.0342
    ##  5 (2000,2020] Guerrero       12  1535249     0.0226
    ##  6 (2000,2020] Michoacan      16  1890914     0.0215
    ##  7 (2000,2020] National        0 45055055     0.0198
    ##  8 (2000,2020] Zacatecas      32   662425     0.0219
    ##  9 (2020,Inf]  Guerrero       12  1615318     0.0146
    ## 10 (2020,Inf]  Michoacan      16  2428192     0.0156
    ## 11 (2020,Inf]  National        0 57038811     0.0135
    ## 12 (2020,Inf]  Zacatecas      32   828243     0.0154

Note that the *year* column changes its name based in whether we chose
to group the data. If the data was not grouped the column is named
`year`. If it is grouped it will be named `year_group`.

If any of the functions of this package receive a wrong input in their
parameters, they will throw an error message and the list of accepted
values, based on the parameter where the error was detected.

``` r
# Throw an error
get_births(v_state = 55, # Is not a character and is not a state name.
           v_year = c(1970, 2000, 2020),
           year_groups = FALSE) 
```

    ## Error in get_births(v_state = 55, v_year = c(1970, 2000, 2020), year_groups = FALSE): v_state must be a character element or vector containing at least one of the next names:
    ## 
    ## Aguascalientes, Baja California, Baja California Sur, Campeche, Chiapas, Chihuahua, Coahuila, Colima, Durango, Guanajuato, Guerrero, Hidalgo, Jalisco, Mexico City, Michoacan, Morelos, National, Nayarit, Nuevo Leon, Oaxaca, Puebla, Queretaro, Quintana Roo, San Luis Potosi, Sinaloa, Sonora, State of Mexico, Tabasco, Tamaulipas, Tlaxcala, Veracruz, Yucatan, Zacatecas

## Births by sex

This function gives the number of [registered births based on Mexico’s
National Institute of Statistical and Geographical
data](https://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/natalidad/nacimientos.asp)
(INEGI, by it’s acronym in Spanish). As in the previous function, it
provides birth rate information. This information disaggregated by sex
for each state and has estimations from 1980 to 2020.

This function has a parameter called `v_sex` that specifies the sexes
that will contain the output data-set. This input accepts the character
arguments “Female”, “Male” and “Total”, giving the information of each
sex or the aggregation of both, respectively.

Here is an example of how this function can be used, without aggregating
by year.

``` r
# Get data without grouping by year range
get_births_INEGI(v_state = c( "Guerrero", "National"),
                 v_year = c(2000, 2020),
                 v_sex = c("Female", "Total"),
                 year_groups = FALSE) 
```

    ## # A tibble: 8 x 7
    ##    year state    CVE_GEO sex     births birth_prop birth_rate
    ##   <dbl> <chr>      <dbl> <chr>    <dbl>      <dbl>      <dbl>
    ## 1  2000 Guerrero      12 Female   85116      0.520     0.0533
    ## 2  2000 Guerrero      12 Total   163640      1         0.0525
    ## 3  2020 Guerrero      12 Female   29606      0.493     0.0157
    ## 4  2020 Guerrero      12 Total    60022      1         0.0164
    ## 5  2000 National       0 Female 1398703      0.500     0.0280
    ## 6  2000 National       0 Total  2797580      1         0.0285
    ## 7  2020 National       0 Female  800264      0.491     0.0123
    ## 8  2020 National       0 Total  1629208      1         0.0128

And here we can see how it works if we decide to aggregate the
information by years.

``` r
# Get data grouping by year range
get_births_INEGI(v_state = c( "Guerrero", "National"),
                 v_year = c(2000, 2020),
                 v_sex = c("Female", "Total"),
                 year_groups = TRUE) 
```

    ## # A tibble: 4 x 7
    ##   year_group  state    CVE_GEO sex      births birth_prop birth_rate
    ##   <fct>       <chr>      <dbl> <chr>     <dbl>      <dbl>      <dbl>
    ## 1 [2000,2020] Guerrero      12 Female  1114059      0.508     0.0304
    ## 2 [2000,2020] Guerrero      12 Total   2194179      1         0.0308
    ## 3 [2000,2020] National       0 Female 25813848      0.497     0.0214
    ## 4 [2000,2020] National       0 Total  51915377      1         0.0219

# Population

`get_population()` is a function that returns the [estimations of CONAPO
on number of people living in
Mexico](https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/e038c360-e6b3-4ddc-a34d-0a3141c0bdad).
This function allows to filter the data based on the year, state, sex
and age specified by the user. This function has population projections
from 1970 to 2050

`get_population()` function includes the next parameters:

-   `v_age` requires a numeric vector that indicates the ages that will
    be returned in the output data.

-   `age_groups` needs a logical argument in order to group the data
    based on the input of `v_age`.

Let’s see an example of how this function is executed without
aggregating the data by age.

``` r
# Get population data without grouping by age
get_population(v_state = "Mexico City",
               v_year = 2000,
               v_sex = c("Female", "Male"),
               v_age = c(10, 25, 100),
               age_groups = FALSE)
```

    ## # A tibble: 6 x 7
    ##    year state       CVE_GEO sex      age population proportion
    ##   <int> <chr>         <dbl> <chr>  <int>      <dbl>      <dbl>
    ## 1  2000 Mexico City       9 Male      10      79634   0.497   
    ## 2  2000 Mexico City       9 Female    10      77915   0.475   
    ## 3  2000 Mexico City       9 Male      25      80613   0.503   
    ## 4  2000 Mexico City       9 Female    25      85947   0.524   
    ## 5  2000 Mexico City       9 Male     100         63   0.000393
    ## 6  2000 Mexico City       9 Female   100        138   0.000841

Now, if we use this function aggregating by age we get the next result:

``` r
# Get population data grouping by age
get_population(v_state = "Mexico City",
               v_year = 2000,
               v_sex = c("Female", "Male"),
               v_age = c(10, 25, 100),
               age_groups = TRUE)
```

    ## # A tibble: 6 x 7
    ##    year state       CVE_GEO sex    age_group population proportion
    ##   <int> <chr>         <dbl> <chr>  <fct>          <dbl>      <dbl>
    ## 1  2000 Mexico City       9 Female [10,25]      1319456  0.355    
    ## 2  2000 Mexico City       9 Female (25,100]     2392044  0.644    
    ## 3  2000 Mexico City       9 Female (100,Inf]        197  0.0000531
    ## 4  2000 Mexico City       9 Male   [10,25]      1283359  0.382    
    ## 5  2000 Mexico City       9 Male   (25,100]     2075376  0.618    
    ## 6  2000 Mexico City       9 Male   (100,Inf]         92  0.0000274

# Migration

This function allows the user to get information about
[international](https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/81802225-00c4-49b4-ab49-a491334062b4)
and
[interstate](https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/3f9c8392-153d-4299-a027-9f1b179f6edf)
migration in Mexico estimated by CONAPO. This data-set can give
information disaggregated by year (from 1970 to 2050), state, sex and
age (from 0 to 89 years). As most functions in the package, the
information retrieved by this function can be aggregated by age.

`get_population()` has a particular parameter called `v_type` that is
used to define the type of migration that will be present in the
data-set output. The available types of migration in this function are
*Interstate* migration, *International* migration and *Total* migration.

We can find an examples of how `get_population()` can be used with
different specifications.

``` r
# Get migration data without grouping by age and
# with both types of migration
get_migration(v_state = "Mexico City",
              v_year = 2010,
              v_sex = "Total",
              v_age = c(25, 50),
              v_type = c("Interstate", "International"),
              age_groups = FALSE)
```

    ## # A tibble: 4 x 11
    ##    year state   CVE_GEO sex     age emigrants immigrants type  migration em_rate
    ##   <int> <chr>     <dbl> <chr> <int>     <dbl>      <dbl> <chr>     <dbl>   <dbl>
    ## 1  2010 Mexico~       9 Total    25       279        317 Inte~        38 1.84e-3
    ## 2  2010 Mexico~       9 Total    25      2900       2149 Inte~      -751 1.91e-2
    ## 3  2010 Mexico~       9 Total    50        70        104 Inte~        34 6.46e-4
    ## 4  2010 Mexico~       9 Total    50      1001        458 Inte~      -543 9.24e-3
    ## # ... with 1 more variable: im_rate <dbl>

``` r
# Get migration data grouping by age and
# with both types of migration
get_migration(v_state = "Mexico City",
              v_year = 2010,
              v_sex = "Total",
              v_age = c(25, 50),
              v_type = c("Interstate", "International"),
              age_groups = TRUE)
```

    ## # A tibble: 4 x 11
    ##    year state       CVE_GEO sex   age_group type  emigrants immigrants migration
    ##   <int> <chr>         <dbl> <chr> <fct>     <chr>     <dbl>      <dbl>     <dbl>
    ## 1  2010 Mexico City       9 Total [25,50]   Inte~      4191       5281      1090
    ## 2  2010 Mexico City       9 Total [25,50]   Inte~     54514      30933    -23581
    ## 3  2010 Mexico City       9 Total (50,Inf]  Inte~       997       1127       130
    ## 4  2010 Mexico City       9 Total (50,Inf]  Inte~     13891       5420     -8471
    ## # ... with 2 more variables: em_rate <dbl>, im_rate <dbl>

Note that if `v_state = National` and `v_type = Interstate`, the
function will return an empty data-set because there is no interstate
migration at a National level.

``` r
# Get migration data without grouping by age
get_migration(v_state = "National",
              v_year = 2010,
              v_sex = "Total",
              v_age = c(0, 10, 15),
              v_type = "Interstate",
              age_groups = FALSE)
```

    ## # A tibble: 0 x 11
    ## # ... with 11 variables: year <int>, state <chr>, CVE_GEO <dbl>, sex <chr>,
    ## #   age <int>, emigrants <dbl>, immigrants <dbl>, type <chr>, migration <dbl>,
    ## #   em_rate <dbl>, im_rate <dbl>

# Mortality

This function allows us to get the estimation on [number of deaths made
by
CONAPO](https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/cb91fe7e-a49c-4b45-b1f7-705f32e1458c).
It works identically as `get_population()`. The user must define the
year(s), state(s), sex(es), age(s) to obtain the data. As well, there is
the option on whether the information should be grouped by age or not.

Ahead you will find some examples of how this function can be used

``` r
# Get mortality data without grouping by age
get_deaths(v_state = c("Guanajuato", "Nuevo Leon"),
           v_year = 2015,
           v_sex = c("Male", "Total"),
           v_age = c(15, 25, 35),
           age_groups = FALSE)
```

    ## # A tibble: 12 x 7
    ##     year state      CVE_GEO sex     age deaths death_rate
    ##    <int> <chr>        <dbl> <chr> <int>  <dbl>      <dbl>
    ##  1  2015 Guanajuato      11 Male     15     45   0.000781
    ##  2  2015 Guanajuato      11 Total    15     67   0.000587
    ##  3  2015 Guanajuato      11 Male     25    121   0.00239 
    ##  4  2015 Guanajuato      11 Total    25    156   0.00150 
    ##  5  2015 Guanajuato      11 Male     35    123   0.00320 
    ##  6  2015 Guanajuato      11 Total    35    170   0.00205 
    ##  7  2015 Nuevo Leon      19 Male     15     34   0.000732
    ##  8  2015 Nuevo Leon      19 Total    15     49   0.000540
    ##  9  2015 Nuevo Leon      19 Male     25    106   0.00231 
    ## 10  2015 Nuevo Leon      19 Total    25    131   0.00146 
    ## 11  2015 Nuevo Leon      19 Male     35    116   0.00303 
    ## 12  2015 Nuevo Leon      19 Total    35    153   0.00197

``` r
# Get mortality data grouping by age
get_deaths(v_state = c("Guanajuato", "Nuevo Leon"),
           v_year = 2015,
           v_sex = c("Male", "Total"),
           v_age = c(15, 25, 35),
           age_groups = TRUE)
```

    ## # A tibble: 12 x 7
    ##     year state      CVE_GEO sex   age_group deaths death_rate
    ##    <int> <chr>        <dbl> <chr> <fct>      <dbl>      <dbl>
    ##  1  2015 Guanajuato      11 Male  [15,25]      970    0.00159
    ##  2  2015 Guanajuato      11 Male  (25,35]     1230    0.00284
    ##  3  2015 Guanajuato      11 Male  (35,Inf]   14540    0.0154 
    ##  4  2015 Guanajuato      11 Total [15,25]     1299    0.00106
    ##  5  2015 Guanajuato      11 Total (25,35]     1625    0.00177
    ##  6  2015 Guanajuato      11 Total (35,Inf]   27028    0.0134 
    ##  7  2015 Nuevo Leon      19 Male  [15,25]      806    0.00155
    ##  8  2015 Nuevo Leon      19 Male  (25,35]     1124    0.00271
    ##  9  2015 Nuevo Leon      19 Male  (35,Inf]   13130    0.0138 
    ## 10  2015 Nuevo Leon      19 Total [15,25]     1041    0.00103
    ## 11  2015 Nuevo Leon      19 Total (25,35]     1426    0.00172
    ## 12  2015 Nuevo Leon      19 Total (35,Inf]   23662    0.0121

# Aging rate

The aging rate is the proportion of the population with age
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i")
that were able to live another year, in other words, the aging rate is
the division of the *aging population* between the *total population*.

![
aging\\ rate = \\frac{aging\\ population_i}{total\\ population_i}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Aaging%5C%20rate%20%3D%20%5Cfrac%7Baging%5C%20population_i%7D%7Btotal%5C%20population_i%7D%0A "
aging\ rate = \frac{aging\ population_i}{total\ population_i}
")

The aging population, as we wrote, is the number of people that is able
to live another period of time (in this case, another year) in an
specific location. Hence, to calculate the aging population in a
specific age
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i"),
we must add the immigrants and subtract the emigrants and deaths to the
total population of that age, as it is stated in the next equation.

![
aging\\ population = population_i + immigrants_i - emigrants_i - deaths_i
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Aaging%5C%20population%20%3D%20population_i%20%2B%20immigrants_i%20-%20emigrants_i%20-%20deaths_i%0A "
aging\ population = population_i + immigrants_i - emigrants_i - deaths_i
")

If the age is 0, then it is necessary to substitute the population of
age
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i")
by the births that occurred, in the specific time and location, to the
equation.

As we see, all the elements to get the aging population and the aging
rate are already at hand, so this function was created based on the
outputs of the rest of functions of `demogmx`. This function requires
that the user specifies the year, state, sex, and the age to filter the
output and, as most of the functions, the data can be aggregated by the
ages defined by the user in `v_age`.

Below you will find some examples of how this function can be used as
well as its outputs.

``` r
# Get mortality data without grouping by age
get_aging_rate(v_state = "National",
               v_year = seq(2000, 2010),
               v_sex = "Total",
               v_age = 47, 
               age_groups = FALSE)
```

    ## # A tibble: 11 x 7
    ##     year state    CVE_GEO sex     age aging_pop aging_rate
    ##    <int> <chr>      <dbl> <chr> <int>     <dbl>      <dbl>
    ##  1  2000 National       0 Total    47    841560      0.995
    ##  2  2001 National       0 Total    47    878597      0.995
    ##  3  2002 National       0 Total    47    917559      0.995
    ##  4  2003 National       0 Total    47    956589      0.995
    ##  5  2004 National       0 Total    47    996809      0.995
    ##  6  2005 National       0 Total    47   1038726      0.996
    ##  7  2006 National       0 Total    47   1079537      0.996
    ##  8  2007 National       0 Total    47   1119101      0.996
    ##  9  2008 National       0 Total    47   1157259      0.996
    ## 10  2009 National       0 Total    47   1193257      0.996
    ## 11  2010 National       0 Total    47   1227207      0.995

``` r
# Get aging data grouping by age
get_aging_rate(v_state = "National",
               v_year = seq(2000, 2010),
               v_sex = "Total",
               v_age = 47, 
               age_groups = TRUE)
```

    ## # A tibble: 11 x 7
    ##     year state    CVE_GEO sex   age_group aging_pop aging_rate
    ##    <int> <chr>      <dbl> <chr> <fct>         <dbl>      <dbl>
    ##  1  2000 National       0 Total [47,Inf]   15210170      0.980
    ##  2  2001 National       0 Total [47,Inf]   15778338      0.980
    ##  3  2002 National       0 Total [47,Inf]   16369935      0.980
    ##  4  2003 National       0 Total [47,Inf]   16986609      0.980
    ##  5  2004 National       0 Total [47,Inf]   17639719      0.981
    ##  6  2005 National       0 Total [47,Inf]   18328520      0.981
    ##  7  2006 National       0 Total [47,Inf]   19048301      0.981
    ##  8  2007 National       0 Total [47,Inf]   19786249      0.981
    ##  9  2008 National       0 Total [47,Inf]   20547323      0.981
    ## 10  2009 National       0 Total [47,Inf]   21319786      0.980
    ## 11  2010 National       0 Total [47,Inf]   22101186      0.980

**NOTA**: creo que especifqué mal la función the `get_aging_rate` cuando
la edad es cero.

# Get help

Finally, it is possible to get help of each of the `demogmx` functions
as any other package. You can go to the help tab and search for
information about a specific function or you can write `?` before the
function in the console as it is in the following code chunk.

``` r
# ?get_population
```
