################################################################################
# This script generates employs the get_death_population() function            #
#                                                                              #
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              #
#     - David Garibay-Treviño, MSc                                             #
################################################################################
# The structure of this code follows DARTH's coding framework                  #
# https://github.com/DARTH-git/darthpack                                       #
################################################################################

rm(list = ls()) # clean the workspace

# 01.1 Load packages and functions ----------------------------------------
## 01.1.1 Load packages ---------------------------------------------------
library(demogmx)

## 01.1.2 Load functions --------------------------------------------------
# no functions required

# 01.2 Execute function ---------------------------------------------------
df_outcome <- get_death_population(v_states = c("National", "Mexico City"),
                                   v_year = c(2010, 2015, 2020),
                                   v_sex = c("Total", "Female"),
                                   v_age = c(0, 15, 45, 70),
                                   age_groups = TRUE)


View(df_outcome)
