################################################################################
# This script generates employs the get_births_INEGI() function                #
#                                                                              #
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              #
#     - David Garibay-Treviño, MSc                                             #
################################################################################
# The structure of this code follows DARTH's coding framework                  #
# https://github.com/DARTH-git/darthpack                                       #
################################################################################

rm(list = ls()) # clean the workspace

# 06.1 Load packages and functions ----------------------------------------
## 06.1.1 Load packages ---------------------------------------------------
library(demogmx)

## 06.1.2 Load functions --------------------------------------------------
# no functions required

# 06.2 Execute function ---------------------------------------------------
df_births_out_INEGI <- get_births_INEGI(v_state = c("National", "Mexico City"),
                               v_year = c(2010, 2015, 2020),
                               v_sex = c("Total", "Female"),
                               year_groups = TRUE)

df_births_out_INEGI
