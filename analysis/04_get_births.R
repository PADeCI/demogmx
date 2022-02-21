################################################################################
# This script generates employs the get_population() function                  #
#                                                                              #
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              #
#     - David Garibay-Treviño, MSc                                             #
################################################################################
# The structure of this code follows DARTH's coding framework                  #
# https://github.com/DARTH-git/darthpack                                       #
################################################################################

rm(list = ls()) # clean the workspace

# 04.1 Load packages and functions ----------------------------------------
## 04.1.1 Load packages ---------------------------------------------------
library(demogmx)

## 04.1.2 Load functions --------------------------------------------------
# no functions required

# 04.2 Execute function ---------------------------------------------------
df_birth_outcome <- get_births(v_state = c("National", "Mexico City"),
                         v_year = c(2010, 2015, 2020),
                         year_groups = FALSE)


df_birth_outcome
