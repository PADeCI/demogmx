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

# 03.1 Load packages and functions ----------------------------------------
## 03.1.1 Load packages ---------------------------------------------------
library(demogmx)

## 03.1.2 Load functions --------------------------------------------------
# no functions required

# 03.2 Execute function ---------------------------------------------------
df_pop_outcome <- get_population(v_state = c("National", "Mexico City"),
                             v_year = c(2010, 2015, 2020),
                             v_sex = c("Total", "Female"),
                             v_age = c(0, 15, 45, 70),
                             age_groups = TRUE)


df_pop_outcome

