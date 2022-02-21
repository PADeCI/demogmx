################################################################################
# This script generates employs the get_migration() function                  #
#                                                                              #
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              #
#     - David Garibay-Treviño, MSc                                             #
################################################################################
# The structure of this code follows DARTH's coding framework                  #
# https://github.com/DARTH-git/darthpack                                       #
################################################################################

rm(list = ls()) # clean the workspace

# 05.1 Load packages and functions ----------------------------------------
## 05.1.1 Load packages ---------------------------------------------------
library(demogmx)

## 05.1.2 Load functions --------------------------------------------------
# no functions required

# 05.2 Execute function ---------------------------------------------------
df_outcome <- get_migration(v_state   = "Aguascalientes",
                            v_year     = c(2000, 2010, 2025),
                            v_sex      = c("Male", "Female", "Total"),
                            v_age      = c(0, 15, 24, 36),
                            v_type     = c("Interstate", "International", "Total"),
                            age_groups = FALSE)


df_outcome

