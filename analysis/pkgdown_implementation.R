

library(pkgdown)
library(usethis)
library(rmarkdown)


# use_build_ignore("pkgdown")

# Configure package to use pkgdown
usethis::use_pkgdown()

# Build the package website
pkgdown::build_site()

# Check navigation bar template
template_navbar()

# To create favicons
# https://pkgdown.r-lib.org/reference/build_favicons.html
build_favicon(pkg = ".", overwrite = TRUE)

# Build home page
pkgdown::build_home()
