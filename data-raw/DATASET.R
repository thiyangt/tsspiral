## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

usethis::use_gpl3_license()
usethis::use_package("rlang")


# Packagedown
# Run once to configure your package to use and deploy pkgdown
usethis::use_pkgdown_github_pages()
# Preview your site locally before publishing
pkgdown::build_site()


# Reading tourism dataset
library(tidyverse)
library(here)
library(readxl)

tourism.srilanka <- read_excel("data-raw/Tourismdata.xlsx")
View(tourism.srilanka)
usethis::use_data(tourism.srilanka, overwrite = TRUE)
colnames(tourism.srilanka)
