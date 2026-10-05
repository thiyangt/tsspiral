## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

usethis::use_gpl3_license()
usethis::use_package("rlang")


# Packagedown
# Run once to configure your package to use and deploy pkgdown
usethis::use_pkgdown_github_pages()
# Preview your site locally before publishing
pkgdown::build_site()
