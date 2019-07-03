do_package_checks(error_on = "error")

# install ranger for README
get_stage("install") %>%
  add_step(step_install_cran("ranger"))

if (ci_on_travis()) {
  do_pkgdown(orphan = TRUE, document = FALSE)
}
