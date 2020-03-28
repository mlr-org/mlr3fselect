# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_ghactions()) {
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  # creates pkgdown site and pushes to gh-pages branch
  do_pkgdown()
}
