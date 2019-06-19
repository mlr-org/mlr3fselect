do_package_checks(error_on = "error")

if (ci_on_travis()) {
  do_pkgdown(orphan = TRUE, document = FALSE)
}
