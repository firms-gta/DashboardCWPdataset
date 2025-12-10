cran_pkgs <- getsysreqs::extract_cran_packages("renv.lock")

libs <- getsysreqs::get_sysreqs(
  packages     = cran_pkgs,
  distribution = "ubuntu",
  release      = "20.04"
)

unique(libs)