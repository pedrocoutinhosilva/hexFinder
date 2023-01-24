# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

.onLoad <- function(libname, pkgname) {
  # Patterns used to match hex logos
  logo_patterns <- c(
    "logo.svg",
    "{pkg_name}.svg",
    "logo.png",
    "{pkg_name}.png",
    "hexsticker.svg",
    "hexsticker.png"
  ) |> paste0(collapse = "|")

  # Patterns to ignore. Often result in false matches
  ignore_patterns <- c(
    "examples",
    "src",
    "tools",
    "shinyAce",
    "Rlogo",
    "r_logo"
  )

  # Some packages logos are not stored in traditional ways.
  # Links to those are stored manually
  known_packages <- list(
    arrow = "https://arrow.apache.org/img/{package}-logo_hex_black-txt_white-bg.png" #nolint
  )

  # Curated repositories of hex packages exist, and often have high quality
  # versions that we can use.
  known_repos <- c(
    "https://raw.githubusercontent.com/rstudio/hex-stickers/main/SVG/{package}.svg", #nolint
    "https://raw.githubusercontent.com/maxogden/hexbin/gh-pages/vector/{package}.svg", #nolint
    "https://raw.githubusercontent.com/maxogden/hexbin/gh-pages/hexagons/{package}.png" #nolint
  )

  options(hexFinder.logo_patterns = logo_patterns)
  options(hexFinder.ignore_patterns = ignore_patterns)
  options(hexFinder.known_packages = known_packages)
  options(hexFinder.known_packages = known_repos)

  options(hexFinder.pat_warning_first_time = TRUE)
}