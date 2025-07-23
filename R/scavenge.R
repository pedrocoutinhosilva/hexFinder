#' Finds hex logos for given packages
#'
#' @description
#' Finds and downloads the best possible image that represents a hex logo for
#' a given list of package names.
#'
#' @param pkg_names A single string or a vector of strings with the names of
#'   packages to find hexes for.
#' @param output the output path where to store the found logos
#' @param repo The repo to use to find the package logo. If none is
#'   provided, it will try to find one based on CRAN meta information.
#'   Useful when we want to minimize API calls to CRAN.
#' @param skip_known_logos Some logos are sourced from known repos or urls.
#'   If set to TRUE, those locations will be skipped. Useful if you are getting
#'   outdated versions of logos, but slower. Defaults to FALSE.
#' @param overwrite If a logo with the package name already exists in the
#'   output folder, should it be overwritten. Defaults to FALSE.
#' @param colors A vector of two valid colors. [1] for the fill and [2] for
#'   the outline of the generated logo. Used if the package exists on CRAN.
#' @param fallback_colors A vector of two valid colors. [1] for the fill and
#'   [2] for the outline of the generated logo. Used if the package
#'   does not exist on CRAN.
#'
#' @keywords finder external
#' @return No return, called for side effects.
#' @export
find_hex <- function(pkg_names,
                     output = NULL,
                     repo = NULL,
                     skip_known_logos = FALSE,
                     overwrite = FALSE,
                     colors = c("#1881C2", "#87B13F"),
                     fallback_colors = c("#a60000", "#360000")) {
  if (is.null(output)) {
    output <- tempdir()
    log("No output folder provided, saving in temp folder")
  } else {
    dir.create(output, showWarnings = FALSE, recursive = TRUE)
  }

  pkg_names |>
    purrr::map(\(pkg_name) {
      # check if logo exists and overwrite is disabled
      if (!overwrite) {
        png_path <- file.path(output, paste0(pkg_name, ".png"))
        if (file.exists(png_path)) {
          log("Logo already exists for", pkg_name)

          return(png_path)
        }

        svg_path <- file.path(output, paste0(pkg_name, ".svg"))
        if (file.exists(svg_path)) {
          log("Logo already exists for", pkg_name)

          return(svg_path)
        }
      }

      if (!skip_known_logos) {
        # check for known packages
        known_url <- check_known_packages(pkg_name)
        if (!is.null(known_url)) {
          path <- download_logo(pkg_name, known_url, output)
          log("Downloaded known hex for", pkg_name)

          return(path)
        }

        # check in known repos
        known_repo_url <- check_known_repos(pkg_name)
        if (!is.null(known_repo_url)) {
          path <- download_logo(pkg_name, known_repo_url, output)
          log("Downloaded from known repo hex for", pkg_name)

          return(path)
        }
      }

      # check on CRAN for repos, if given as arg, use the given repo instead
      if (is.null(repo)) {
        repo <- locate_repo(pkg_name)
      }
      unknown_repo_url <- search_repo_logo(pkg_name, repo)

      if (!is.null(unknown_repo_url)) {
        path <- download_logo(pkg_name, unknown_repo_url, output)
        log("Downloaded from GitHub repo hex for", pkg_name)

        return(path)
      }

      # Nothing was found, generate new one
      path <- generate_hex(pkg_name, output)
      log("No logo found, generated hex for", pkg_name)

      return(path)
    }) |>
    unlist()
}

#' Same as [find_hex], but with extra raccoons
#'
#' @param ... All arguments that can be passed to [find_hex].
#'
#' @importFrom methods getPackageName
#'
#' @keywords finder external
#' @return No return, called for side effects.
#' @export
scavenge <- function(...) {
  log("Summoning racoons to help with the search...")
  file.path(system.file(package = getPackageName()), "ascii.txt") |>
    readLines() |>
    cat(sep = "\n")

  find_hex(...)
}
