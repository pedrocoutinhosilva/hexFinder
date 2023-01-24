#' Seaches a list of known package hex links for a given package name
#'
#' @description
#' Some packages have logos but are not tied to CRAN in any way. These are
#'   stored as special cases.
#'
#' @param pkg_name A single string with the name of a package to find
#'   the hex for.
#' @param known_packages Named list with known hex locations for some packages.
#'
#' @importFrom glue glue
#'
#' @keywords locator internal
#' @return A URL that points to a image or NULL in no result was found.
check_known_packages <- function(pkg_name,
                                 known_packages = getOption("hexFinder.known_packages")) { #nolint

  if (pkg_name %in% names(known_packages)) {
    raw_url <- glue(known_packages[[pkg_name]], package = pkg_name)

    return(raw_url)
  }

  return(NULL)
}

#' Seaches a list of known package hex curated lists for a given package name
#'
#' @description
#' Some packages have logos already in curated repositories. This avoids
#'   additional CRAN calls and returns high definition versions of the logos.
#'
#' @param pkg_name A single string with the name of a package to find
#'   the hex for.
#' @param known_packages Named list with known curated repos that store logos
#'   for some packages. quality on these is usually higher that normal.
#'
#' @importFrom glue glue
#'
#' @keywords locator internal
#' @return A URL that points to a image or NULL in no result was found.
check_known_repos <- function(pkg_name,
                              known_repos = getOption("hexFinder.known_repos")) { #nolint

  for (template in known_repos) {
    raw_url <- glue(template, package = pkg_name)

    if (url_file_exists(raw_url)) {
      return(raw_url)
    }
  }

  return(NULL)
}

#' Finds the best repo for a given package
#'
#' @description
#' All CRAN packages have a cloned github repo that we can query for logos,
#'   but CRAN only stores the actual content of the package file. By looking
#'   at meta CRAN links we can try to find the original repo and use that one
#'   instead.
#'
#' @param pkg_name A single string with the name of a package to find
#'   the repo for.
#'
#' @importFrom glue glue
#'
#' @keywords locator internal
#' @return A URL that points to a github repository.
locate_repo <- function(pkg_name) {
  # get cran info
  cran_info <- pkg_name |>
    get_cran_info() |>
    catch()

  # pick best from links
  matches <- clean_repo_links(cran_info$URL, cran_info$BugReports)
  if (!is.null(matches)) {
    return(matches[1])
  }

  # get metacran info
  metacran_info <- pkg_name |>
    get_metacran_info() |>
    catch()

  # pick best from links
  matches <- clean_repo_links(cran_info$URL, cran_info$BugReports)
  if (!is.null(matches)) {
    return(matches[1])
  }

  # use cran repo
  return(glue("https://github.com/cran/{pkg_name}"))
}