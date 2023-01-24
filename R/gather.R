#' Returns CRAN information about a given package
#'
#' @param pkg_name A single string with the name of a package to find
#'   the meta information for.
#'
#' @importFrom pkgsearch cran_package
#'
#' @keywords gather internal
#' @return A named list with CRAN metadata about the package
get_cran_info <- function(pkg_name) {
  cran_package(pkg_name)
}

#' Returns metacran information about a given package
#'
#' @param pkg_name A single string with the name of a package to find
#'   the meta information for.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#'
#' @keywords gather internal
#' @return A named list with metadata information about the package
get_metacran_info <- function(pkg_name) {
  metacran <- fromJSON(
    glue("https://search.r-pkg.org/package/_search?q=Package:{pkg_name}")
  )

  metacran$hits$hits$`_source`
}

#' Fetch the content of a endpoint. Multiple branches can be given, the content
#' returned will also include wich was the valid branch.
#'
#' @param endpoint The github endpoint to search
#' @param branchs The branches to search. Defaults to main and master.
#' @param token A github personal access token. Defaults to the enviromental
#'
#' @importFrom httr GET add_headers content
#' @importFrom glue glue
#'
#' @keywords gather internal
#' @return A named list with the branch name and the repo content
get_endpoint_content <- function(endpoint,
                                 branchs = c("master", "main"),
                                 token) {
  responses <- branchs |>
    map(\(branch) {
      query_endpoint <- glue("{endpoint}/git/trees/{branch}?recursive=1",
        endpoint = endpoint
      )

      if (token != "") {
        request <- GET(
          query_endpoint,
          add_headers(Authorization = paste("Bearer", token, sep = " "))
        )
      } else {
        request <- GET(
          query_endpoint
        )
      }

      if (request$status_code != 200) {
        return(NULL)
      }

      list(
        content = httr::content(request, "parsed"),
        branch = branch
      )
    }) |>
    keep(\(response) {
      if (is.null(response$content) || is.null(response$content$tree)) {
        return(FALSE)
      }

      return(TRUE)
    }) |>
    map(\(response) {
      list(
        content = response$content$tree,
        branch = response$branch
      )
    })

  # if no branchs are valid, abort
  if (length(responses) < 1) {
    return(NULL)
  }

  # if the found branch is not valid, abort
  if (length(responses[[1]]$content) < 1) {
    return(NULL)
  }

  responses[[1]]
}

#' Search the given content for paths that match the constrains from
#'   ignore_patterns and logo_patterns.
#'
#' @param content The result of calling get_endpoint_content() on a github repo
#' @param ignore_patterns A vector of patterns to ignore
#' @param logo_patterns A vector of patterns to ignore
#'
#' @importFrom glue glue
#'
#' @keywords gather internal
#' @return A list of paths that match the given patterns
get_possible_paths <- function(content,
                               ignore_patterns,
                               logo_patterns,
                               pkg_name) {
  paths <- content |>
    purrr::keep(\(entry) {
      if (is.null(names(entry))) {
        return(FALSE)
      }

      if (is.null(entry$path)) {
        return(FALSE)
      }

      ignore_patterns <- ignore_patterns |>
        paste0(collapse = "|")

      if (grepl(ignore_patterns, entry$path, fixed = FALSE)) {
        return(FALSE)
      }

      if (entry$type != "blob") {
        return(FALSE)
      }

      logo_patterns <- logo_patterns |>
        paste0(collapse = "|") |>
        glue(pkg_name = pkg_name)

      grepl(logo_patterns, entry$path, fixed = FALSE)
    })
}

#' Returns the best image path from a list of image paths
#'
#' @param paths A list of path information
#' @param download_endpoint The download endpoint for the repo
#' @param branch The repo active branch
#'
#' @importFrom glue glue
#' @importFrom purrr map
#'
#' @keywords gather internal
#' @return A single url path
get_best_image <- function(paths, download_endpoint, branch) {
# keep only good aspect rations
  ratios <- purrr::map(paths, \(entry) {
    info <- image_read(glue("{download_endpoint}/{branch}/{entry$path}")) |>
        magick::image_info()

    dimentions <- c(info$width, info$height) |>
      sort()

    (dimentions[2] / dimentions[1])
  }) |>
  unlist()

  # find path with the closest aspect ratio to perfect ratio
  ideal_ratio <- 1.153
  path <- paths[[which.min(abs(ratios - ideal_ratio))]]$path

  return(glue::glue("{download_endpoint}/{branch}/{path}"))
}

#' Filters a list of image entries keeping images that might be a logo
#'
#' @param paths A list of path information
#' @param download_endpoint The download endpoint for the repo
#' @param branch The repo active branch
#'
#' @importFrom glue glue
#' @importFrom purrr keep
#' @importFrom magick image_read image_trim image_info image_write
#'
#' @keywords gather internal
#' @return A list of path information
keep_good_ratio_images <- function(paths, download_endpoint, branch) {
  purrr::keep(paths, \(entry) {
    # trim white space
    info <- glue("{download_endpoint}/{branch}/{entry$path}") |>
      image_read() |>
      image_trim() |>
      magick::image_info()

    dimentions <- c(info$width, info$height) |>
      sort()

    aspect_ratio <- dimentions[2] / dimentions[1]

    # discard way off aspect ratios
    if (aspect_ratio > 1.8) {
      return(FALSE)
    }

    # use perfect ratio image
    if (aspect_ratio > 1.151 && aspect_ratio < 1.154) {
      return(TRUE)
    }

    # discard really large images unless perfect ratio
    if (info$width > 1279 || info$height > 1279) {
      return(FALSE)
    }

    return(TRUE)
  })
}

#' Searches a given github repo URL for the best hex logo image.
#'
#' @param pkg_name The mane of the package we want the logo for.
#' @param repository The github repository to search in.
#' @param token A github personal access token. Defaults to the enviromental
#'   variable github_pat. If that variable is not set, you might run into API
#'   limits when running too many queries.
#' @param logo_patterns String of valid name.extention file names for files to
#'   look for, seperated by |. {pkg_name} Can be used as a placeholder for
#'   the package name.
#' @param ignore_patterns String of patterns to ignore when looking for a valid
#'   logo. Can be part of the filename or part of the file path.
#'
#' @importFrom stringr str_replace
#' @importFrom glue glue
#' @importFrom httr GET content add_headers
#' @importFrom stringr str_replace
#' @importFrom purrr keep
#' @importFrom magick image_read image_trim image_info image_write
#'
#' @keywords gather internal
#' @return A URL to a image or NULL if no image was found
search_repo_logo <- function(pkg_name,
                             repository,
                             token = Sys.getenv("github_pat"),
                             logo_patterns = getOption("hexFinder.logo_patterns"), #nolint
                             ignore_patterns = getOption("hexFinder.ignore_patterns")) { #nolint

  # Warn user about github pat. Trigers once per session
  if (token == "" && getOption("hexFinder.pat_warning_first_time")) {
    log("No github personal access token provided.")
    log("Limited search rates for github will apply.")
    log("Set up github_pat enviromental variable if you plan to query multiple repos in a short time") #nolint

    options(hexFinder.pat_warning_first_time = FALSE)
  }

  # if no valid repo is given, abort
  if (length(repository) == 0 || repository == "") {
    return(NULL)
  }

  # api endpoint based on given repo
  endpoint <- repository |>
    str_replace(
      "github.com",
      "api.github.com/repos"
    )

  response <- get_endpoint_content(endpoint, token = token)

  # if the found branch is not valid, abort
  if (is.null(response)) {
    return(NULL)
  }

  paths <- get_possible_paths(
    response$content,
    ignore_patterns,
    logo_patterns,
    pkg_name
  )

  download_endpoint <- repository |> str_replace(
    "github.com",
    "raw.githubusercontent.com"
  )

  branch <- response$branch

  # keep only good aspect rations
  paths <- keep_good_ratio_images(paths, download_endpoint, branch)

  # bail if no images
  if (is.null(paths)) {
    return(NULL)
  }

  if (length(paths) > 0) {
    path <- get_best_image(paths, download_endpoint, branch)

    return(path)
  }

  return(NULL)
}
