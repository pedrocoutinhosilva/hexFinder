#' Generates a default logo for a package.
#'
#' @description
#' Generates a generic logo for a package. IF the package is not on CRAN, a
#'   different set of colors is used.
#'
#' @param name The package to generate the logo for.
#' @param output the path to store the generated logo at.
#' @param colors A vector of two valid colors. [1] for the fill and [2] for
#'   the outline of the generated logo. Used if the package exists on CRAN.
#' @param fallback_colors A vector of two valid colors. [1] for the fill and
#'   [2] for the outline of the generated logo. Used if the package
#'   does not exist on CRAN.
#'
#' @importFrom ggplot2 ggplot theme_void
#' @importFrom hexSticker sticker
#' @importFrom glue glue
#'
#' @keywords images internal
#' @return The path to the generated logo image.
generate_hex <- function(name,
                         output,
                         colors = c("#1881C2", "#87B13F"),
                         fallback_colors = c("#a60000", "#360000")) {

  dir.create(output, showWarnings = FALSE, recursive = TRUE)
  output_path <- file.path(output, paste0(name, ".svg"))

  if (!url_file_exists(glue("https://github.com/cran/{name}"))) {
    log("Not a cran package")

    colors[1] <- fallback_colors[1]
    colors[2] <- fallback_colors[2]
  }

  font_size <- 5
  if (nchar(name) > 15) {
    font_size <- 4
  }

  sticker(
    ggplot() + theme_void(),
    package = name,
    p_size = font_size,
    p_x = 1,
    p_y = 1,
    s_width = 0,
    s_height = 0,
    h_size = 2.4,
    h_fill = colors[1],
    h_color = colors[2],
    filename = output_path
  ) |> suppressWarnings()

  output_path
}

#' Crops the empty space from a image
#'
#' @param path The path of the image to crop.
#'
#' @importFrom magick image_read image_trim image_write image_info
#'
#' @keywords images internal
#' @return No return, called for side effects.
crop_image <- function(path) {
  original <- image_read(path)
  trimmed <-  original |>
    image_trim()

  original_info <- image_info(original)
  trimmed_info <- image_info(trimmed)

  # if different size, crop it
  if (!identical(original_info$width, trimmed_info$width) ||
      !identical(original_info$height, trimmed_info$height)) {
    image_write(trimmed, path)
  }
}

#' Download a logo image
#'
#' @description
#' Downloads a given URL and stores it using the package name into
#' the specified output folder
#'
#' @param pkg_name The name of the file to save.
#' @param raw_url The image URL.
#' @param output The Folder path where to store the download file.
#' @param crop Should empty space be removed from the final image file.
#'
#' @importFrom tools file_ext
#' @importFrom utils download.file
#'
#' @keywords images internal
#' @return The path where the downloaded image was stored.
download_logo <- function(pkg_name, raw_url, output, crop = TRUE) {
  base_name <- basename(raw_url)
  filename <- paste0(pkg_name, ".", file_ext(base_name))
  output_path <- file.path(output, filename)

  download.file(raw_url, output_path, quiet = TRUE, mode = "wb")

  # If crop is enable, see if the image needs cropping and crop it
  if (crop) {
    crop_image(output_path)
  }

  return(output_path)
}