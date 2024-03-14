
### Define colour palettes -----------------------------------------------------
dave_colours <-  list(
  thuenen_all = c("#008CD2", "#00A0E1", "#00AAAA", "#00AA82",
                  "#78BE1E", "#37464B", "#4B3228", "#AF0A19",
                  "#E10019", "#E17D00"),
  thuenen_primary = c("#008CD2", "#00A0E1", "#00AAAA", "#00AA82",
                      "#78BE1E"),
  thuenen_secondary = c("#37464B", "#4B3228", "#AF0A19", "#E10019",
                        "#E17D00"),
  # daves_faves_old = c("#ffbf49", "#3faeb8", "#024B7A"),
  daves_faves = c("#045275", "#089099", "#7CCBA2", "#ffd579",
                  "#F0746E", "#DC3977", "#7C1D6F"),
  daves_faves_b_y = c("#045275", "#089099", "#7CCBA2", "#ffd579"),
  daves_faves_p_y = c("#7C1D6F", "#DC3977", "#F0746E", "#ffd579")
)


### Helper functions -----------------------------------------------------------
## Palette collection
daves_palettes <- function(palette,
                           n,
                           all_palettes = dave_colours,
                           type = c("discrete", "continuous"),
                           direction = c("foreward", "reverse")) {

  palette <- all_palettes[[palette]]

  if (missing(n)) {
    n <- length(palette)
  }

  type <- match.arg(type)
  out <- switch(type,
                continuous = grDevices::colorRampPalette(palette)(n),
                discrete = palette[1:n])

  structure(out, palette = palette, class = "palette")

  direction <- match.arg(direction)
  out <- switch(direction,
                foreward = out,
                reverse = rev(out))
}

## Rescale colour palette around a midpoint
mid_rescaler <- function(mid) {
  if (!is.na(mid)) {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to, from, mid)
    }
  } else {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale(x, to, from)
    }
  }
}

## Input check
# Combine checks
input_check <- function(palette, direction, midcol) {
  palette_name_check(palette)
  palette_direction_check(direction)
  mid_palette_col_check(midcol)
}

# Check palete name argument helper
palette_name_check <- function(palette) {
  if (!palette %in% names(dave_colours))
    stop(paste0("Incorrect value for 'palette' agrument. ",
                "Correct values include: ",
                paste0(paste0("'", names(dave_colours), "'"), collapse = ", "),
                "."))
}

# Check direction argument helper
palette_direction_check <- function(direction) {
  if (!direction %in% c("foreward", "reverse"))
    stop("Incorrect value for 'dir' argument, ",
         "please select 'foreward' or 'reverse'. Default value is 'foreward'.")
}

mid_palette_col_check <- function(midcol) {
  if (!is.na(midcol))
    tryCatch(col2rgb(midcol), error = function(e) FALSE)
}

### Palette functions ----------------------------------------------------------

#' Discrete outline palettes
#'
#' Apply colour palettes to plot outlines
#' @param palette Name of the colour palette: 'thuenen_all', 'thuenen_primary', 'thuenen_secondary', 'daves_faves'
#' @param direction Direction of the colour palette: 'forward' or 'reverse'
#' @return Colour palette
#' @examples
#' ggplot(dat, aes(x, y)) + scale_colour_dave_d("daves_faves");
#' ggplot(dat, aes(x, y)) + scale_colour_dave_d("thuenen_primary", direction = "reverse");
#' @export
scale_colour_dave_d <- function(palette, direction = "foreward") {

  input_check(palette, direction)

  ggplot2::scale_colour_manual(values = daves_palettes(palette,
                                                       type = "discrete",
                                                       direction = direction),
                               na.value = "transparent")

}

#' Discrete fill palettes
#'
#' Apply colour palettes to plot fills
#' @param palette Name of the colour palette: 'thuenen_all', 'thuenen_primary', 'thuenen_secondary', 'daves_faves'
#' @param direction Direction of the colour palette: 'forward' or 'reverse'
#' @return Colour palette
#' @examples
#' ggplot(dat, aes(x, y)) + scale_fill_dave_d("daves_faves");
#' ggplot(dat, aes(x, y)) + scale_fill_dave_d("thuenen_primary", direction = "reverse");
#' @export
scale_fill_dave_d <- function(palette, direction = "foreward") {

  input_check(palette, direction)

  ggplot2::scale_fill_manual(values = daves_palettes(palette,
                                                     type = "discrete",
                                                     direction = direction),
                             na.value = "transparent")

}

#' Continuous outline palettes
#'
#' Apply colour palettes to plot outlines
#' @param palette Name of the colour palette: 'thuenen_all', 'thuenen_primary', 'thuenen_secondary', 'daves_faves'
#' @param direction Direction of the colour palette: 'forward' or 'reverse'
#' @return Colour palette
#' @examples
#' ggplot(dat, aes(x, y)) + scale_colour_dave_c("daves_faves");
#' ggplot(dat, aes(x, y)) + scale_colour_dave_c("thuenen_primary", direction = "reverse");
#' @export
scale_colour_dave_c <- function(palette,
                                direction = "foreward",
                                mid = NA,
                                midcol = NA) {

  input_check(palette, direction, midcol)

  palette <- daves_palettes(palette,
                            type = "continuous",
                            direction = direction)

  if (!is.na(midcol)) {
    palette[ceiling(length(palette) / 2)] <- midcol
  }

  ggplot2::scale_colour_gradientn(colours = palette,
                                  rescaler = mid_rescaler(mid),
                                  na.value = "transparent")

}

#' Continuous fill palettes
#'
#' Apply colour palettes to plot fills
#' @param palette Name of the colour palette: 'thuenen_all', 'thuenen_primary', 'thuenen_secondary', 'daves_faves'
#' @param direction Direction of the colour palette: 'forward' or 'reverse'
#' @return Colour palette
#' @examples
#' ggplot(dat, aes(x, y)) + scale_fill_dave_c("daves_faves");
#' ggplot(dat, aes(x, y)) + scale_fill_dave_c("thuenen_primary", direction = "reverse");
#' @export
scale_fill_dave_c <- function(palette,
                              direction = "foreward",
                              mid = NA,
                              midcol = NA) {

  input_check(palette, direction)

  palette <- daves_palettes(palette,
                            type = "continuous",
                            direction = direction)

  # If midcol is not NA, replace the middle colour with midcol
  if (!is.na(midcol)) {
    palette[ceiling(length(palette) / 2)] <- midcol
  }

  ggplot2::scale_fill_gradientn(colours = palette,
                                rescaler = mid_rescaler(mid),
                                na.value = "transparent")

}

### Account for American/Canadian spelling
scale_color_dave_d <- scale_colour_dave_d
scale_color_dave_c <- scale_colour_dave_c
