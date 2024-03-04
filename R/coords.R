#' Cartesian coordinates with fixed "aspect ratio"
#'
#' A fixed scale coordinate system forces a specified ratio similar to
#' `coord_fixed`. It holds the coordinates fixed at the right ratio
#' to ensure each brick is of the right dimensions.
#'
#' @param bricks_per_layer Number of bricks per layer. Should match the `bricks_per_layer`
#' specification in `geom_brick`. Default is `4`.
#' @param ratio aspect ratio, expressed as `y / x`
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand If `TRUE`, the default, adds a small expansion factor to
#'   the limits to ensure that data and axes don't overlap. If `FALSE`,
#'   limits are taken exactly from the data or `xlim`/`ylim`.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. In most cases, the default of `"on"` should not be changed,
#'   as setting `clip = "off"` can cause unexpected results. It allows
#'   drawing of data points anywhere on the plot, including in the plot margins.
#' @name coord_brick
#' @export
#' @examples
#' # ensures that the ranges of axes are equal to the specified ratio by
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' # create a base plot
#' plt <- mpg %>%
#'   count(class, drv) %>%
#'   ggplot() +
#'   geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6)
#'
#' # view the base plot
#' plt
#'
#' # View the base plot with fixed coords
#' # Ensure `bricks_per_layer` matches the geom
#' plt %>%
#'   coord_brick(6)
#'
coord_brick <- function(bricks_per_layer = 4, ratio = NULL, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  if(is.null(ratio)) {
    d <- bricks_per_layer^2/4
    ratio <- 1/(d*10)
  }
  ggproto(NULL, CoordFixed,
          limits = list(x = xlim, y = ylim),
          ratio = ratio,
          expand = expand,
          clip = clip
  )
}

# CoordBrick
CoordBrick <- ggproto("CoordBrick", CoordCartesian,
                      is_free = function() FALSE,
                      aspect = function(self, ranges) {
                        diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
                      }
)

#' Cartesian coordinates with fixed "aspect ratio"
#'
#' @rdname coord_brick
#' @export
#' @param ratio aspect ratio, expressed as `y / x`
#' @examples
#' # The same using `geom_brick_waffle`
#' plt <- mpg %>%
#'   count(class, drv) %>%
#'   ggplot() +
#'   geom_brick_waffle(aes(class, n, fill = drv), bricks_per_layer = 6) +
#'   coord_brick_waffle(6)
coord_brick_waffle <- function(bricks_per_layer = 4, ratio = NULL, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  if(is.null(ratio)) {
    d <- bricks_per_layer^2/4
    ratio <- 1/(d*10)*2.5
  }
  ggproto(NULL, CoordFixed,
          limits = list(x = xlim, y = ylim),
          ratio = ratio,
          expand = expand,
          clip = clip
  )
}

# CoordBrickWaffle
CoordBrickWaffle <- ggproto("CoordBrickWaffle", CoordCartesian,
                      is_free = function() FALSE,

                      aspect = function(self, ranges) {
                        diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
                      }
)
