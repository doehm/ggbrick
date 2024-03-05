utils::globalVariables(c("xmin", "xmax", "ymin", "ymax", 'brick_type', 'brick_type_cm',
                         'tail', 'dnorm', "new_x", "val_cm"))

#' stat_brick
#'
#' @param geom Geom
#'
#' @rdname brick
stat_brick_waffle <- function(mapping = NULL, data = NULL,
                       geom = "rect", position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, brick_layers = 100,
                       bricks_per_layer = 4, type = "ordered",
                       gap = NULL, ...) {
  layer(
    stat = StatBrickWaffle,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      brick_layers = brick_layers,
      bricks_per_layer = bricks_per_layer,
      type = type,
      gap = gap,
      na.rm = na.rm,
      ...
    )
  )
}

# StatBrickWaffle
StatBrickWaffle <- ggproto(
  "StatBrickWaffle",
  Stat,
  required_aes = c("x", "y"),
  setup_params = function(data, params) {

   dat_1 <- data %>%
     group_by(x) %>%
     summarise(
       y = sum(y),
       .groups = "drop"
       )

   if(max(dat_1$y) > 1000*params$bricks_per_layer*params$brick_layers) {
     params$r <- (params$bricks_per_layer*params$brick_layers)/max(dat_1$y)
     message("Number of bricks has been scaled to a maximum of ",
             params$bricks_per_layer*params$brick_layers,
             " bricks. 1 brick equals ", round(1/params$r, 1),
             " units.\nTo adjust, increase the number of 'brick_layers' and/or 'bricks_per_layer'")
   } else {
     params$r <- 1
   }

   return(params)
  },
  compute_panel = function(data, scales, brick_layers = params$brick_layers,
                          bricks_per_layer = params$bricks_per_layer,
                          type = params$type, r = params$r, gap = params$gap
                          ) {

    message_bank <- new.env()
   dat_1 <- data %>%
     group_by(x, PANEL) %>%
     summarise(y = sum(y), .groups = "drop") %>%
     mutate(y = robust_round(r*y, round(sum(r*y))))

   do_fill <- "fill" %in% colnames(data)

   dat_out <- NULL
   for(k in 1:nrow(dat_1)) {
     x <- build_wall_by_brick_waffle(dat_1$y[k], bricks_per_layer, r = r, gap = gap) %>%
       mutate(
         x = dat_1$x[k],
         y = dat_1$y[k],
         xmin = xmin + (dat_1$x[k]-1),
         xmax = xmax + (dat_1$x[k]-1),
         PANEL = dat_1$PANEL[k]
       )

     if(do_fill) {
       ids <- which(data$x == dat_1$x[k])
       fill_levels <- data$fill[ids]
       n_of_levels <- robust_round(data$y[ids]*r, sum(x$brick_type))

       x$fill <- make_new_fill(fill_levels, n_of_levels, x$brick_type)
       x$fill <- switch(
         type,
         "ordered" = x$fill,
         "soft_random" = switch_pos(x$fill, floor(nrow(x)/2)),
         "random" = robust_random(x$fill, x$brick_type)
       )
     }

     dat_out <- rbind(dat_out, x)
   }

   # print messages
   # browser()
   # message(message_bank$gap)

   dat_out$y <- max(dat_out$ymax)

   return(dat_out)
  }
)

#' GeomBrick
GeomBrickWaffle <- ggproto(
  "GeomBrickWaffle",
  GeomRect,
  default_aes = aes(
   colour = "black",
   fill = "grey30",
   linewidth = 0.5,
   linetype = 1,
   alpha = NA
  ),
  brick_layers = 100,
  bricks_per_layer = 4,
  type = "ordered",
  gap = NULL
)

#' Brick chart
#'
#' Creates a 'waffle' style chart with the aesthetic of a brick wall. Usage is
#' similar to `geom_col` where you supply counts as the height of the bar. Each
#' whole brick represents 1 unit. Two half bricks equal one whole brick. Where
#' the count exceeds the number of brick layers, the number of bricks is scaled
#' to retain the brick wall aesthetic.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this
#'    layer, either as a `ggproto` `Geom` subclass or as a string naming the
#'    stat stripped of the `stat_` prefix (e.g. `"count"` rather than
#'    `"stat_count"`)
#' @param position Position adjustment, either as a string naming the adjustment
#'   (e.g. `"jitter"` to use `position_jitter`), or the result of a call to a
#'   position adjustment function. Use the latter if you need to change the
#'   settings of the adjustment.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param na.rm If `FALSE` removes `NA`s from the data.
#' @param brick_layers The number of brick layers. Default is the height of the column divded by
#' the number of bricks per layer.
#' @param bricks_per_layer The number of bricks per layer. Default 4.
#' @param type The type of fill ordering. one of 'ordered', 'random' or 'soft_random', Default 'ordered'
#' @param gap The space between bricks.
#' @param ... Dots.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom purrr map_dfr
#' @importFrom stats dnorm
#' @importFrom utils tail
#' @importFrom glue glue
#'
#' @return Grob
#' @export
#'
#' @name brick
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' mpg %>%
#'   count(class, drv) %>%
#'   ggplot() +
#'   geom_brick_waffle(aes(class, n, fill = drv))
geom_brick_waffle <- function(mapping = NULL, data = NULL, stat = "brick_waffle",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE,
                       brick_layers = 100, bricks_per_layer = 4,
                       type = "ordered", gap = NULL, ...) {
  layer(
    geom = GeomBrickWaffle,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      brick_layers = brick_layers,
      bricks_per_layer = bricks_per_layer,
      type = type,
      gap = gap,
      na.rm = na.rm,
      ...)
  )
}

#' @export
#' @rdname brick
geom_brick_waffle0 <- function(mapping = NULL, data = NULL, stat = "brick_waffle",
                               position = "identity", na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE,
                               brick_layers = 100, bricks_per_layer = 4,
                               type = "ordered", gap = 0, ...) {
  layer(
    geom = GeomBrickWaffle,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      brick_layers = brick_layers,
      bricks_per_layer = bricks_per_layer,
      type = type,
      gap = gap,
      na.rm = na.rm,
      ...)
  )
}

#' GeomBrick
GeomBrickWaffle0 <- ggproto(
  "GeomBrickWaffle0",
  GeomBrickWaffle,
  default_aes = aes(
    colour = "black",
    fill = "grey30",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  brick_layers = 100,
  bricks_per_layer = 4,
  type = "ordered",
  gap = 0
)
