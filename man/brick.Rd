% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-brick.R
\name{stat_brick}
\alias{stat_brick}
\alias{brick}
\alias{geom_brick}
\title{stat_brick}
\usage{
stat_brick(
  mapping = NULL,
  data = NULL,
  geom = "rect",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  brick_layers = 100,
  bricks_per_layer = 4,
  type = "ordered",
  gap = NULL,
  ...
)

geom_brick(
  mapping = NULL,
  data = NULL,
  stat = "brick",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  brick_layers = 60,
  bricks_per_layer = 4,
  type = "ordered",
  gap = NULL,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by [aes()]. If specified and
`inherit.aes = TRUE` (the default), it is combined with the default mapping
at the top level of the plot. You must supply `mapping` if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
   options:

   If `NULL`, the default, the data is inherited from the plot
   data as specified in the call to [ggplot()].

   A `data.frame`, or other object, will override the plot
   data. All objects will be fortified to produce a data frame. See
   [fortify()] for which variables will be created.

   A `function` will be called with a single argument,
   the plot data. The return value must be a `data.frame`, and
   will be used as the layer data. A `function` can be created
   from a `formula` (e.g. `~ head(.x, 10)`).}

\item{geom}{Geom}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. `"jitter"` to use `position_jitter`), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If `FALSE` removes `NA`s from the data.}

\item{show.legend}{logical. Should this layer be included in the legends?
`NA`, the default, includes if any aesthetics are mapped.
`FALSE` never includes, and `TRUE` always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If `FALSE`, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. [borders()].}

\item{brick_layers}{The number of brick layers. Default is the height of the column divded by
the number of bricks per layer.}

\item{bricks_per_layer}{The number of bricks per layer. Default 4.}

\item{type}{The type of fill ordering. one of 'ordered', 'random' or 'soft_random', Default 'ordered'}

\item{gap}{The space between bricks.}

\item{...}{Dots.}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a `ggproto` `Geom` subclass or as a string naming the
stat stripped of the `stat_` prefix (e.g. `"count"` rather than
`"stat_count"`)}
}
\value{
Grob
}
\description{
Creates a 'waffle' style chart with the aesthetic of a brick wall. Usage is
similar to `geom_col` where you supply counts as the height of the bar. Each
whole brick represents 1 unit. Two half bricks equal one whole brick. Where
the count exceeds the number of brick layers, the number of bricks is scaled
to retain the brick wall aesthetic.
}
\examples{
library(ggplot2)
library(dplyr)
mpg \%>\%
  count(class, drv) \%>\%
  ggplot() +
  geom_brick(aes(class, n, fill = drv))
}
