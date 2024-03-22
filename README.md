
<img src='https://cranlogs.r-pkg.org/badges/ggbrick'/><img src='https://cranlogs.r-pkg.org/badges/grand-total/ggbrick'/><img src='https://www.r-pkg.org/badges/version/ggbrick'/>

# ggbrick <img src='dev/images/ggbrick1.png' align="right" height="240" />

If you’re looking for something a little different, `ggbrick` creates a
‘waffle’ style chart with the aesthetic of a brick wall. The usage is
similar to `geom_col` where you supply counts as the height of the bar
and a fill for a stacked bar. Each whole brick represents 1 unit. Two
half bricks equal one whole brick.

# Installation

Install from CRAN

``` r
install.packages("ggbrick")
```

or from Git

``` r
devtools::install_github("doehm/ggbrick")
```

# Geoms

There are two main geoms included:

1.  `geom_brick()`: To make the brick wall-style waffle chart.
2.  `geom_waffle()`: To make a regular-style waffle chart.

## geom_brick()

Use `geom_brick()` the same way you would use `geom_col()`.

``` r
library(dplyr)
library(ggplot2)
library(ggbrick)
 
# basic usage
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv)) +
  coord_brick()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

`coord_brick()` is included to maintain the aspect ratio of the bricks.
It is similar to `coord_fixed()`, in fact, it is just a wrapper for
`coord_fixed()` with a parameterised aspect ratio based on the number of
bricks. The default number of bricks is 4. To change the width of the
line outlining the brick use the linewidth parameter as normal.

To change specify the `bricks_per_layer` parameter in the geom and coord
functions.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6) +
  coord_brick(6)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

You can change the width of the columns similar to `geom_col()` to add
more space between the bars. To maintain the aspect ratio you also need
to set the width in `coord_brick()`.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), width = 0.5) +
  coord_brick(width = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

To get more space between each brick use the `gap` parameter.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), gap = 0.04) +
  coord_brick()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

For no gap set `gap = 0` or use the shorthand `geom_brick0()`.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick0(aes(class, n, fill = drv)) +
  coord_brick()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

For fun, I’ve included a parameter to randomise the fill of the bricks
or add a small amount of variation at the join between two groups. The
proportions are maintained and designed to just give a different visual.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), type = "soft_random") +
  coord_brick()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), type = "random") +
  coord_brick()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## geom_waffle()

`geom_waffle()` has the same functionality as `geom_brick()` but the
bricks are square giving a standard waffle chart. I added this so you
can make a normal waffle chart in the same way you would use
`geom_col()`. It requires `coord_waffle()`. To maintain the aspect
ratio.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv)) +
  coord_waffle()
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv), bricks_per_layer = 6) +
  coord_waffle(6)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

You may want to flip the coords when using `geom_waffle()`. To do so
you’ll need to use `coord_flip()` and `theme(aspect.ratio = <number>)`.

``` r
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv)) +
  coord_flip() +
  theme(aspect.ratio = 1.8)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
