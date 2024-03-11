library(ggplot2)
library(patchwork)
library(dplyr)

# 🧱 geom_waffle -----------------------------------------------------------

# defaults
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv)) +
  coord_waffle()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6) +
  coord_waffle(6)

# gap
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), gap = 0.02) +
  coord_waffle()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.02) +
  coord_waffle(6)

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.04) +
  coord_waffle(6)

# different bricks per layer gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv)) +
  coord_waffle()

# bpl = 8
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv), bricks_per_layer = 8) +
  coord_waffle(8)

# bpl = 24
# 24 just isn't quite right
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, 24*n, fill = drv), bricks_per_layer = 24) +
  coord_waffle(24)

# 🩴 flip --------------------------------------------------------------------

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv)) +
  coord_flip() +
  theme(
    aspect.ratio = 2
  )

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv)) +
  coord_flip() +
  theme(
    aspect.ratio = 2
  )


# col_width ---------------------------------------------------------------

# defaults
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), width = 0.5) +
  coord_waffle(width = 0.5)

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, width = 0.5) +
  coord_waffle(6, width = 0.5)

# 0.02 gap + 6 bpl + 0.75 width
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.02, width = 0.75) +
  coord_waffle(6, width = 0.75)

# shadow ------------------------------------------------------------------

mpg |>
  count(class, drv) |>
  ggplot() +
  with_inner_glow(
    with_shadow(
      geom_waffle(aes(class, n, fill = drv)),
      x_offset = 3,
      y_offset = 3
    ),
    sigma = 6
  ) +
  coord_waffle() +
  scale_fill_brewer(type = "qual")
