library(patchwork)
library(dplyr)

# 🧱 geom_brick -----------------------------------------------------------

# defaults
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv)) +
  coord_brick()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6) +
  coord_brick(6)

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 3) +
  coord_brick(3)

# gap
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), gap = 0.02) +
  coord_brick()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.02) +
  coord_brick(6)

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.04) +
  coord_brick(6)

# different bricks per layer gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick0(aes(class, n, fill = drv)) +
  coord_brick()

# bpl = 8
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick0(aes(class, n, fill = drv), bricks_per_layer = 8) +
  coord_brick(8)


# width

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), width = 0.5) +
  coord_brick(width = 0.5)


# shadow ------------------------------------------------------------------

mpg |>
  count(class, drv) |>
  ggplot() +
  with_inner_glow(
    with_shadow(
      geom_brick(aes(class, n, fill = drv)),
      x_offset = 4,
      y_offset = 4
    ),
    sigma = 6
  ) +
  coord_brick() +
  scale_fill_brewer(type = "qual")
