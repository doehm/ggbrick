library(patchwork)

# defaults
g4 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle(aes(class, n, fill = drv), colour = "black", gap = NULL) +
  coord_brick_waffle()

# different bricks per layer
g6 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, colour = "black", gap = NULL) +
  coord_brick_waffle(6)

g4 / g6

# different gap
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle(aes(class, n, fill = drv), colour = "black", gap = 0.02) +
  coord_brick_waffle()

# different gap and bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, colour = "black", gap = 0.02) +
  coord_brick_waffle(6)

# different gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle(aes(class, n, fill = drv), colour = "black", gap = 0) +
  coord_brick_waffle()

# layres 6 gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle(aes(class, n, fill = drv), bricks_per_layer = 8, colour = "black", gap = 0) +
  coord_brick_waffle(8)

# shadow
mpg |>
  count(class, drv) |>
  ggplot() +
  with_shadow(
    geom_brick_waffle(aes(class, n, fill = drv), gap = 0.012),
    x_offset = 2,
    y_offset = 2
  ) +
  coord_brick_waffle() +
  scale_fill_brewer(type = "qual")

# 🧱 geom_brick -----------------------------------------------------------

# defaults
g4 <- mpg |>
  count(class, drv) |>
  ggplot() +
  with_shadow(geom_brick(aes(class, n, fill = drv), gap = NULL), x_offset = 2, y_offset = 2) +
  coord_brick()

# different bricks per layer
g6 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6, gap = NULL) +
  coord_brick(6)

g4 / g6

# defaults
g4 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), gap = 0.004) +
  coord_brick()

# different bricks per layer
g6 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.004) +
  coord_brick(6)

g4 / g6


# different bricks per layer gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0) +
  coord_brick(6)

# different gap
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), colour = "black", gap = 0.02) +
  coord_brick()

# different gap and bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 6, gap = 0.02) +
  coord_brick(6)

# shadow
mpg |>
  count(class, drv) |>
  ggplot() +
  with_shadow(
    geom_brick(aes(class, n, fill = drv)),
    x_offset = 4,
    y_offset = 4
  ) +
  coord_brick() +
  scale_fill_brewer(type = "qual")

# 🧇 zero -----------------------------------------------------------------

# defaults
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle0(aes(class, n, fill = drv)) +
  coord_brick_waffle()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick_waffle0(aes(class, n, fill = drv), bricks_per_layer = 6) +
  coord_brick_waffle(6)


# 🧱 zero -----------------------------------------------------------------

# defaults
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick0(aes(class, n, fill = drv)) +
  coord_brick()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick0(aes(class, n, fill = drv), bricks_per_layer = 6) +
  coord_brick(6)
