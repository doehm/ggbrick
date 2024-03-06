library(patchwork)
library(dplyr)

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n)) +
  coord_waffle()

# defaults
g4 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), colour = "black", gap = NULL) +
  coord_waffle()

# different bricks per layer
g6 <- mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, colour = "black", gap = NULL) +
  coord_waffle(6)

g4 / g6

# different gap
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), colour = "black", gap = 0.02) +
  coord_waffle()

# different gap and bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 6, colour = "black", gap = 0.02) +
  coord_waffle(6)

# different gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), colour = "black", gap = 0) +
  coord_waffle()

# layres 6 gap = 0
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), bricks_per_layer = 8, colour = "black", gap = 0) +
  coord_waffle(8)

# shadow
mpg |>
  count(class, drv) |>
  ggplot() +
  with_shadow(
    geom_waffle(aes(class, n, fill = drv), gap = 0.012),
    x_offset = 2,
    y_offset = 2
  ) +
  coord_waffle() +
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

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, 4*n, fill = drv), bricks_per_layer = 8) +
  coord_brick(8)

# different gap
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), gap = 0.02) +
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
  geom_waffle0(aes(class, n, fill = drv)) +
  coord_waffle()

# different bricks per layer
mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv), bricks_per_layer = 12) +
  coord_waffle(12)


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

# Too many ----------------------------------------------------------------

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, 10*n, fill = drv)) +
  coord_waffle()

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, 10*n, fill = drv), bricks_per_layer = 12) +
  coord_waffle(12)

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, 10*n, fill = drv), bricks_per_layer = 12) +
  coord_waffle(12)

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, 10*n, fill = drv)) +
  coord_brick()

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, 10*n, fill = drv), bricks_per_layer = 12) +
  coord_brick(12)

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick0(aes(class, 10*n, fill = drv), bricks_per_layer = 12) +
  coord_brick(12)

# 🩴 flip --------------------------------------------------------------------

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv)) +
  coord_flip() +
  theme(
    aspect.ratio = 2
  )


# adjust width ------------------------------------------------------------

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle(aes(class, n, fill = drv), col_width = 0.5, gap = 0.02) +
  coord_waffle(col_width = 0.5)

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_waffle0(aes(class, n, fill = drv), col_width = 0.5) +
  coord_waffle(col_width = 0.5)

