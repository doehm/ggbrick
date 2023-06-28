mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv))

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), type = "soft_random")

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), type = "random")

spec <- c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226")
pal <- colorRampPalette(eyedroppeR::pencil_case$mountains_in_autumn$pal)(10)


d10 <- c("#788FCE", "#BD8184", "#E6956F", "#F2CC8F", "#A6BA96", "#C5E8E3", "#F4F1DE", "#CDC3D4", "#A88AD2", "#60627C")

mpg |>
  count(class, trans) |>
  mutate(n = 5*n) |>
  ggplot() +
  geom_brick(aes(class, n, fill = trans)) +
  scale_fill_manual(values = d10)

ggsave("dev/images/pic1.png", height = 6, width = 8)

mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv)) +
  scale_fill_manual(values = d10[c(1, 3, 5)])

ggsave("dev/images/pic0.png", height = 3, width = 8)

ggplot() +
  geom_brick(aes(x = 1, y = 96), fill = "firebrick", bricks_per_layer = 8)

ggsave("dev/images/pic3.png", height = 6, width = 8)


mpg |>
  count(class, drv) |>
  # mutate(n = 5*n) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), type = "random") +
  scale_fill_manual(values = d10[c(1, 3, 5)])

ggsave("dev/images/pic2.png", height = 3, width = 8)
