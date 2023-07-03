mpg |>
  count(class, drv) |>
  ggplot() +
  geom_brick(aes(class, n, fill = drv), bricks_per_layer = 3)

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
  geom_brick(aes(class, n, fill = trans), type = "soft_random", gap = 0.015) +
  # facet_wrap(~class) +
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


df <- expand_grid(
  x = 1:6,
  fill = letters[1:4],
  facet = 1:3
) |>
  mutate(
    n = rpois(n(), 10)*facet
    )

df |>
  ggplot() +
  geom_brick(aes(x, n, fill = fill), bricks_per_layer = 3, brick_layers = 200) +
  facet_wrap(~facet, nrow = 3) +
  coord_flip()



x1 <- c(15.39359, 15.04373)+0.5
N <- 31

robust_round <- function(x, N) {
  n <- round(x)
  add <- N-sum(n)
  id <- sort(x, index.return = TRUE, decreasing = TRUE)$ix[1:add]
  n[id] <- n[id]+1
  n
}

x1 <- c(15.39359, 15.04373)+0.5
N <- 31
robust_round(x1, 31)

x1 <- c(15.39359, 15.04373)
N <- 31
robust_round(x1, 31)

x1 <- c(15.59359, 15.04373)
N <- 31
robust_round(x1, 31)

x <- c("a", "a", "a", "b", "b", "b")
val <- c(1, 1, 1, 1, 0.5, 0.5)
robust_random <- function(x, val) {
  orig <- tibble(
    x = x,
    val = val,
    id = 1:length(x)
  ) %>%
    mutate(
      val_cm = cumsum(val),
      id = ceiling(val_cm)
      )

  rand <- orig %>%
    distinct(id, x) %>%
    mutate(new_x = sample(x))

  orig |>
    left_join(rand, by = "id") |>
    pull(new_x)
}

robust_random(x, val)

r <- 0.77
y <- c(25, 235, 205)
robust_round(y*r, round(sum(y*r)))

expand_grid(
  x = 0:10,
  level = letters[1:2]
) |>
  mutate(n = ifelse(level == "a", 100-10*x, 10*x)) |>
  ggplot() +
  geom_brick(aes(x, n, fill = level), colour = "black", linewidth = 0.1) +
  scale_fill_manual(values = c(a = "grey90", b = "firebrick4")) +
  scale_x_continuous(breaks = 0:10, labels = paste0(seq(0, 100, 10), "%")) +
  # scale_y_continuous(breaks = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%")) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white")
  )

ggsave("dev/images/ordered.png", height = 3, width = 8)

