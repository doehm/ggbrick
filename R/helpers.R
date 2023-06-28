#' Brick row
#'
#' @param layer Brick layer
#' @param width Number of bricks in the layer
#' @param brick_height Brick height
#' @param brick_width Brick width
#' @param gap Gap between the bricks
brick_row <- function(layer, width, brick_height = 1, brick_width = 2.5, gap = 0.125) {
  tibble(
    xmin = seq(1, 1+(width-1)*brick_width, brick_width),
    xmax = xmin + brick_width - gap,
    ymin = 0 + layer*brick_height,
    ymax = brick_height + layer*brick_height - gap,
    brick_type = 1
  )
}

#' half brick row
#'
#' @param layer Brick layer
#' @param width Number of bricks in the layer
#' @param brick_height Brick height
#' @param brick_width Brick width
#' @param gap Gap between the bricks
half_brick_row <- function(layer, width, brick_height = 1, brick_width = 2.5, gap = 0.125) {
  tibble(
    xmin = c(1, seq(1, 1+(width-1)*brick_width, brick_width) + brick_width/2),
    xmax = c(1+brick_width/2, seq(1, 1+(width-2)*brick_width, brick_width) + brick_width*3/2, brick_width*width+1)-gap,
    ymin = 0 + layer*brick_height,
    ymax = brick_height + layer*brick_height - gap,
    brick_type = c(0.5, rep(1, width-1), 0.5)
  )
}

#' Build the wall
#'
#' @param height Height of the wall
#' @param width Width of the wall in number of bricks
#' @param start_height Starting height of the wall
#' @param r Scale factor
build_wall <- function(height, width, start_height = 0, r = 1) {
  brick_height <- 0.9/width*2/5
  brick_width <- 0.9/width
  gap <- brick_height/10
  scale <- 1/brick_height*width*1/r
  map_dfr(seq(0, height-1, 1), ~{
    if((.x+start_height) %% 2 == 0) {
      brick_row(.x, width, brick_height, brick_width, gap)
    } else {
      half_brick_row(.x, width, brick_height, brick_width, gap)
    }
  }) %>%
    mutate(
      brick_id = 1:n(),
      xmin = xmin-0.45,
      xmax = xmax-0.45,
      ymin = ymin*scale + start_height,
      ymax = ymax*scale + start_height
    )
}

#' build wall brick by brick
#'
#' @param n_bricks Number of bricks
#' @param width Width of the wall in bricks
#' @param r Scale factor
build_wall_by_brick <- function(n_bricks, width, r = 1) {
  ht <- ceiling(n_bricks/width)
  build_wall(ht, width, r = r) %>%
    arrange(ymin, desc(brick_type)) %>%
    mutate(brick_type_cm = cumsum(brick_type)) %>%
    filter(brick_type_cm <= n_bricks)
}

#' round preserve sum
#'
#' @param x A number of vector
#' @param digits Number of digits
round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x*up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y/up
}

#' Fill
#'
#' Makes the vector for the fill aesthetic
#'
#' @param fill The fill vector
#' @param n Vector representing the number of bricks for the fill level
#' @param val Vector of length the same as fill of with 1 o 0.5 for whole or helf bricks
make_new_fill <- function(fill, n, val) {
  val_cm <- c(0, cumsum(val))
  n_cm <- c(0, cumsum(n))
  new_fill <- rep(NA, length(val))
  for(k in 1:length(fill)) {
    i <- which(val_cm > n_cm[k] & val_cm <= n_cm[k+1])-1
    new_fill[i] <- fill[k]
  }
  new_fill
}

#' Switch position for soft random
#'
#' @param x Vector to switch values in
#' @param n Number to switch
switch_pos <- function(x, n) {
  starting_pos <- sample(1:length(x), n)
  dist <- -15:15
  p <- sample(dist, n, replace = TRUE, prob = dnorm(seq(-3, 3, length = length(dist))))
  next_pos <- pmin(pmax(starting_pos + p, 1), length(x))
  y <- x
  y[next_pos] <- x[starting_pos]
  y[starting_pos] <- x[next_pos]
  y
}
