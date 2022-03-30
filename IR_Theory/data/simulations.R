library(tidyverse)

y <- rep(NA, 200)
y[1:50] <- rnorm(50, 0, 0.5)
y[51:100] <- rnorm(50, 1, 0.5)
y[101:150] <- rnorm(50, 2, 0.5)
y[151:200] <- rnorm(50, 3, 0.5)
x <- 1:200
pts <- tibble(x, y)

ggplot(pts, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

mdl1 <- lm(y ~ x, data = pts)
summary(mdl1)
