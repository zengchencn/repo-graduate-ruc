library(tidyverse)

set.seed(1)

y <- rep(NA, 200)
y[1:50] <- rnorm(50, 0, 0.5)
y[51:100] <- rnorm(50, 1, 0.5)
y[101:150] <- rnorm(50, 2, 0.5)
y[151:200] <- rnorm(50, 3, 0.5)

x <- seq(1, 200, by = 1)
z <- rep(c(1, 2, 3, 4), each = 50)

demo <- as.data.frame(cbind(x, y, z))
demo$z <- factor(demo$z, labels = c("G1", "G2", "G3", "G4"))

fig1 <- ggplot(demo, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm")

fig2 <- ggplot(demo, aes(x, y, color = z)) +
  geom_point() +
  geom_smooth(method = "lm")

mdl1 <- lm(y ~ x, data = demo)
