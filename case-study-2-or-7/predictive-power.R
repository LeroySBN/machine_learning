library(dslabs)
library(tidyverse)
library(broom)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train, geom=c("point", "smooth"))

fit <- loess(as.numeric(y) ~ x_2, family = "symmetric", data = mnist_27$train)

mnist_27$train %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(x_2, smooth), color="red")

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")
