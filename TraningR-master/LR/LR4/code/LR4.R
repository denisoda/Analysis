library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

x = seq(-20, 20)
y = x*x
png(file = "D:/git/TraningR/LR4/data/plots/LR4_plot_1.png", bg = "transparent")
plot(x, y,
     cex = c((20:1), 1 , (1:20)),
     xlim=range(-30:30),
     ylim=range(0:500),
     col='green')
title(main='y = x^2', col.main='red')

dev.off()


x.norm <- rnorm(n = 200, mean = 0, sd = 10)
png(file = "D:/git/TraningR/LR4/data/plots/LR4_plot_2.png", bg = "transparent")
par(bg = 'gray66')
hist(x.norm, breaks = 15, freq = FALSE, col = "lightblue",
     xlab = "x.norm", ylab = "Density ", main = "Histogram of x.norm",
     ylim=range(0, 0.06), xlim=range(-30, 30))
lines(density(x.norm, bw = 1), col = "white", lwd = 2)
lines(density(x.norm, bw = 10), col = "red", lwd = 2)
dev.off()

mpg


ggplot(mpg) + geom_point(mapping=aes(x=hwy, y=cyl))
ggplot(mpg) + geom_point(mapping=aes(x=hwy, y=cyl), position = 'jitter')

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color='blue')

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
head(mpg)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = cty))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ .)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point()

ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point(position = 'jitter')

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + geom_abline() +  coord_fixed()