# EM ----------------------------------------------------------

library(mclust)

df <- mtcars [, c("mpg", "hp")]

fit <- Mclust(df)
plot(fit, what = "classification")

summary(fit, parameter = TRUE)


# One variable

fit <- Mclust(df$mpg, G = 5)
plot(fit, what = "classification")

summary(fit, parameter = TRUE)
