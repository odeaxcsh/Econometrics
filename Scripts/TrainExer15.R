library(ggplot2)
library(glue)

df <- read.delim("Datasets/TrainExer15.txt", header = T, sep = "\t")

# Linear model
men_model <- lm(Winmen ~ Game, data = df)
men_c <- coef(men_model)

women_model <- lm(Winwomen ~ Game, data = df)
women_c <- coef(women_model)

cm <- rbind(coef(men_model), coef(women_model))
inter <- c(-solve(cbind(cm[, 2], -1)) %*% cm[, 1])

ggplot(data = df, mapping = aes(x = Game)) +
    geom_line(mapping = aes(y = Winwomen), color = "red") +
    geom_point(mapping = aes(y = Winwomen), color = "red") +
    geom_line(mapping = aes(y = Winmen), color = "blue") +
    geom_point(mapping = aes(y = Winmen), color = "blue") +
    geom_abline(intercept = men_c[1], slope = men_c[2]) +
    geom_abline(intercept = women_c[1], slope = women_c[2]) +
    geom_point(x = inter[1], y = inter[2], size = 2, color = "purple") +
    xlim(1, 49) + ylim(8, 12)

print(glue(
    "Intersection of two lines is
    Game: {round(inter[1])},
    Year: {round(inter[1] - 1) * 4 + 1948}
    Win time: {inter[2]}
"))


# Non-linear model
men_model <- lm(log(Winmen) ~ Game, data = df)
men_c <- coef(men_model)

women_model <- lm(log(Winwomen) ~ Game, data = df)
women_c <- coef(women_model)

cm <- rbind(coef(men_model), coef(women_model))
inter <- c(-solve(cbind(cm[, 2], -1)) %*% cm[, 1])


ggplot(data = df, mapping = aes(x = Game)) +
    geom_line(mapping = aes(y = Winwomen), color = "red") +
    geom_point(mapping = aes(y = Winwomen), color = "red") +
    geom_line(mapping = aes(y = Winmen), color = "blue") +
    geom_point(mapping = aes(y = Winmen), color = "blue") +
    geom_point(x = inter[1], y = inter[2], size = 2, color = "purple") +
    stat_function(fun = function(x) exp(men_c[1] + men_c[2] * x)) +
    stat_function(fun = function(x) exp(women_c[1] + women_c[2] * x)) +
    geom_point(x = inter[1], y = exp(inter[2]), size = 2, color = "purple") +
    xlim(0, 65)

print(glue(
    "Intersection of two lograithmic lines is
    Game: {round(inter[1])},
    Year: {round(inter[1] - 1) * 4 + 1948}
    Win time: {exp(inter[2])}
"))
