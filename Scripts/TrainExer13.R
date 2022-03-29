library("glue")

df <- read.delim(
  "Datasets/TrainExer13.txt",
  header = T, sep = "\t"
)

model <- lm(Winning.time.men ~ Game, data = df)
smry <- summary(model)

a <- smry$coefficients[1, 1]
b <- smry$coefficients[2, 1]

r_squared <- smry$r.squared
s <- smry$sigma

print(glue("Intercept is {a}"))
print(glue("Coefficient is {b}"))
print(glue("R-squared is {r_squared}"))
print(glue("Sigma is {s}"))

p <- predict(model, data.frame(Game = c(16, 17, 18)))

predictions <- data.frame(
  Year = c(2008, 2012, 2016),
  Predicted = p,
  actual = c(9.69, 9.63, 9.81)
)

print(predictions)
