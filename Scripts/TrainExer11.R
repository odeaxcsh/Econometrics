df <- read.delim("Datasets/TrainExer11.txt", header = T, sep = "\t", row.names = 1)

hist(df$Age, xlab = "Age",
  main = "Histogram for Ages"
)

hist(
  df$Expenditures, xlab = "Expenditures",
  main = "Histogram for Expenditures"
)

plot(df$Age, df$Expenditures,
  main = "Scatter Plot of Expenditures/Age",
  xlab = "Age",
  ylab = "Expenditures"
)

print(sprintf("the sample mean of expenditures of all 26 clients is %f",
    mean(df$Expenditures)
))

youngers <- df[df$Age < 40, ]
olders <- df[df$Age >= 40, ]

print(sprintf(
  "The sample mean of younger clients is %f and for olders is %f",
  mean(youngers$Expenditures),
  mean(olders$Expenditures)
))

ymodel <- lm(Expenditures ~ Age, data = youngers)

y25exp <- predict(ymodel, data.frame(Age = c(25)))

print(sprintf("We expect a 25 years old expenditures to be about %f", y25exp))


omodel <- lm(Expenditures ~ Age, data = olders)

o50exp <- predict(ymodel, data.frame(Age = c(50)))

print(sprintf("We expect a 50 years old expenditures to be about %f", o50exp))
