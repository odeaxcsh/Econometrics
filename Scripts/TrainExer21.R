library(glue)

df <- read.delim(
    "Datasets/TrainExer21.txt",
    header = TRUE,
    sep = "\t",
    row.names = 1
)

model <- lm(log(Wage) ~ Female, data = df)
print(coef(model))
print(summary(model)$r.squared)
print(
    paste(
        if (summary(model)$r.squared < 0.05)
            "Accept the null hypothesis"
        else "Reject the null hypothesis",
        "For the main model"
    )
)

e <- model$residuals

education_model <- lm(e ~ df$Edu)
print(coef(education_model))
print(summary(education_model)$r.squared)
print(
    paste(
        if (summary(education_model)$r.squared < 0.05)
            "Accept the null hypothesis"
        else "Reject the null hypothesis",
        "For the education model"
    )
)

inc_per_unit <- exp(coef(education_model)[2])
print(glue(
    "Increase of wage for each education level is {inc_per_unit}",
))

parttime_model <- lm(e ~ df$Parttime)
print(coef(parttime_model))
print(summary(parttime_model)$r.squared)
print(
    paste(
        if (summary(parttime_model)$r.squared < 0.05)
            "Accept the null hypothesis"
        else "Reject the null hypothesis",
        "For the parttime model"
    )
)

inc_per_unit <- exp(coef(parttime_model)[2])
print(glue(
    "Increase of wage for being a parttime employee {inc_per_unit}",
))
