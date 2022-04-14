require(tidyverse)
require(janitor)
require(leaps)

suppressWarnings(RNGkind(sample.kind = "Rounding"))
set.seed(1132)

df <- read.csv("xy_data_v5.csv")

plot(df$x, df$y)

df <- df %>%
  dplyr::mutate(
    x_sq = x^2,
    log_y = log(y),
    sqrt_y = sqrt(y)
  )

orig <- lm(y ~ x, data = df)

one <- lm(y ~ x_sq, data = df)
two <- lm(log_y ~ x, data = df)
three <- lm(log_y ~ x_sq, data = df)
four <- lm(sqrt_y ~ x, data = df)

hist(orig$residuals)
hist(one$residuals)
hist(two$residuals)
hist(three$residuals)
hist(four$residuals)

plot(orig)
plot(one)
plot(two)
plot(three)
plot(four)

####################3
df <- read.csv("titanic_v3.csv") %>%
  dplyr::mutate(Pclass = as.character(Pclass))

mod <- lm(Fare ~ Age + Pclass + Sex + Embarked, data = df)

new <- data.frame(
  Age = c(42, 50),
  Pclass = c("1", "3"),
  Sex = c("male", "female"),
  Embarked = c("S", "S")
)

predict(mod, newdata = new)
###########################333
df <- read.csv("Concrete_Data-12.csv")

colnames(df)[1] = 'cement'

# cars_selection = regsubsets(logmpg~cylinders+displacement+horsepower+logweight+year+origin,data = cars,method = 'forward')
# plot(cars_selection,scale='r2')
sel = regsubsets(strength ~ Age + FineAggregate + CoarseAggregate + Superplasticizer + water + ash + slag + cement,
                 data = df, method = "forward")

summary(lm(strength ~ Age + CoarseAggregate + Superplasticizer + water + ash + slag + cement,
           data = df))

bsel = regsubsets(strength ~ Age + FineAggregate + CoarseAggregate + Superplasticizer + water + ash + slag + cement,
                 data = df, method = "backward")

summary(lm(strength ~ Age + water + ash + slag + cement,
           data = df))
# bsel 5 = 0.611

summary(lm(strength ~ Age + Superplasticizer + water + slag + cement,
           data = df))
# fsel 5 = 0.5843