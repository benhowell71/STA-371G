require(tidyverse)
require(janitor)

auto <- read.csv("hw4/Auto-2.csv")
heart <- read.csv("hw4/Heart.csv")
re <- read.csv("hw4/LA_real_estate2-2.csv")

set.seed(123)

model <- lm(sqrt(mpg) ~ cylinders + displacement + horsepower + weight + (year), data = auto)
summary(model)
plot(model)

plot(log(auto$year), auto$mpg)
plot((1 / (auto$cylinders)), auto$mpg)
plot((auto$mpg))

new <- data.frame(
  cylinders = c(8),
  displacement = c(307),
  horsepower = c(130),
  weight = c(3504),
  year = c(70)
)

predict(model, new)

model <- lm(sqrt(mpg) ~ cylinders + displacement + horsepower + weight + (year) + origin, data = auto)
summary(model)


model <- lm(LastSalePrice ~ SQFTmain + Bedrooms + Bathrooms + View, data = re)
model

new <- data.frame(
  SQFTmain = c(4078, 4078),
  Bedrooms = c(4, 4),
  Bathrooms = c(4, 4),
  View = c("City", "Canyon")
)

predict(model, new)


model <- lm(Chol ~ Thal + Gender + ChestPain, data = heart)
plot(model)
plot(heart$Thal, heart$Chol)

new <- data.frame(
  Thal = c("normal", "normal", "normal"),
  Gender = c("Male", "Male", "Male"),
  ChestPain = c("typical", "nonanginal", "nontypical")
)

predict(model, new)
