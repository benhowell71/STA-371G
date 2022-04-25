require(tidyverse)
require(janitor)

df <- read.csv("titanic-4.csv")

df$Cabin[df$Cabin == ""] <- NA
df = na.omit(df)

all <- glm(Survived ~ Pclass + Age + Fare + Sex + Embarked, data = df, family = "binomial")
summary(all)

four <- glm(Survived ~ Pclass + Age + Sex + Embarked, data = df, family = "binomial")
summary(four)

df$pred <- predict(four, newdata = df, type = "response")

df <- df %>%
  dplyr::mutate(
    pred_survive = ifelse(pred >= 0.6, 1, 0)
  )

df %>%
  group_by(Survived, pred_survive) %>%
  count()

summary(four)

three <- glm(Survived ~ Pclass + Age + Sex, data = df, family = "binomial")
summary(three)

new <- data.frame(
  "Pclass" = c(1, 3, 1, 3),
  "Age" = c(54, 21, 25, 25),
  "Sex" = c("male", "female", "female", "female"),
  "Embarked" = c("S", "C", "C", "C")
)

new$pred <- predict(four, newdata = new, type = "response")


two <- glm(Survived ~ Age + Sex, data = df, family = "binomial")
summary(two)

one <- glm(Survived ~ Sex, data = df, family = "binomial")
summary(one)

RNGkind(sample.kind = 'Rounding')
set.seed(11346)

require(boot)

fivecv <- cv.glm(df, all, K = 10)
fourcv <- cv.glm(df, four, K = 10)
threecv <- cv.glm(df, three, K = 10)
twocv <- cv.glm(df, two, K = 10)
onecv <- cv.glm(df, one, K = 10)

fivecv$delta[1]
fourcv$delta[1]
threecv$delta[1]
twocv$delta[1]
onecv$delta[1]