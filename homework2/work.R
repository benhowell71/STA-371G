
auto <- read.csv("homework2/Auto.csv")
frame <- read.csv("homework2/Framingham.csv")

mod <- lm(horsepower ~ displacement, data = auto)
summary(mod)

mod2 <- lm(acceleration ~ horsepower, data = auto)
summary(mod2)

mod3 <- lm(acceleration ~ horsepower + weight, data = auto)
summary(mod3)
######################################################################
fmod <- lm(BMI ~ sysBP, data = frame)
summary(fmod)

hmod <- lm(heartRate ~ BMI, data = frame)
summary(hmod)

bmod <- lm(BMI ~ totChol + sysBP + diaBP + heartRate + glucose, data = frame)
summary(bmod)
