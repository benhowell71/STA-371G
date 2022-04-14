require(tidyverse)
require(janitor)
require(boot)
require(leaps)
require(Metrics)

nba_train <- read.csv("NBA_train-4.csv")
nba_test <- read.csv("NBA_test-2.csv")
la <- read.csv("LA_real_estate2-2.csv")

b <- regsubsets(W ~ PTS + oppPTS + FG + FGA + X2P + X2PA + FTA + ORB + DRB + AST + STL + BLK + TOV,
           data = nba_train, method = "backward", nvmax = 13)
summary(b)

f <- regsubsets(W ~ PTS + oppPTS + FG + FGA + X2P + X2PA + FTA + ORB + DRB + AST + STL + BLK + TOV,
                data = nba_train, method = "forward", nvmax = 13)
summary(f)

e <- regsubsets(W ~ PTS + oppPTS + FG + FGA + X2P + X2PA + FTA + ORB + DRB + AST + STL + BLK + TOV,
                data = nba_train, method = "exhaustive", nvmax = 13)
summary(e)

plot(b, scale = "r2")

bg <- lm(W ~ PTS + oppPTS + BLK, data = nba_train)
fg <- lm(W ~ DRB + AST + oppPTS, data = nba_train)
eg <- lm(W ~ PTS + oppPTS + BLK, data = nba_train)

summary(bg)
summary(fg)
summary(eg)

nba_test$bt <- predict(bg, newdata = nba_test)
nba_test$ft <- predict(fg, newdata = nba_test)
nba_test$et <- predict(eg, newdata = nba_test)

rmse(nba_test$W, nba_test$bt)
rmse(nba_test$W, nba_test$ft)
rmse(nba_test$W, nba_test$et)

###################################

RNGkind(sample.kind = "Rounding") # this line is important so that all versions of R behave the same
set.seed(41230)

n = nrow(la)
nval <- round(0.15*n)
ind <- sample(n, nval)

la_train <- la[-ind, ]
la_test <- la[ind, ] 

b <- regsubsets(LastSalePrice ~ SQFTmain+ Bedrooms+ Bathrooms+ LandSQFT + YearOfSale + TaxBeforeSale,
                data = la_train, method = "backward", nvmax = 6)
summary(b)

f <- regsubsets(LastSalePrice~ SQFTmain+ Bedrooms+ Bathrooms+ LandSQFT + YearOfSale + TaxBeforeSale,
                data = la_train, method = "forward", nvmax = 6)
summary(f)

e <- regsubsets(LastSalePrice~ SQFTmain+ Bedrooms+ Bathrooms+ LandSQFT + YearOfSale + TaxBeforeSale,
                data = la_train, method = "exhaustive", nvmax = 6)
summary(e)

bl <- glm(LastSalePrice ~ SQFTmain + YearOfSale + TaxBeforeSale,
         data = la_train)
summary(bl)
fl <- glm(LastSalePrice ~ LandSQFT + YearOfSale + TaxBeforeSale,
         data = la_train)
summary(fl)
el <- glm(LastSalePrice ~ SQFTmain + YearOfSale + TaxBeforeSale,
         data = la_train)
summary(el)

la$pred <- predict(bl, newdata = la)
rmse(la$LastSalePrice, la$pred)

RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

RSQUARE(la$LastSalePrice, la$pred)

set.seed(41230)
cv1 = cv.glm(la, bl, K = 10)
