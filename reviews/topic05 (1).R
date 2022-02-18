bball = read.csv('~/library/mobile documents/com~apple~clouddocs/utexas/sta371g - s22/r code/baseball.csv')



basball_regression = lm(W~OBP+SLG+BA+OOBP+OSLG,data=bball)
summary(basball_regression)

print(basball_regression$coefficients[1])


pred = basball_regression$coefficients[1] + 
  basball_regression$coefficients[2]*0.32 +
  basball_regression$coefficients[3]*0.415 +
  basball_regression$coefficients[4]*0.249 +
  basball_regression$coefficients[5]*0.308 +
  basball_regression$coefficients[6]*0.391
pred

# BA looks insignificant!
basball_regression2 = lm(W~OBP+SLG+OOBP+OSLG,data=bball)
summary(basball_regression2)
pred2 = basball_regression2$coefficients[1] + 
  basball_regression2$coefficients[2]*0.32 +
  basball_regression2$coefficients[3]*0.415 +
  basball_regression2$coefficients[4]*0.308 +
  basball_regression2$coefficients[5]*0.391
pred2

