bball = read.csv('~/library/mobile documents/com~apple~clouddocs/utexas/sta371g - s22/r code/baseball.csv')

basball_regression1 = lm(W~OBP+SLG+BA+OOBP+OSLG,data=bball)
basball_regression2 = lm(W~RS+OOBP+OSLG,data=bball)

# which one will have a higher R-squared?

summary(basball_regression1)
summary(basball_regression2)
