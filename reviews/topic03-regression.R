bendrix = read.csv('~/library/mobile documents/com~apple~clouddocs/utexas/sta371g - s22/r code/Bendrix.csv')
View(bendrix)
bendrix = bendrix[,-1] # remove month column...
View(bendrix)
pairs(bendrix)

machine_regression = lm(Overhead~Machine.Hours,data=bendrix)
summary(machine_regression)

production_regression = lm(Overhead~Production.Runs,data=bendrix)
summary(production_regression)

sd(production_regression$residuals)
sqrt(var(production_regression$residuals)*35/34)
