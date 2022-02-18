
bendrix = read.csv('~/library/mobile documents/com~apple~clouddocs/utexas/sta371g - s22/r code/bendrix.csv')

machine_regression = lm(Overhead~Machine.Hours,data=bendrix)
summary(machine_regression)

production_regression = lm(Overhead~Production.Runs,data=bendrix)
summary(production_regression)


multiple_regression = lm(Overhead~Machine.Hours+Production.Runs,data=bendrix)
summary(multiple_regression)
