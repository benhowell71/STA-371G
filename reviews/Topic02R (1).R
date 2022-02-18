# topic 2 - R introduction
# anything to the right of a # is ignored by R...these are comments 

# on a Mac use command+enter to run a single line
# on a PC  use control+enter to run a single line
# or push the 'Run' button on the upper right of this screen, it will run the line where your cursor is
# to run every line in the file click the 'Source' button on the upper right of this screen

# scalar variables
a = 2
print(a)
b = 3
print(b)
d = a+b # don't use c... we'll see why in a minute
print(d)
a = a+55
a
print(a)
print(d)

# vector variables
?rep
?c
a = rep(5,3)
print(a)
b = c(1,2,3) # now you see why we didn't use c as a variable name earlier?
print(b)
d = a + b
print(d)
d = d+98
print(d)
e = rep("hello",6)
print(e)
f = 1:5
print(f)
print(length(f))
print(sum(f))
print(prod(f))
?seq
g = seq(5,6,length.out = 11)
print(g)
print(g^2)


# indices
print(d[2])
d[3] = 189
print(d)
g[4:6] = 8:10 # dan wrote some random text here and it is ignored by R
print(g)
g[7] = g[7]^2
print(g)


# conditionals
5 < 4
5 > 4
5 == 3
2 != 3
if(5 < 4){
  print('no')
} else if(5 == 3){
  print('no way')
} else if(100 >= 108){
  print('nope')
} else if(3 != 3){
  print('wrong again')
} else {
  print('yep')
}

if(5 > 1){
  print('dan')
} else {
  print('mitchell')
} 

# vectors of logicals
w = (e == 'hello')
print(e)
print(w)
print(sum(w))
x = (f <= 3)
print(x)
print(sum(x))
mean(x)
x
y = c(TRUE,TRUE,FALSE,FALSE,TRUE)
f
f[x] = 33
print(f)
f[y]

# loops
nvec = 17
vec = rep(0,nvec)
print(vec)
for(i in 1:5){
  print(i)
}
1:nvec
for(entry in 1:nvec){
  vec[entry] = log(entry)*cos(entry*pi)
}
print(vec)

for(entry in 1:nvec){
  if(entry > 15){
    vec[entry] = 333333
  }
}
print(vec)

iter.vec = rep(0,15)
for(i in 2:15){
  iter.vec[i] = sqrt(iter.vec[i-1]^2+9)
}
iter.vec
iter.vec[15]

# are any of these entries equal to 3?
iter.vec==3
sum(iter.vec==3)
which(iter.vec==3)


# how many are bigger than 9?
sum(iter.vec > 9)
sum(iter.vec >= 9)

# graphs
xvec = 1:15
plot(xvec,iter.vec,type='l',xlab='X-Axis',ylab = 'Y-Axis',main = 'TITLE')


# importing data
baseball_data = read.csv('~/Library/Mobile Documents/com~apple~clouddocs/utexas/sta371g - s22/r code/baseball.csv')
# baseball_data = read.csv('c:/users/dan/documents/sta371g/rcode/baseball.csv')
# alternatively click File -> Import Dataset -> From Text (Base)...
# browse to the dataset on your computer, then name it whatever you want
# here the following lines will only work if you name it baseball_data though...
View(baseball_data)
head(baseball_data)
colnames(baseball_data)
print(baseball_data$Team[3])
print(baseball_data[3,1])
baseball_data$Team[3] = 'Dan does not play professional baseball'
View(baseball_data)
baseball_data$NewColumn = baseball_data$RS+baseball_data$W
View(baseball_data)
directory = '~/Library/Mobile Documents/com~apple~clouddocs/utexas/sta371g - s22/r code/'
name = 'baseball_modified.csv'
write.csv(baseball_data,paste(directory,name,sep=''),row.names = FALSE)
print(baseball_data$W[baseball_data$Team == 'NYY']/baseball_data$G[baseball_data$Team == 'NYY'])


# some possible problems when importing data
# this is very important! if you run into an issue importing data on an exam
# you will not be given extra time.  
# one of these methods will be able to fix the problem!

# the first row of the csv file is interpreted as values rather that column names!
# and the column names are V1, V2, ...
# how would you fix this?
# two possibilities
data = read.csv(...,header=TRUE)
# when you do file -> import data set -> from text (base) be sure to click the button that says yes for heading
# otherwise you can assign the column names to be the first row and then remove first row
colnames(data) = data[1,]
nrows = nrow(data)
data = data[2:nrows,]

# other possible problem: the name of the first column has some weird ï at the beginning?!?!
# you can just change the name of that first column
colnames(data)[1] = 'new_name_NO_SPACES'
# then you can reference it using the new name!

# graphs
plot(baseball_data$BA,baseball_data$SLG,xlab='Batting Average',ylab='Slugging',col='blue',pch=18)
hist(baseball_data$BA,xlab='Batting Average',breaks=25)
boxplot(baseball_data$BA,xlab='Batting Average')

# sta309 content
ba_mean = mean(baseball_data$BA)
ba_std  = sd(baseball_data$BA)
ba_median = median(baseball_data$BA)
print(ba_mean)
print(ba_std)
print(ba_median)

?t.test
baseballT = t.test(baseball_data$BA,alternative = 'greater',mu=0.250)
# null hypothesis: true mean BA is 0.250
# alternative hyp: true mean BA is greater than 0.250
baseballT
baseballT$statistic
baseballT$p.value
# p-value is very small -> therefore reject the null in favor of the alternative

baseballT2 = t.test(baseball_data$BA,baseball_data$SLG)
# null hypothesis: true mean BA is equal to true mean slugging
# altenrtive: these true means aren't the same
print(baseballT2$p.value)
# the p-value is so small that the computer rounds to 0...so reject the null

# simple linear regression
my_regression = lm(W~BA,data=baseball_data) # left of squiggle is y, right of squiggle is x
summary(my_regression)
# wins = -13.168 + 362.829 * batting_average + error

my_regression$coefficients[1]
my_regression$coefficients[2]

