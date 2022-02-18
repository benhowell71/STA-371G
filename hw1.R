require(tidyverse)
require(janitor)

df <- vroom::vroom("baseball.csv")

lst <- list()

for (i in 1:100) {
  
  v <- cos(i)
  vv <- data.frame("result" = v, "num" = i)
  lst[[i]] <- vv
  
  print(i)
  
}

results <- dplyr::bind_rows(lst) %>%
  arrange(desc(result))
#################
# 7
cor(df$RS, df$W)
# 0.51

# 8
sd(df$OBP, na.rm = TRUE)

# 9
mean(df %>%
       dplyr::filter(Team == "HOU") %>%
       pull(RS),
     na.rm = TRUE)

# 10
median(df %>%
       dplyr::filter(Team == "TEX") %>%
         pull(RA),
       na.rm = TRUE)

df %>%
  ggplot() +
  geom_point(aes(x = BA, y = RS))

baseballT = t.test(df$BA, alternative = 'greater', mu=0.250)
# null hypothesis: true mean BA is 0.250
# alternative hyp: true mean BA is greater than 0.250
baseballT
baseballT$statistic
baseballT$p.value

# 2 Use baseball data to perform a t-test of the hypothesis that the mean runs scored is >= 710.
# What is the t-statistic? (round your answer to two decimal places)

t.test(df$RS, alternative = "greater", mu = 710)

# 6 
# Use the baseball dataset to perform a sample test on OOBP vs OBP with not equal alternative in seasons 2010 and later. 
# What's the degree of freedom?

df2 <- df %>%
  dplyr::filter(Year >= 2010)

y <- t.test(df2$OBP, df2$OOBP)
y

####################
s <- c()
s[1] <- 1
s[2] <- 2

for (i in 3:10) {
  
  s[i] <- 2 * s[i - 1]- sqrt(s[i - 2])
  
}

