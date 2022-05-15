require(tidyverse)
require(janitor)
require(leaps)
require(boot)

suppressWarnings(RNGkind(sample.kind = "Rounding"))
set.seed(9821)

df <- read.csv("ConcreteData-1.csv")

colnames(df) <- c("cement", "slag", "ash", "water", "superplasticizer", "coarseaggregate",
                  "fineaggregate", "age", "strength")

reg <- regsubsets(strength ~ cement + slag + ash + water + 
                    superplasticizer + coarseaggregate + fineaggregate + 
                    age, data = df, nvmax = 9,
                  method = "exhaustive")

summary(reg)

r5 <- glm(strength ~ cement + slag + ash + water + age,
          data = df)
r6 <- glm(strength ~ cement + slag + ash + water + age +
            superplasticizer,
          data = df)
r7 <- glm(strength ~ cement + slag + ash + water + age +
            superplasticizer + coarseaggregate,
          data = df)

fivecv <- cv.glm(df, r5, K = 10)
fivecv$delta[1]

sixcv <- cv.glm(df, r6, K = 10)
sevencv <- cv.glm(df, r7, K = 10)

fivecv$delta[1]
sixcv$delta[1]
sevencv$delta[1]

new1 <- data.frame(
  "cement" = c(250),
  "slag" = c(50),
  "ash" = c(10),
  "water" = c(185),
  "age" = c(250),
  "superplasticizer" = c(5.5),
  "fineaggregate" = c(750),
  "coarseaggregate" = c(900)
)

predict.glm(r7, newdata = new1, interval = "confidence")


########################## Next Section
set.seed(883212)

votes <- read.csv("votes.csv", stringsAsFactors = TRUE) %>%
  clean_names() %>%
  dplyr::mutate(party = ifelse(party == "democrat", 1, 0))

mod <- glm(
  party ~ .,
  data = votes,
  family = "binomial"
)

# elsalvadoraid has the largest difference

sing <- glm(
  party ~ el_salvador_aid,
  data = votes,
  family = "binomial"
)

allcv <- cv.glm(votes, mod, K = 10)
singlecv <- cv.glm(votes, sing, K = 10)

allcv$delta[1]
singlecv$delta[1]

votes$pred_party <- predict(mod, newdata = votes, type = "response")

votes %>%
  dplyr::mutate(pred = ifelse(pred_party >= 0.5, 1, 0)) %>%
  group_by(party, pred) %>%
  count()

new <- read.csv("new.csv") %>%
  clean_names()

new$pred <- predict(mod, newdata = new, type = "response")

#####################################33
### Simulations

set.seed(123)

nsims <- 100000

successes <- 0

for (z in 1:nsims) {
  
  r <- data.frame("res" = c(replicate(20, sample(c('one','two', 'three', 'four'), 
                                                 1, replace=TRUE, prob=c(0.1, 0.2, 0.3, 0.4)))))
  
  r <- r %>%
    dplyr::mutate(
      nx = lead(res),
      tw = lead(res, 2),
      th = lead(res, 3),
      success = ifelse(res == "one" & nx == "two" & 
                         tw == "three" & th == "four",
                       1, 0)
    )
  
  if (max(r$success, na.rm = TRUE) > 0) {
    
    add <- 1
    
  } else {
    add <- 0
  }
  
  successes <- successes + add
  
  print(z)
  
}
