require(tidyverse)
require(janitor)

set.seed(132)

df <- read.csv("la_data.csv") %>%
  janitor::clean_names()

head(df)

sd(df$last_sale_price, na.rm = TRUE)

df %>%
  dplyr::filter(bedrooms >= 3) %>%
  summarise(
    n = n(),
    avg_sale = mean(last_sale_price, na.rm = TRUE)
  )

eight <- df %>%
  dplyr::filter(year_of_sale == 2018) %>%
  dplyr::select(year_of_sale, last_sale_price)

nine <- df %>%
  dplyr::filter(year_of_sale == 2019) %>%
  dplyr::select(year_of_sale, last_sale_price)

test <- t.test(eight$last_sale_price, nine$last_sale_price)
test$statistic

t.test(df$last_sale_price, alternative = "less", mu = 8000000)

hist(df$land_sqft, breaks = 20)
# skewed right
round(mean(df$sqf_tmain, na.rm = TRUE))
############################################333
nba <- read.csv("nba.csv") %>%
  janitor::clean_names()

model <- lm(w ~ fg + ft + orb + drb + stl + tov, data = nba)
summary(model)

new <- data.frame(
  fg = c(3300),
  ft = c(1900),
  orb = c(1100),
  drb = c(2550),
  stl = c(750),
  tov = c(1254)
)

predict(model, newdata = new, interval = "confidence")

nba_test <- read.csv("nba_test.csv") %>%
  janitor::clean_names()

preds <- predict(model, newdata = nba_test, interval = "confidence") %>%
  data.frame()

nba_test %>%
  mutate(ord = row_number()) %>%
  left_join(preds %>%
              mutate(ord = row_number()), 
            by = c("ord")) %>%
  dplyr::select(w, fit) %>%
  mutate(diff = (fit - w)^2) %>%
  dplyr::summarise(
    ssd = round(sum(diff, na.rm = TRUE))
  )



