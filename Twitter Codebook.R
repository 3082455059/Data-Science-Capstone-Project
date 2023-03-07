tweets <- read.csv(file.choose(), header = TRUE)
dim(tweets)  # 26093   20
summary(tweets)

unique(tweets$retweet_count)

unique(tweets$reply_count)

unique(tweets$like_count)

unique(tweets$quote_count)

length(tweets$followers_count[tweets$followers_count == ''])

unique(tweets$data_concern_relevant)

length(tweets$text[tweets$text == ''])

unique(tweets$verified)

length(tweets$created_at_x[tweets$created_at_x == ''])


tweets$created_at <- tweets$created_at_x
tweets$created_at <- strptime(tweets$created_at, "%Y-%m-%d")

tweets$created_at_x[tweets$created_at_x <= '2022-05-01'] <- 1
tweets$created_at_x[tweets$created_at_x > '2022-05-01' & tweets$created_at_x <= '2022-06-23'] <- 2
tweets$created_at_x[tweets$created_at_x > '2022-06-23' & tweets$created_at_x <= '2022-07-23'] <- 3
tweets$created_at_x[tweets$created_at_x != 1 & tweets$created_at_x != 2 & tweets$created_at_x != 3] <- 4

split <- strsplit(tweets$text, " ")
tweets$tweet_length <- sapply(split , length)

tweets$engagement <- (tweets$retweet_count + tweets$reply_count + tweets$like_count + tweets$quote_count + 1) / (tweets$followers_count + 1)
# 113 NAs, 9439 0's

attach(tweets)


par(mfrow = c(2, 2))
boxplot(engagement ~ data_concern_relevant, data = subset(tweets, created_at_x == '1'), main = 'Pre Leak', ylim = c(0, 180))
boxplot(engagement ~ data_concern_relevant, data = subset(tweets, created_at_x == '2'), main = 'Between  Leak and Overturn', ylim = c(0, 180))
boxplot(engagement ~ data_concern_relevant, data = subset(tweets, created_at_x == '3'), main = 'Overturn', ylim = c(0, 180))
boxplot(engagement ~ data_concern_relevant, data = subset(tweets, created_at_x == '4'), main = 'Post Overturn', ylim = c(0, 180))

# cor(retweet_count, reply_count)        # 0.9405534
# cor(retweet_count, like_count)         # 0.8006877
# cor(retweet_count, quote_count)        # 0.9932745
# cor(retweet_count, followers_count)    # 0.00512691
# cor(reply_count, like_count)           # 0.8035177
# cor(reply_count, quote_count)          # 0.955584
# cor(reply_count, followers_count)      # 0.03603507
# cor(like_count, quote_count)           # 0.8159655
# cor(like_count, followers_count)       # 0.002837
# cor(quote_count, followers_count)      # 0.01696403


fit <- lm(engagement ~ as.factor(created_at_x) + as.factor(data_concern_relevant) + as.factor(verified) + tweet_length)

library(MASS)
boxcox <- boxcox(fit)
boxcox$x[which.max(boxcox$y)] # 0

# All-subset selection 
library(leaps)

x <- cbind(as.factor(created_at_x), as.factor(data_concern_relevant), as.factor(verified), tweet_length)
y <- log(engagement)

result_cp=leaps(x,y, method="Cp")
which.min(result_cp$Cp)
order(result_cp$Cp) 
result_cp$which[15,] # TRUE TRUE TRUE TRUE 

result_adjr2=leaps(x,y, method="adjr2")
which.max(result_adjr2$adjr2)
order(result_adjr2$adjr2)
result_adjr2$which[15,] # TRUE TRUE TRUE TRUE 

# # Automatic selection process
# library(MASS)
# stepback_aic=step(fit, direction="both", k=2) # AIC
# 
# stepback_bic=step(fit, direction="both", k=log(length(engagement))) #BIC


# # Cross validation
# set.seed(10000)
# CV1 = CV2 <- 0
# n <- dim(tweets)[1]
# n.shuffle <- sample(1:n,n,replace=FALSE)
# id.cv <- list()
# id.cv[[1]] <- n.shuffle[1:2609]
# id.cv[[2]] <- n.shuffle[2610:5218]
# id.cv[[3]] <- n.shuffle[5219:7827]
# id.cv[[4]] <- n.shuffle[7828:10436]
# id.cv[[5]] <- n.shuffle[10437:13045]
# id.cv[[6]] <- n.shuffle[13046:15634]
# id.cv[[7]] <- n.shuffle[15635:18263]
# id.cv[[8]] <- n.shuffle[18264:20872]
# id.cv[[9]] <- n.shuffle[20873:23481]
# id.cv[[10]] <- n.shuffle[23482:26093]
# 
# for(i in 1:10)
# {
#   fit1 <- lm(engagement ~ as.factor(created_at_x) + as.factor(data_concern_relevant) + tweet_length)
#   fit2 <- lm(engagement ~ as.factor(created_at_x) + as.factor(data_concern_relevant))
#   
#   CV1 <- CV1+ (1/n)*sum(engagement[id.cv[[i]]] - predict(fit1,tweets[id.cv[[i]],]) )^2
#   CV2 <- CV2+ (1/n)*sum(engagement[id.cv[[i]]] - predict(fit2,tweets[id.cv[[i]],]) )^2
# }
# 
# print(c(CV1,CV2))
# # 23296381 23314228


fit2 <- lm(log(engagement) ~ as.factor(created_at_x) * as.factor(data_concern_relevant) + as.factor(verified) + tweet_length)
summary(fit2)
plot(fit2)

# library(tidyverse)
# cooksD <- cooks.distance(fit1.transform)
# influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
# influential
# names_of_influential <- names(influential)
# outliers <- tweets[names_of_influential,]
# tweets.transform <- tweets %>% anti_join(outliers)

# fit2.transform <- lm(log(engagement) ~ as.factor(created_at_x) * as.factor(data_concern_relevant) + as.factor(verified) + tweet_length, data = tweets.transform)
# summary(fit2.transform)
# plot(fit2.transform)

color.pat <- ifelse(data_concern_relevant == '1', 'red', 'blue')
plot(created_at, log(engagement), col = alpha(color.pat, 0.1))
privacy <- smooth.spline(created_at[data_concern_relevant == '1'], log(engagement[data_concern_relevant == '1']))
lines(privacy$x, privacy$y, lty = 3, col = 'red', lwd = 4)
non.privacy <- smooth.spline(created_at[data_concern_relevant == '0'], log(engagement[data_concern_relevant == '0']))
lines(non.privacy$x, non.privacy$y, lty = 3, col = 'blue', lwd = 4)
abline(v = as.POSIXct("2022-05-01"),lty = 3, col = 'orange', lwd = 2)
abline(v = as.POSIXct("2022-06-23"), lty = 3, col = 'orange', lwd = 2)
abline(v = as.POSIXct("2022-07-23"), lty = 3, col = 'orange', lwd = 2)

par(mfrow = c(2, 2))
boxplot(log(engagement) ~ data_concern_relevant, data = subset(tweets, created_at_x == '1'), main = 'Pre Leak', ylim = c(-15, 5))
boxplot(log(engagement) ~ data_concern_relevant, data = subset(tweets, created_at_x == '2'), main = 'Between  Leak and Overturn', ylim = c(-15, 5))
boxplot(log(engagement) ~ data_concern_relevant, data = subset(tweets, created_at_x == '3'), main = 'Overturn', ylim = c(-15, 5))
boxplot(log(engagement) ~ data_concern_relevant, data = subset(tweets, created_at_x == '4'), main = 'Post Overturn', ylim = c(-15, 5))


library(tidyverse)
arrange(subset(tweets, data_concern_relevant == '1'), engagement)

