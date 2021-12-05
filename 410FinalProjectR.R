library(dplyr)
library(Lahman)
library(ggplot2)
library(caret)
library(leaps)


# Find the number of total wins for every pitcher

y = Pitching %>% group_by(playerID) %>%
                 summarise(W = sum(W))
y = arrange(y, desc(y$W))
y = y[500:1000,]

# create X based on Pitching statistics

X = Pitching %>% filter(playerID %in% y$playerID) %>% 
  group_by(playerID) %>%
  summarise(ERA = mean(ERA), HR = sum(HR), G = sum(G), 
            SO = sum(SO), WP = sum(WP), L = sum(L), SHO = sum(SHO))

# Combine X and y to create data frame

data = merge(X, y, by = "playerID", all.x=TRUE)
data = data[complete.cases(data),]

#View(data)
#dim(data)

# Visualize

X = data[,2:8]

X = data.matrix(X)
yval = as.numeric(data$W)

newdata = data.frame(cbind(yval, X))

View(head(newdata))

model = lm(yval ~ X)

# Check MLR assumptions

summary(model)
plot(model)

#######

# First perform SLR

ggplot(newdata, aes(x = ERA, y = yval)) +
  geom_point() +
  stat_smooth() +
  ylab("Wins") +
  ggtitle("ERA vs Wins")

summary(lm(newdata$yval ~ newdata$ERA))

ggplot(newdata, aes(x = SO, y = yval)) +
  geom_point() +
  stat_smooth() +
  ylab("Wins") +
  ggtitle("Strike Outs vs Wins")

summary(lm(newdata$yval ~ newdata$SO))

ggplot(newdata, aes(x = G, y = yval)) +
  geom_point() +
  stat_smooth() +
  ylab("Wins") +
  ggtitle("Games Played vs Wins")

summary(lm(newdata$yval ~ newdata$G))

#######


# F statistic

anova(model)


# Check for colinearity between each x value.

plot(newdata[c(2,4,3,5,6,7,8)])
View(cor(newdata[c(2,3,4,5,6,7,8)]))

# Largest correlation is HRs vs SO at 0.65
# We consider larger than .9 to be an issue with multicollinearity, 
# so this is fine

model.indiv = lm(yval ~ ERA + HR + G + SO + WP + L + SHO, data = newdata)

#VIF = 1/(1 - summary(model.indiv)$r.squared) is total VIF

# Calculate individual VIF's
vif(model.indiv)

##########

plot(predict(model), newdata$yval,
     xlab = "Fitted Values",
     ylab = "Observed Values",
     main = "Observed Values vs Fitted Values")
abline(lm(newdata$yval ~ predict(model)),
       col = "red", lwd = 2)


regsubsetsObj <- regsubsets(x=X ,y=yval, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")

# This tells us that our highest R^2 value is about 0.36, and this occurs
# if we remove ERA and WP from our model. We should check statistical 
# significance when these variables are removed. 
# We can also see that we can achieve an R^2 value of nearly 0.35 from just
# SO, L, and SHO. Since this is the model that retains a decent R^2 value and 
# it has the least amount of regressors, we should test it's significance as
# well


model.both = lm(yval ~ X[,c(2:4,6,7)])
anova(model, model.both)

summary(model.both)

# Every stat is significant at the 0.05 level. 


# Now we test the very reduced model (only SO, L, and SHO)

model.least = lm(yval ~ X[,c(4,6,7)])
anova(model, model.least)

summary(model.least)

# Its good, R^2 of 0.3463 and everything is significant at 0.01 level

# Check assumptions
plot(model.least)

# Plot it and find p values

plot(predict(model.least), newdata$yval,
     xlab = "Fitted Values",
     ylab = "Observed Values",
     main = "Observed Values vs Fitted Values")
abline(lm(newdata$yval ~ predict(model.least)),
       col = "red", lwd = 2)

summary(model.least)



