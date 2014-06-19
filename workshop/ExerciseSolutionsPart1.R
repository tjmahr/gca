# Exercise 1: WISQARS suicide rate data
wisqars.suicide$Time <- wisqars.suicide$Year - 1999
m.base <- lmer(Crude.Rate ~ Time + (Time | State), data = wisqars.suicide, REML=F)
coef(summary(m.base))
m.0 <- lmer(Crude.Rate ~ Time + Region + (Time | State), data = wisqars.suicide, REML=F)
anova(m.base, m.0)
# regions differed in baseline suicide rate
m.1 <- lmer(Crude.Rate ~ Time * Region + (Time | State), data = wisqars.suicide, REML=F)
anova(m.base, m.0, m.1)
# regions did not differ in rate of change in suicide rate
# plot observed data and (full) model fit
ggplot(wisqars.suicide, aes(Year, Crude.Rate, color=Region)) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(aes(y=fitted(m.1)), fun.y=mean, geom="line") + 
  scale_x_continuous(breaks=1999:2007)

####################
# Exercise 2: Categorical perception
summary(CP)
s <- poly(unique(CP$Stimulus), 2)
CP[, paste("ostim", 1:2, sep="")] <- s[CP$Stimulus, 1:2]
m.base <- lmer(d.prime ~ (ostim1 + ostim2) + (ostim1 + ostim2 | Participant), data=CP, REML=F)
m.0 <- lmer(d.prime ~ (ostim1 + ostim2) + Type + (ostim1 + ostim2 | Participant), data=CP, REML=F)
m.1 <- lmer(d.prime ~ (ostim1 + ostim2) + Type + ostim1:Type + (ostim1 + ostim2 | Participant), data=CP, REML=F)
m.cp <- lmer(d.prime ~ (ostim1 + ostim2)*Type + (ostim1 + ostim2 | Participant), data=CP, REML=F)
anova(m.base, m.0, m.1, m.cp)
coefs <- as.data.frame(coef(summary(m.cp)))
coefs$p <- 2*(1-pnorm(abs(coefs[,3])))
#lmerTest
library(lmerTest)
m.cp.lT <- lmer(d.prime ~ (ostim1 + ostim2)*Type + (ostim1 + ostim2 | Participant), data=CP, REML=F)
summary(m.cp.lT)
#plot
ggplot(CP, aes(Stimulus, d.prime, color=Type)) + stat_summary(fun.data=mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.cp)), fun.y=mean, geom="line")

####################
# Exercise 3: Motor learning
summary(MotorLearning)
#plot the data
ggplot(MotorLearning, aes(Trial, Accuracy, color=Difficulty)) + 
  facet_wrap(~ Condition) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(fun.y=mean, geom="line")
#basic GCA
t <- poly(1:30, 3)
MotorLearning[, paste("ot", 1:3, sep="")] <- t[MotorLearning$Trial, 1:3]
m.ML <- lmer(Accuracy ~ (ot1+ot2+ot3) * Difficulty*Condition +
               (ot1+ot2+ot3 | SubjID) + 
               (ot1+ot2+ot3 | SubjID:Difficulty:Condition),
             data=MotorLearning, REML=FALSE)
summary(m.ML)
coef(summary(m.ML))

#re-code variables for more intuitive/informative parameter estimates
MotorLearning$Difficulty <- relevel(MotorLearning$Difficulty, "Low")
MotorLearning$DifficultySum <- C(MotorLearning$Difficulty, sum)
MotorLearning$ConditionSum <- C(MotorLearning$Condition, sum)
#fit new model using these variables
m.MLsum <- lmer(Accuracy ~ (ot1+ot2+ot3) * DifficultySum*ConditionSum +
                  (ot1+ot2+ot3 | SubjID) +
                  (ot1+ot2+ot3 | SubjID:DifficultySum:ConditionSum),
                data=MotorLearning, REML=FALSE)
coef(summary(m.MLsum))
