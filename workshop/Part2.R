library(ggplot2) 
library(lme4) 
library(plyr)
load("Examples.Rdata")

# 1. CATEGORICAL OUTCOMES
#    (a) why logistic regression is important
# 2. LOGISTIC GCA
summary(TargetFix)
#build 3rd-order orth poly
t <- poly(1:max(TargetFix$timeBin), 3)
TargetFix[, paste("ot", 1:3, sep="")] <- t[TargetFix$timeBin, 1:3]
#logistic GCA: Y/N outcome variable, family=binomial
m.log <- glmer(cbind(sumFix, N-sumFix) ~
                 (ot1+ot2+ot3)*Condition +
                 (ot1+ot2+ot3 | Subject) +
                 (ot1+ot2 | Subject:Condition),
               data=TargetFix, family=binomial)
#much slower to fit and prone to convergence failure 
#  note simplified random effect structure and convergence warning
#  a warning is not an error: estimates and SE are exactly the same as from earlier version of lme4 that did not produce a warning
coef(summary(m.log))
#parameter estimates are on logit scale
#compare with linear GCA
m.lin <- lmer(meanFix ~ 
                (ot1+ot2+ot3)*Condition +
                (ot1+ot2+ot3 | Subject) + 
                (ot1+ot2+ot3 | Subject:Condition),
               data=TargetFix, REML=F)
coef(summary(m.lin))
#plot model fit. fitted() conveniently returns proportions
ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=fitted(m.log)), fun.y=mean, geom="line") +
  theme_bw(base_size=10) + expand_limits(y=c(0,1)) + 
  labs(y="Fixation Proportion", x="Time since word onset (ms)")

# 3. QUASI-LOGISTIC GCA
#    (a) finite data sets, granularity near boundaries (0 and 1)
#    (b) convergence etc.
#    (c) "Empirical" logit
TargetFix$elog <- with(TargetFix, log((sumFix+0.5) / (N-sumFix+0.5)))
# Weights are recommended for taking observation reliability into account
TargetFix$wts <- with(TargetFix, 1/(sumFix+0.5) + 1/(N-sumFix+0.5))
# fit empirical logit model
m.elog <- lmer(elog ~ (ot1+ot2+ot3)*Condition +
                 (ot1+ot2+ot3 | Subject) +
                 (ot1+ot2+ot3 | Subject:Condition),
               data=TargetFix, weights=1/wts, REML=F)
coef(summary(m.elog))
#parameter estimates are on same(ish) scale as logistic GCA
#plot model fit
ggplot(TargetFix, aes(Time, elog, color=Condition)) + 
  stat_summary(fun.data = mean_se, geom="pointrange") + 
  stat_summary(aes(y=fitted(m.elog)), fun.y = mean, geom="line") + 
  ylab("Fixation Empirical Log-Odds")

# 4. VISUALIZING HIGHER-ORDER POLYNOMIAL TERMS
# One problem with using polynomials is that it can be hard to mentally visualize effects on the higher-order terms
# Example: function vs. thematic competition in VWP
summary(FunctTheme)
#all data
ggplot(FunctTheme, aes(Time, meanFix, color=Object, fill=Object)) + facet_wrap(~ Condition) + 
  stat_summary(fun.y=mean, geom="line") + 
  stat_summary(fun.data=mean_se, geom="ribbon", color=NA, alpha=0.3) + 
  theme_bw(base_size=10) + labs(x="Time Since Word Onset (ms)", y="Fixation Proportion") +
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background=element_rect(color="black", fill="white"))
#competition effects
ggplot(subset(FunctTheme, Object!="Target"), aes(Time, meanFix, color=Object, fill=Object)) + facet_wrap(~ Condition) + 
  stat_summary(fun.y=mean, geom="line") + 
  stat_summary(fun.data=mean_se, geom="ribbon", color=NA, alpha=0.3) + 
  theme_bw(base_size=10) + labs(x="Time Since Word Onset (ms)", y="Fixation Proportion") +
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background=element_rect(color="black", fill="white"))

#prep data for GCA
FunctTheme$timeBin <- FunctTheme$Time/50 - 9
t <- poly(1:max(FunctTheme$timeBin), 4)
FunctTheme[,paste("ot", 1:4, sep="")] <- t[FunctTheme$timeBin, 1:4]

#fit full model
m.ft<-lmer(meanFix ~ (ot1+ot2+ot3+ot4)*Object*Condition + 
             (1+ot1+ot2+ot3+ot4 | Subject) + 
             (1+ot1+ot2+ot3+ot4 | Subject:Object:Condition), 
           data=subset(FunctTheme, Object != "Target"), REML=F)
#look at parameter estimates
coefs.ft <- as.data.frame(coef(summary(m.ft)))
coefs.ft$p <- format.pval(2*(1-pnorm(abs(coefs.ft[,"t value"]))))
coefs.ft
#critical effect only significant for ot3 and ot4

#check model fit
#combine observed data subset and model-fitted values
data.comp <- data.frame(subset(FunctTheme, Object != "Target"), GCA_Full=fitted(m.ft))
#make plot
ggplot(data.comp, aes(Time, meanFix, color=Object)) + facet_wrap(~ Condition) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(aes(y=GCA_Full), fun.y=mean, geom="line") + 
  theme_bw(base_size=10) + labs(x="Time Since Word Onset (ms)", y="Fixation Proportion") +
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background=element_rect(color="black", fill="white"))
#model fit looks pretty good, but do those ot3 and ot4 effects correspond to the early-late difference?
#we can try to answer that question by removing those effects from the model and visually comparing the model fit

#fit a reduced model
#for a statistical model comparison, we would only reduce the fixed effects
#for this visual comparison we need to reduce both the fixed and random effects
m.reduced <- lmer(meanFix ~ (ot1+ot2+ot3+ot4)*Object + (ot1+ot2+ot3+ot4)*Condition + 
                    (ot1+ot2)*Object*Condition + #remove ot3 and ot4 interactions from fixed effects
                    (ot1+ot2+ot3+ot4 | Subject) + 
                    (ot1+ot2 | Subject:Object:Condition), #also remove from random effects!
                  data=subset(FunctTheme, Object != "Target"), REML=F)
#look at parameter estimates
coef(summary(m.reduced))
#add reduced-model-fitted values to data frame
data.comp$GCA_Reduced <- fitted(m.reduced)

#try to plot the everything...
ggplot(data.comp, aes(Time, meanFix, color=Object)) + facet_wrap(~ Condition) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(aes(y=GCA_Full), fun.y=mean, geom="line", size=2) + #full model fit will be thicker lines
  stat_summary(aes(y=GCA_Reduced), fun.y=mean, geom="line") + 
  theme_bw(base_size=10) + labs(x="Time Since Word Onset (ms)", y="Fixation Proportion") +
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background=element_rect(color="black", fill="white"))
#sort of looks like Function competition effect is earlier in reduced model and Thematic competition effect is later 
#meh, kind of hard to see what is going on
#compute competition effect size (Comp - Unrel) so we have half as much to plot
ES <- ddply(data.comp, .(Subject, Time, Condition), summarize, 
            Competition = meanFix[Object=="Competitor"] - meanFix[Object=="Unrelated"], 
            GCA_Full = GCA_Full[Object=="Competitor"] - GCA_Full[Object=="Unrelated"], 
            GCA_Reduced = GCA_Reduced[Object=="Competitor"] - GCA_Reduced[Object=="Unrelated"])
summary(ES)
#plot effect size time course
ggplot(ES, aes(Time, Competition, color=Condition)) + 
  stat_summary(fun.y=mean, geom="point") + 
  stat_summary(aes(y=GCA_Full), fun.y=mean, geom="line") + 
  stat_summary(aes(y=GCA_Reduced), fun.y=mean, geom="line", linetype="dashed") + 
  theme_bw(base_size=10) + labs(x="Time Since Word Onset (ms)", y="Competition") + 
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.background=element_rect(color="black", fill="white"))
#now can see that solid lines pull apart the time course, but the dashed lines are almost the same
#that is, the full model (with ot3:Object:Condition and ot4:Object:Condition) captured the time course difference, but the reduced model did not

##########################
# ANALYSIS TIME
# try GCA on your own data, ask for help if you need it
# if you don't have any appropriate data on hand, try one (or more) of these exercises:
#
# Exercise 4: Re-analyze the word learning data (WordLearnEx) using logistic and quasi-logistic GCA
#  You'll need to convert the accuracy proportions to counts of correct and incorrect responses. To do that you need to know that there were 6 trials per block.
#  Compare the linear, logistic, and quasi-logistic (empirical logit) GCA results: are there any differences? If so, what are they and what do they mean?
#  Plot the model fits for each analysis
#
# Exercise 5: using the TargetFix data, make plots showing the effects of Condition on each of the time terms (intercept, linear, quadratic and cubic)
##########################