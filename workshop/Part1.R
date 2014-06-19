## 1. PRELIMINARIES
#     (a) What are time course data?
#     (b) Some challenges of analyzing time course data
## 2. PREPARING FOR GCA
#     (a) load required packages
library(ggplot2) #graphing
library(lme4) #multilevel regression
#     (b) other helpful packages
library(plyr) #split-apply-combine
library(reshape2) #melt and cast
library(multcomp) #testing multiple comparisons

## 3. PLOTTING WITH GGPLOT2
# basic line+points plot
ggplot(Orange, aes(x=age, y=circumference, color=Tree)) + geom_point() + geom_line()
# B&W version
ggplot(Orange, aes(x=age, y=circumference, shape=Tree, linetype=Tree)) + geom_point() + geom_line()
# compute summary on-the-fly
ggplot(Orange, aes(age, circumference)) + stat_summary(fun.y=mean, geom="line") 
last_plot() + geom_point()
# exclude cases on-the-fly
ggplot(subset(Orange, Tree!="5"), aes(age, circumference)) + stat_summary(fun.y=mean, geom="line") + geom_point()
# other summary statistics
ggplot(Orange, aes(age, circumference)) + stat_summary(fun.data=mean_se, geom="pointrange")
# show distribution information
ggplot(Orange, aes(factor(age), circumference)) + geom_boxplot()
ggplot(Orange, aes(factor(age), circumference)) + geom_violin()
# customizing graphs for publication
ggplot(Orange, aes(age, circumference)) + stat_summary(fun.data=mean_se, geom="pointrange") + theme_bw(base_size=12) + labs(x="Age in days", y="Size")
# control over image output
ggsave("fig.pdf", width=3, height=3)
# "small multiples", aka facets
ggplot(Orange, aes(age, circumference)) + facet_wrap(~ Tree) + geom_line()
ggplot(Orange, aes(age, circumference)) + facet_wrap(~ Tree, ncol=1) + geom_line()

## 4. FORMATTING DATA FOR PLOTTING AND GCA
#     (a) merge
#     (b) wide vs. long formats, reshape2 
# example: how negative affect is influenced by different 9-minute film exerpts (from psych package)
load("Affect.Rdata")
summary(affect.subset)
head(affect.subset)
#this is "wide" format, convert to "long":
affect.subset$SubjNum <- 1:dim(affect.subset)[1] #add a subject number
affect.m <- melt(affect.subset, id=c("SubjNum", "Study", "Film"), measure=c("NA1", "NA2"))
summary(affect.m)
#default variable names are not very informative, customize them:
affect.m <- melt(affect.subset, measure=c("NA1", "NA2"), variable.name="TestTime", value.name="NegAffect")
summary(affect.m)
#now can use ggplot
ggplot(affect.m, aes(Film, NegAffect, fill=TestTime)) + geom_boxplot()
#cast can convert from long to wide format, I mostly use it to make summary tables
dcast(affect.m, Film ~ TestTime, value.var = "NegAffect", mean)

## 5. CONCEPTUAL OVERVIEW OF GCA
#     (a) Linear regression reminder
#     (b) Extension to multilevel regression
#     (c) Fixed vs. Random effects
#     (d) Maximum likelihood estimation
## 6. SIMPLE (LINEAR) GCA EXAMPLE
#load example data sets
load("Examples.Rdata")
#look at the visual search data
summary(VisualSearchEx)
#plot the data
ggplot(VisualSearchEx, aes(Set.Size, RT, color=Dx)) + stat_summary(fun.data=mean_se, geom="pointrange")
#fit a base model
vs.null <- lmer(RT ~ 1 + (Set.Size | Participant), data=VisualSearchEx, REML=F)
#add effect of set size
vs <- lmer(RT ~ Set.Size + (Set.Size | Participant), data=VisualSearchEx, REML=F)
#add effect of diagnosis
vs.0 <- lmer(RT ~ Set.Size + Dx + (Set.Size | Participant), data=VisualSearchEx, REML=F)
#add interaction
vs.1 <- lmer(RT ~ Set.Size * Dx + (Set.Size | Participant), data=VisualSearchEx, REML=F)
#compare models
anova(vs.null, vs, vs.0, vs.1)

#plotting model fit
ggplot(VisualSearchEx, aes(Set.Size, RT, color=Dx)) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(aes(y=fitted(vs.0)), fun.y=mean, geom="line")
#compare with full model fit
last_plot() + stat_summary(aes(y=fitted(vs.1)), fun.y=mean, geom="line", linetype="dashed")

##########################
# BREAK
# Exercise 1: analyze the state-level suicide rate data from the WISQARS (wisqars.suicide)
#  did the regions differ in their baseline (1999) suicide rates?
#  did the regions differ in their rates of change of suidice rate?
#  plot observed data and model fits
##########################

## 7. NON-LINEAR GCA: CONCEPTUAL ISSUES
#     (a) Choosing a functional form: adequacy, dynamics consistency, predictions
#     (b) Natural and orthogonal polynomials
## 8. NON-LINEAR GCA: EXAMPLE
# effect of TP on word learning
summary(WordLearnEx)
ggplot(WordLearnEx, aes(Block, Accuracy, color=TP)) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(fun.y=mean, geom="line")

#make orthogonal polynomial
t <- poly(1:10, 2)
t
#it can be a good idea to pull the range directly from your data set
t <- poly(1:max(WordLearnEx$Block), 2)
t

#add it into the data frame
WordLearnEx[, paste("ot", 1:2, sep="")] <- t[WordLearnEx$Block, 1:2]
#re-check data
summary(WordLearnEx)
#orthogonal polynomial time
ggplot(WordLearnEx, aes(Block, ot1)) + stat_summary(fun.y=mean, geom="line")
last_plot() + stat_summary(aes(y=ot2), fun.y=mean, geom="line", color="red")

#fit base model
m.base <- lmer(Accuracy ~ (ot1+ot2) + (ot1 + ot2 | Subject), data=WordLearnEx, REML=F)
#add effect of TP on intercept 
m.0 <- lmer(Accuracy ~ (ot1+ot2) + TP + (ot1 + ot2 | Subject), data=WordLearnEx, REML=F)
#add effect on slope
m.1 <- lmer(Accuracy ~ (ot1+ot2) + TP + TP:ot1 + (ot1 + ot2 | Subject), data=WordLearnEx, REML=F)
#add effect on quadratic
m.2 <- lmer(Accuracy ~ (ot1+ot2)*TP + (ot1+ot2 | Subject), data=WordLearnEx, REML=F)
#model comparisons
anova(m.base, m.0, m.1, m.2)

#plot model fit
ggplot(WordLearnEx, aes(Block, Accuracy, color=TP)) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(aes(y=fitted(m.2)), fun.y=mean, geom="line")

#parameter estimates
summary(m.2)
coefs <- data.frame(coef(summary(m.2)))
#parameter-specific p-values: use normal approximation
coefs$p <- 2*(1-pnorm(abs(coefs$t.value)))
coefs

#Alternative: use lmerTest to get Satterthwaite approximation
library(lmerTest)
# lmerTest will take over the lmer function
# Pro: you can use the same model-fitting code
m.2t <- lmer(Accuracy ~ (ot1+ot2)*TP + (ot1+ot2 | Subject), data=WordLearnEx, REML=F)
summary(m.2t)
# Con: your model is now a lmerTest object rather than a lmerMod object and has somewhat different behavior
detach("package:lmerTest", unload=T)

# 9. MORE ABOUT RANDOM EFFECTS
#    (a) WISQARS data: different random effects structures example
#    (b) Keep it maximal
#    (c) convergence problems can sometimes be addressed by simplifying the random effects structure
#        (i) remove higher-order terms
#        (ii) remove correlations
#        (iii) comparing model fits can help decide which random effects are least important

## 10. WITHIN-SUBJECT EFFECTS
# Example: Target fixation in spoken word-to-picure matching (VWP)
#plot data
ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(aes(fill=Condition), fun.data=mean_se, geom="ribbon", color=NA, alpha=0.3) +
  theme_bw(base_size=10) + expand_limits(y=c(0,1)) + 
  labs(y="Fixation Proportion", x="Time since word onset (ms)")

#make 3rd-order orthogonal polynomial
t <- poly(1:max(TargetFix$timeBin), 3)
#add it into data frame
TargetFix[,paste("ot", 1:3, sep="")] <- t[TargetFix$timeBin, 1:3]

#fit full model
m.full <- lmer(meanFix ~ (ot1+ot2+ot3)*Condition + #fixed effects
                 (ot1+ot2+ot3 | Subject) + 
                 (ot1+ot2+ot3 | Subject:Condition), #random effects
               data=TargetFix, REML=F)
summary(m.full)
# look at random effects
str(ranef(m.full))
head(ranef(m.full)$"Subject")
head(ranef(m.full)$"Subject:Condition")
VarCorr(m.full)
# what is being estimated?
# (a) random variance and covariance
# (b) unit-level random effects
# this is why df for parameter estimates are poorly defined in MLR

#alternative random effect structure
m.alt <- lmer(meanFix ~ (ot1+ot2+ot3)*Condition + #fixed effects
                ((ot1+ot2+ot3)*Condition | Subject), #random effects
              data=TargetFix, REML=F)
str(ranef(m.alt))
head(ranef(m.alt)$"Subject")
VarCorr(m.alt)
#this alternative version makes fewer assumptions: 
# unequal variances across conditions
# more flexible covariance structure between random effect terms
#but requires more parameters...

###############
## PARTICIPANTS AS FIXED VS. RANDOM EFFECTS
# Treating participants as fixed effects produces more flexible model, perhaps too flexible
# - Shrinkage
# - Generalization
# Example:
m.pfix <- lmer(meanFix ~ (ot1+ot2+ot3)*Condition + (ot1+ot2+ot3)*Subject + #fixed effects
                 (ot1+ot2+ot3 | Subject:Condition), #random effects
               data=TargetFix, REML=F)
#fixed effects
coef(summary(m.pfix))
#compare with participants as random effects
coef(summary(m.full))
#compare model fits
anova(m.pfix, m.full)

# Bottom line: Treating participants as random effects captures the typical assumption of random sampling from some population to which we wish to generalize. Treating participants as fixed effects can be appropriate when this is not the case (e.g., neurological case studies).

##########################
# ANALYSIS TIME
# try GCA on your own data, ask for help if you need it
# if you don't have any appropriate data on hand, try one (or more) of these exercises:
#
# Exercise 2: Categorical perception (CP: d' peak at category boundary)
#  compare categorical perception along spectral vs. temporal dimensions using second-order orthogonal polynomial
#  which terms show significant effects of dimension type? (model comparisons)
#  estimate parameter-specific p-values using normal approximation and Satterthwaite approximation (lmerTest): to what extent do model comparisons and the two parameter-specific approaches yield the same results?
#  plot observed and model fit data
#
# Exercise 3: analyze the combined effects of task difficulty and impairment (alcohol) on motor learning (MotorLearning)
#  plot the observed data
#  run a basic GCA with third-order orthogonal polynomials
#  re-code variables to get main effects instead of simple effects (i.e., set factor contrasts to "sum")
#  re-run GCA and compare results
##########################
