#Exercise 4: logistic and quasi-logistic analysis of WordLearnEx
load("Examples.Rdata")
#There were 6 trials per block
WordLearnEx$Correct <- round(WordLearnEx$Accuracy * 6)
WordLearnEx$N <- 6
#compute empirical logits and weights
WordLearnEx$elog <- with(WordLearnEx, log((Correct+0.5) / (N-Correct+0.5)))
WordLearnEx$wts <- with(WordLearnEx, 1/(Correct+0.5) + 1/(N-Correct+0.5))
#make orth poly
t <- poly(unique(WordLearnEx$Block), 2)
WordLearnEx[,paste("ot", 1:2, sep="")] <- t[WordLearnEx$Block, 1:2]

#fit linear model
m.lin <- lmer(Accuracy ~ (ot1+ot2)*TP + (ot1+ot2 | Subject), data=WordLearnEx, REML=F)
coef(summary(m.lin))
ggplot(WordLearnEx, aes(Block, Accuracy, color=TP)) + 
  stat_summary(fun.data=mean_se, geom="pointrange") + 
  stat_summary(aes(y=fitted(m.lin)), fun.y=mean, geom="line")

#fit logistic model
m.logit <- glmer(cbind(Correct, N-Correct) ~ (ot1+ot2)*TP + (ot1+ot2 | Subject), data=WordLearnEx, family=binomial)
coef(summary(m.logit))
ggplot(WordLearnEx, aes(Block, Accuracy, color=TP)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.logit)), fun.y = mean, geom="line") + ylab("Accuracy")
logLik(m.logit)

#fit quasi-logistic model
m.elogit <- lmer(elog ~ (ot1+ot2)*TP + (ot1+ot2 | Subject), data=WordLearnEx, weights=1/wts, REML=F)
coef(summary(m.elogit))
ggplot(WordLearnEx, aes(Block, elog, color=TP)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.elogit)), fun.y = mean, geom="line") + ylab("Accuracy")
#why does the model fit deviate from the observed means?
#try re-fitting the model without weights
m.elogit.nowts <- lmer(elog ~ (ot1+ot2)*TP + (ot1+ot2 | Subject), data=WordLearnEx, REML=F)
coef(summary(m.elogit.nowts))
ggplot(WordLearnEx, aes(Block, elog, color=TP)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.elogit.nowts)), fun.y = mean, geom="line") + ylab("Accuracy")

####################
#Exercise 5: visualizing effects of Condition on different time terms
load("Examples.Rdata")
#prep for GCA
t <- poly(1:max(TargetFix$timeBin), 3)
TargetFix[,paste("ot", 1:3, sep="")] <- t[TargetFix$timeBin, 1:3]
#fit models and make plots showing each effect of adding each term
#for each plot, reduced model is shown by dashed lines, larger model is shown by solid lines
#store plot specifications in variables to be combined later
m.base <- lmer(meanFix ~ (ot1+ot2+ot3) + (ot1+ot2+ot3 | Subject), data=TargetFix, REML=F)
m.0 <- lmer(meanFix ~ (ot1+ot2+ot3) + Condition + (ot1+ot2+ot3 | Subject) + (1 | Subject:Condition), data=TargetFix, REML=F)
p1 <- ggplot(TargetFix, aes(Time, meanFix, color=Condition)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.0)), fun.y=mean, geom="line") + stat_summary(aes(y=fitted(m.base)), fun.y=mean, geom="line", linetype="dashed") + ggtitle("+ Condition")

m.1 <- lmer(meanFix ~ (ot1+ot2+ot3) + Condition + ot1:Condition + (ot1+ot2+ot3 | Subject) + (ot1 | Subject:Condition), data=TargetFix, REML=F)
p2 <- ggplot(TargetFix, aes(Time, meanFix, color=Condition)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.1)), fun.y=mean, geom="line") + stat_summary(aes(y=fitted(m.0)), fun.y=mean, geom="line", linetype="dashed") + ggtitle("+ ot1:Condition")

m.2 <- lmer(meanFix ~ (ot1+ot2)*Condition + ot3 + (ot1+ot2+ot3 | Subject) + (ot1+ot2 | Subject:Condition), data=TargetFix, REML=F)
p3 <- ggplot(TargetFix, aes(Time, meanFix, color=Condition)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.2)), fun.y=mean, geom="line") + stat_summary(aes(y=fitted(m.1)), fun.y=mean, geom="line", linetype="dashed")+ ggtitle("+ ot2:Condition")

m.full <- lmer(meanFix ~ (ot1+ot2+ot3)*Condition + (ot1+ot2+ot3 | Subject) + (ot1+ot2+ot3 | Subject:Condition), data=TargetFix, REML=F)
p4 <- ggplot(TargetFix, aes(Time, meanFix, color=Condition)) + stat_summary(fun.data = mean_se, geom="pointrange") + stat_summary(aes(y=fitted(m.full)), fun.y=mean, geom="line")+ stat_summary(aes(y=fitted(m.2)), fun.y=mean, geom="line", linetype="dashed") + ggtitle("+ ot3:Condition")

library(gridExtra)
grid.arrange(p1, p2, p3, p4)
#easy to see effect of Condition on intercept and ot2, not much effect on ot1 or ot3 -- which matches the statistical results

####################
#Exercise 6: individual differences in cohort and rhyme competition
load("Examples.Rdata")
#plot data
ggplot(CohortRhyme, aes(Time, FixProp, color=Object)) + facet_grid(Type ~ Group) + stat_summary(fun.y=mean, geom="point")

#prep for GCA
t <- poly(1:max(CohortRhyme$timeBin), 4)
CohortRhyme[,paste("ot", 1:4, sep="")] <- t[CohortRhyme$timeBin, 1:4]
#group effects on cohort competition
cohort.base <- lmer(FixProp ~ (ot1+ot2+ot3+ot4)*Object + (1+ot1+ot2+ot3+ot4 | subjID) + (1+ot1+ot2 | subjID:Object), data=subset(CohortRhyme, Type == "Cohort"), REML=F)
cohort.group <- lmer(FixProp ~ (ot1+ot2+ot3+ot4)*Object*Group + (1+ot1+ot2+ot3+ot4 | subjID) + (1+ot1+ot2 | subjID:Object), data=subset(CohortRhyme, Type == "Cohort"), REML=F)
anova(cohort.base, cohort.group)
#pair-wise group comparisons
x <- coef(summary(cohort.group))
x[21:30,]
library(multcomp)
contrast.matrix = rbind(
  "Intercept: Ctrl - Br" = c(rep(0, 20), 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  "Intercept: Ctrl - We" = c(rep(0, 20), 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  "Intercept: We - Br" = c(rep(0, 20), 1, -1, 0, 0, 0, 0, 0, 0, 0, 0),
  "Linear: Ctrl - Br" = c(rep(0, 20), 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  "Linear: Ctrl - We" = c(rep(0, 20), 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  "Linear: We - Br" = c(rep(0, 20), 0, 0, 1, -1, 0, 0, 0, 0, 0, 0),
  "Quad: Ctrl - Br" = c(rep(0, 20), 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  "Quad: Ctrl - We" = c(rep(0, 20), 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  "Quad: We - Br" = c(rep(0, 20), 0, 0, 0, 0, 0, 1, -1, 0, 0, 0)
)
summary(glht(cohort.group, contrast.matrix), test = adjusted("none"))

#group effects on rhyme competition
rhyme.base <- lmer(FixProp ~ (ot1+ot2+ot3+ot4)*Object + (1+ot1+ot2+ot3+ot4 | subjID) + (1+ot1+ot2 | subjID:Object), data=subset(CohortRhyme, Type == "Rhyme"), REML=F)
rhyme.group <- lmer(FixProp ~ (ot1+ot2+ot3+ot4)*Object*Group + (1+ot1+ot2+ot3+ot4 | subjID) + (1+ot1+ot2 | subjID:Object), data=subset(CohortRhyme, Type == "Rhyme"), REML=F)
anova(rhyme.base, rhyme.group)
#pairwise comparisons
summary(glht(rhyme.group, contrast.matrix), test = adjusted("none"))

#individual-level analyses
#get cohort effect sizes
blup.cohort <- data.frame(colsplit(row.names(ranef(cohort.base)$'subjID:Object'), ":", c("Subject", "Object")), ranef(cohort.base)$'subjID:Object')
ES.coh <- ddply(blup.cohort, .(Subject), summarize, 
                Cohort.Intercept = X.Intercept.[Object=="Competitor"] - X.Intercept.[Object=="Unrelated"], 
                Cohort.Linear = ot1[Object=="Competitor"] - ot1[Object=="Unrelated"], 
                Cohort.Quadratic = ot2[Object=="Competitor"] - ot2[Object=="Unrelated"])

#get rhyme effect sizes
blup.rhyme <- data.frame(colsplit(row.names(ranef(rhyme.base)$'subjID:Object'), ":", c("Subject", "Object")), ranef(rhyme.base)$'subjID:Object')
ES.rhy <- ddply(blup.rhyme, .(Subject), summarize, 
                Rhyme.Intercept = X.Intercept.[Object=="Competitor"] - X.Intercept.[Object=="Unrelated"], 
                Rhyme.Linear = ot1[Object=="Competitor"] - ot1[Object=="Unrelated"], 
                Rhyme.Quadratic = ot2[Object=="Competitor"] - ot2[Object=="Unrelated"])

#combine cohort and rhyme individual effect sizes
ES <- merge(ES.coh, ES.rhy, by="Subject")
group <- unique(subset(CohortRhyme, select=c(subjID, Group))) #get group assignments from original data frame
ES <- merge(ES, group, by.x="Subject", by.y="subjID")

#effect size correlations: Intercept
cor.test(ES$Cohort.Intercept, ES$Rhyme.Intercept)
cor.test(ES$Cohort.Intercept[ES$Group != "Control"], ES$Rhyme.Intercept[ES$Group != "Control"])
cor.test(ES$Cohort.Intercept[ES$Group == "Control"], ES$Rhyme.Intercept[ES$Group == "Control"])
#effect size correlations: Linear
cor.test(ES$Cohort.Linear, ES$Rhyme.Linear)
cor.test(ES$Cohort.Linear[ES$Group != "Control"], ES$Rhyme.Linear[ES$Group != "Control"])
cor.test(ES$Cohort.Linear[ES$Group == "Control"], ES$Rhyme.Linear[ES$Group == "Control"])
#effect size correlations: Quadratic
cor.test(ES$Cohort.Quadratic, ES$Rhyme.Quadratic)
cor.test(ES$Cohort.Quadratic[ES$Group != "Control"], ES$Rhyme.Quadratic[ES$Group != "Control"])
cor.test(ES$Cohort.Quadratic[ES$Group == "Control"], ES$Rhyme.Quadratic[ES$Group == "Control"])

#scatterplot
ES.m <- melt(ES, id=c("Subject", "Group"))
ES.m <- cbind(ES.m, colsplit(ES.m$variable, "\\.", c("Type", "Term")))
ES.c <- dcast(ES.m, Subject + Group + Term ~ Type)
ggplot(ES.c, aes(Cohort, Rhyme, color=Group)) + facet_wrap(~ Term, scales="free") + geom_point() + scale_color_manual(values=c("grey", "red", "blue"))
