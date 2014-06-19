library(ggplot2) 
library(lme4) 
library(reshape2)
load("Examples.Rdata")

# 1. INDIVIDUAL DIFFERENCES
#    individual differences provide an additional level of analysis for understanding phenomena
#      at a group level, a treatment works better than a placebo, but why does it work better for some people than for others?
#      people solve easy problems faster than hard problems, but why are some people a lot faster on the easy problems and other people only a little faster?
#
#    t-test and ANOVA methods treat individual differences as noise
#    multilevel regression provides two ways to quantify and analyze individual differences

#    (a) "External" individual differences can be added as fixed effects
#example: an aphasia treatment study aimed at improving picture naming
summary(NamingRecovery)
# let's just look at semantic errors
ggplot(NamingRecovery, aes(TestTime, Semantic.error)) + 
  stat_summary(fun.y=mean, geom="point", size=3) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) + 
  stat_summary(fun.y=mean, geom="line") +
  labs(x="Test Number", y="Semantic Errors (Proportion)") + 
  theme_bw(base_size=12) + expand_limits(y=c(0, 0.1))
#is decrease modulated by how long it has been since the stroke? (months post-onset)
m.sem <- lmer(Semantic.error ~ TestTime*MPO + 
                (TestTime | SubjectID), 
              data=NamingRecovery, REML=F)
summary(m.sem)
#looks like it is
#visualizing three continuous variables is a little tricky, so let's split up MPO
#a simple median split
NamingRecovery$MPO2 <- factor(NamingRecovery$MPO >= median(NamingRecovery$MPO), labels=c("Low","High"))
#tertile split
#   define break points
b <- quantile(NamingRecovery$MPO, probs=seq(0, 1, by=1/3)) 
#   split continuous predictor and provide level labels
NamingRecovery$MPO3 <- cut(NamingRecovery$MPO, breaks=b, include.lowest=T, labels=c("Low", "Medium", "High"))

ggplot(NamingRecovery, aes(TestTime, Semantic.error, color=MPO2)) + 
  stat_summary(fun.y=mean, geom="point", size=3) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) + 
  stat_summary(aes(y=fitted(m.sem)), fun.y=mean, geom="line") +
  labs(x="Test Number", y="Semantic Errors (Proportion)") + 
  theme_bw(base_size=12) + expand_limits(y=c(0, 0.1))

# (b) "Internal" individual differences don't have a measure that can be entered as a fixed effect or individual differences might be needed for a different analysis (e.g., VLSM)
#For such situations, random effects provide a way to quantify individual effect sizes in the context of a model of overall group performance
#   simple diagram of how this works...
#Example: function and thematic knowledge following stroke
summary(FunctThemePts)
#Data: Function and Thematic competition for 17 LH stroke patients
ggplot(subset(FunctThemePts, Time >= 0 & Time <= 2200 & Object != "Target"), aes(Time, meanFix, linetype=Object)) + facet_wrap(~ Condition, ncol=1) + stat_summary(fun.y=mean, geom="line") + stat_summary(fun.data=mean_se, geom="ribbon", color=NA, alpha=0.3, fill="gray") + geom_vline(xintercept=c(500, 2000)) + labs(y="Fixation Proportion", x="Time since word onset (ms)") + theme_bw(base_size=12)
#Question: is there a correlation between Function and Thematic competition across patients?
#prep data for GCA
FunctThemePts.gca <- subset(FunctThemePts, Time >= 500 & Time <= 2000 & Object != "Target")
summary(FunctThemePts.gca)
FunctThemePts.gca$timeBin <- FunctThemePts.gca$timeBin - 29
t <- poly((unique(FunctThemePts.gca$timeBin)), 4)
FunctThemePts.gca[, paste("ot", 1:4, sep="")] <- t[FunctThemePts.gca$timeBin, 1:4]
#fit full models
m.funct <- lmer(meanFix ~ (ot1+ot2+ot3+ot4)*Object +
                  (ot1+ot2+ot3+ot4 | subj) + (ot1+ot2 | subj:Object),
                data=subset(FunctThemePts.gca, Condition == "Function"), REML=F)
m.theme <- lmer(meanFix ~ (ot1+ot2+ot3+ot4)*Object +
                  (ot1+ot2+ot3+ot4 | subj) + (ot1+ot2 | subj:Object),
                data=subset(FunctThemePts.gca, Condition == "Thematic"), REML=F)
coef(summary(m.theme))
#let's remind ourselves what random effects look like
str(ranef(m.funct), vec.len=2)
head(ranef(m.funct)$"subj:Object")
#those row names are handy, let's pull them into the data frame
#   use colsplit (from reshape2) to divide the row names using the : as a separator
re.id <- colsplit(row.names(ranef(m.funct)$"subj:Object"), ":", c("Subject", "Object"))
#combine the row names with the random effect estimates
re.funct <- data.frame(re.id, ranef(m.funct)$"subj:Object")
head(re.funct)
#compute individual effect size as random effects difference between the two Conditions for each Subject for Intercept and Linear time term
ES.funct <- ddply(re.funct, .(Subject), summarize,
                  Function_Intercept = X.Intercept.[Object=="Competitor"] - X.Intercept.[Object=="Unrelated"], 
                  Function_Linear = ot1[Object=="Competitor"] - ot1[Object=="Unrelated"])
#same steps for thematic condition
re.theme <- data.frame(colsplit(row.names(ranef(m.theme)$"subj:Object"), ":", c("Subject", "Object")), 
                       ranef(m.theme)$"subj:Object")
ES.theme <- ddply(re.theme, .(Subject), summarize,
                  Thematic_Intercept = X.Intercept.[Object=="Competitor"] - X.Intercept.[Object=="Unrelated"],
                  Thematic_Linear = ot1[Object=="Competitor"] - ot1[Object=="Unrelated"])
#combine condition effect sizes
ES <- merge(ES.funct, ES.theme)
head(ES)
#explain these numbers...
#now can compute correlations between effect sizes
cor.test(ES$Function_Intercept, ES$Thematic_Intercept)
cor.test(ES$Function_Linear, ES$Thematic_Linear)
#both are significant negative correlations
#make two-panel scatterplot
#   first, re-arrange data
ES.m <- melt(ES, id="Subject")
ES.m <- cbind(ES.m, colsplit(ES.m$variable, "_", c("Condition", "Term")))
ES.c <- dcast(ES.m, Subject + Term ~ Condition)
head(ES.c)
ggplot(ES.c, aes(Function, Thematic)) + facet_wrap(~ Term, scales="free") + geom_point()

#Why bother with the model-fitting when you could just take the difference of the observed data to get effect sizes?
#Because that would *over*-estimate the individual differences
#Stein's paradox (Efron & Morris, 1977): re-visit QB example. Stein combined individual batting average and MLB grand mean batting average and variability to estimate of final batting average
#Return of the shrinkage: group and individual models have the same mean effect size (the group's overall mean effect size), but the range is much wider for Individual-based effect size estimates
#Why?
#   Individual model assigns all of the individual effect size to that individual
#   Group (multilevel) model estimates a mean group effect, individual differences from that mean, and noise. This means that the estimate of the individual effect size is informed by (a) a model of the performance of the group of which this individual is a member and (b) the fact that some of the observed individual difference is just random noise. This "shrinks" the individual effect size estimate toward the group mean in proportion to the model's cetainty about that mean and estimate of the noisiness of the data.

# 2. REPORTING GCA RESULTS
# General principle: provide enough information that another researcher would be able to replicate your analysis.
# (i) Model structure: clearly describe the functional form, all of the fixed effects (may want to include contrast coding for factors and scale for continuous predictors), and the random effects structure.
# (ii) Basis for the inferential statistics: which models were compared? how were parameter-specific p-values estimated?
# (iii) Complete model results, not just p-values: For model comparisons, report the change in log-likelihood and the degrees of freedom (i.e., the chi-squared test). For parameter estimates, report the estimates and their standard errors (the t-values are optional because they are just the estimates divided by standard errors).
# Example (WordLearnEx):
#   Growth curve analysis (Mirman, 2014) was used to analyze the
# learning of the novel words over the course of 10 training blocks.
# The overall learning curves were modeled with second-order orthogonal 
# polynomials and fixed effects of TP on all time terms. The low TP 
# condition was treated as the baseline and parameters were estimated 
# for the high TP condition. The model also included random effects of 
# participants on all time terms. The fixed effects of TP were added 
# individually and their effects on model fit were evaluated using 
# model comparisons. Improvements in model fit were evaluated using 
# -2 times the change in log-likelihood, which is distributed as chi-
# squared with degrees of freedom equal to the number of parameters 
# added. All analyses were carried out in R version 3.1 using the 
# lme4 package (version 1.1-6).
#   The effect of TP on the intercept did not improve model fit
# (chi^2(1) = 1.55; p = 0.213), nor did the effect of TP on the linear
# term (chi^2(1) = 0.358; p = 0.55). The effect of TP on the quadratic
# term, however, did improve model fit (chi^2(1) = 5.95; p = 0.0147),
# indicating that the low and high TP conditions differed in the rate
# of word learning. Table 1 shows the fixed effect parameter estimates 
# and their standard errors along with p-values estimated using the 
# normal approximation for the t-values.

##########################
# ANALYSIS TIME
# try GCA on your own data, ask for help if you need it
# if you don't have any appropriate data on hand, try one (or more) of these exercises:
#
# Exercise 6: individual differences in cohort and rhyme competition
# The CohortRhyme data set contains data from an eye-tracking experiment (Mirman et al., 2011) that investigated phonological competition between cohorts (e.g., penny - pencil) and rhymes (e.g., carrot - parrot). Three groups of participants were tested: 5 Broca's aphasics, three Wernicke's aphasics, and 12 control participants.
#  (a) Make a multi-panel plot showing each kind of competition for each group  
#  (b) Use fourth-order orthogonal polynomials to analyze (separately) the cohort and rhyme competition effects. Do the diagnosis groups differ in competition effect sizes? 
#  (c) Use random effects to compute individual competition effect sizes (note: you'll need to use a model without any group fixed effects to get random effects relative to the overall sample rather than the diagnosis sub-group). Test correlations between cohort and rhyme competition effects for patients and controls. Make a multi-panel scatterplot showing these correlations.
##########################