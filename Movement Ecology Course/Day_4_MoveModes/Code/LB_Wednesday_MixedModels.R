

###   Margam - refresher - Grouped data

##########################################################################################################

###   Learning aims:

# Understand what grouped data are

# Understand what to do and know various alternatives


##########################################################################################################

### SECTION 5.1.

library(nlme)

library(lattice)

data(Rail)

xyplot(Rail ~ travel, groups = Rail, data=Rail)


fm1Rail.lm <- lm( travel ~ 1, data = Rail )

summary(fm1Rail.lm)

Rail$residsLM1 <- resid(fm1Rail.lm)

plot(residsLM1 ~ factor(Rail), data = Rail)


# separate intercept for each rail?

fm2Rail.lm <- lm( travel ~ Rail - 1, data = Rail )

summary(fm2Rail.lm)

# compare the residual standard error to lm1
# repeat the boxplot of the residuals

# FYI, here a mixed model analysis

fm1Rail.lme <- lme(travel ~ 1, data = Rail, random = ~ 1 | Rail)

summary( fm1Rail.lme )

# compare estimated mean travel time to fm1 and the residual standard deviation to fm2

plot( fm1Rail.lme )

intervals( fm1Rail.lme )


# randomized block design - experimental factor + blocking factor

data(ergoStool)

summary(ergoStool)

plot.design( ergoStool )

with(ergoStool, xtabs(effort ~ Type + Subject))

xyplot(effort ~ Type|Subject, data = ergoStool)

fm1Stool <- lme(effort ~ Type, data = ergoStool, random = ~ 1 | Subject)

summary(fm1Stool)

fm2Stool <- lm(effort ~ Type + Subject, data = ergoStool)

# compare to the mixed model and discuss


# compare also to model without intercept

fm3Stool <- lme(effort ~ 0 + Type, data = ergoStool, random = ~ 1 | Subject)

anova(fm3Stool)

anova(fm1Stool)

# compare to aggregated mean and sd values per Type

with(ergoStool, tapply(effort, Type,mean))

with(ergoStool, tapply(effort, Type,sd))


####









