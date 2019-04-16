library('MASS')

ChickWeight
# show the groups
levels(dataset$Diet)


plot(weight ~ Diet, data=ChickWeight)

library(dplyr)
group_by(dataset, Diet) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

boxplot(dataset$weight ~ dataset$Diet)

# Compute the analysis of variance
res.aov <- aov(weight ~ Diet, data = dataset)
# Summary of the analysis
summary(res.aov)



# The null hypothesis is the mean of four groups are same, we can see from the outcome
# of anova, the p value is very small which means we can reject the null hypothesis
# and have the conclusion that at least one pair of means are not same.

for (i in 1:nrow(ChickWeight)){
  for (j in 1:50){
    if (ChickWeight$Chick[i] == j){
      ChickWeight$firstweight[i] = ChickWeight[ChickWeight$Time == 0,'weight'][j]
    }
  }
}


res.aov2 <- aov(weight ~ Diet + firstweight, data = ChickWeight)
summary(res.aov2)

install.packages('lsmeans')
library('lsmeans')
lm1 <- lm(weight ~ Diet + firstweight, data = ChickWeight)
res.aov2 <-anova(lm1)
weight.rg1 <- ref.grid(lm1)
lsmeans(weight.rg1,'Diet')

# we can see the lsmean of first diet group is 104.12, for the second group is 121.27, 
# for the third group is 141.95 and for the forth group is 134.89. We can conclude
# that these four mean of four groups are different. 

#2)
pairwise.t.test(ChickWeight$weight,ChickWeight$Diet,p.adjust.method = 'bonferroni')

# Using bonferroni test, diet 1 group and diet 3 group comparison and diet 1 group and 
# diet 4 group comparison are significant.but that there is insufficient statistical support
# to distinguish between the diet 2 group and diet 3 group, diet 1 group and diet 2 group,
# diet 2 group and diet 4 group or diet 3 group and diet 4 group.

TukeyHSD(res.aov)
# we can see for the p-value that diet 1 group and diet 3 group comparison and diet 1 group and 
# diet 4 group comparison are significant, while others are not significant.

#3)
kruskal.test(weight~Diet, ChickWeight)
# As the p-value is less than the significance level 0.05, we can conclude that there are significant
# differences between the Diet groups.

#4)
hist(resid(res.aov))
qqnorm(resid(res.aov))
# violate the normality assumption. We can use Box-Cox transformations.

bartlett.test(ChickWeight$weight,ChickWeight$Diet)

# From the output we can see that the p-value of 1.768e-06 
# is less than the significance level of 0.05. This means we can reject 
# the null hypothesis that the variance is the same for all treatment groups. This means
# this dataset violate the same variance assumption

library(car)
# Levene's test with one independent variable
leveneTest(weight ~ Diet, data = ChickWeight)
# From the output we can see that the p-value of 3.418e-06
# is less than the significance level of 0.05. This means we can reject 
# the null hypothesis that the variance is the same for all treatment groups. This means
# this dataset violate the same variance assumption
install.packages('biotools')
library('biotools')
MBoxtest(ChickWeight$weight,ChickWeight$Diet)

#2>
hist(resid(res.aov2))
qqnorm(resid(res.aov2))  
