library(datasets)
library(ggplot2)
install.packages(e1071)
library(e1071)
data("ToothGrowth")
str(ToothGrowth)
ToothGrowth
by(ToothGrowth$len,INDICES = list(ToothGrowth$supp),summary)
library(e1071)
kurtosis(ToothGrowth$len)
by(ToothGrowth$len,INDICES = list(ToothGrowth$supp),kurtosis)
by(ToothGrowth$len,INDICES = list(ToothGrowth$supp),median)
by(ToothGrowth$len,INDICES = list(ToothGrowth$supp),IQR)
kurtosis(VC)
VC = ToothGrowth[ToothGrowth[,'supp'] == 'VC','len']
OJ = ToothGrowth[ToothGrowth[,'supp'] == 'OJ','len']
boot_kurtosis_vc <- numeric(1000)
boot_kurtosis_oj <- numeric(1000)
x_vc = length(VC)
x_oj = length(OJ)
for (i in 1:1000) boot_kurtosis_vc[i] <- kurtosis(sample(x_vc,replace = T))
for (i in 1:1000) boot_kurtosis_oj[i] <- kurtosis(sample(x_oj,replace = T))

kurtosis
mean(boot_kurtosis_vc)
bias_kurtosis_vc <- mean(boot_kurtosis_vc) - kurtosis(VC)
bias_kurtosis_vc
variance_kurtosis_vc = mean((boot_kurtosis_vc - mean(boot_kurtosis_vc))**2)
variance_kurtosis_vc

bias_kurtosis_oj <- mean(boot_kurtosis_oj) - kurtosis(OJ)
bias_kurtosis_oj
variance_kurtosis_oj = mean((boot_kurtosis_oj - mean(boot_kurtosis_oj))**2)
variance_kurtosis_oj

install.packages("boot")
install.packages('bootstrap')
library(bootstrap)

results <- jackknife(VC,kurtosis)
mean(results$jack.values)
bias = mean(results$jack.values) - kurtosis(VC)

t.test(VC,OJ)

wilcox.test(VC,OJ)

boot.stat_median = boot_median_vc - boot_median_oj
quantile(boot.stat_median,c(0.05,0.95))



boot_variance_vc <- numeric(1000)
boot_variance_oj <- numeric(1000)
x_vc = length(VC)
x_oj = length(OJ)
for (i in 1:1000) boot_variance_vc[i] <- var(sample(x_vc,replace = T))
for (i in 1:1000) boot_variance_oj[i] <- var(sample(x_oj,replace = T))

boot.stat_variance = boot_variance_vc - boot_variance_oj
quantile(boot.stat_variance,c(0.05,0.95))

for (i in 1:nrow(ToothGrowth)){
 if (ToothGrowth[i,'len'] >20){
   ToothGrowth[i,'level'] = 'high'
   
 }
  else {ToothGrowth[i,'level'] = 'low'}
}

VC_level = ToothGrowth[ToothGrowth[,'supp'] == 'VC','level']
OJ_level = ToothGrowth[ToothGrowth[,'supp'] == 'OJ','level']


num_vc = 0
num_oj = 0
for (i in 1:length(VC_level)){
  if (VC_level[i] == "high"){
    num_vc = num_vc + 1
  }
}

for (i in 1:length(OJ_level)){
  if (OJ_level[i] == "high"){
    num_oj = num_oj + 1
  }
}

res <- prop.test(x = c(num_vc,num_oj), n = c(length(VC_level),length(OJ_level)))
res

num_low_0.5=0
num_low_1=0
num_low_2=0
num_high_0.5=0
num_high_1=0
num_high_2=0
for (i in 1:nrow(ToothGrowth)){
  if (ToothGrowth$level[i] == 'low'){
    if (ToothGrowth$dose[i] == 0.5){
      num_low_0.5= num_low_0.5 + 1
    }
     if (ToothGrowth$dose[i] == 1.0) {
       num_low_1 = num_low_1 +1
     }
    if (ToothGrowth$dose[i] == 2.0) {
      num_low_2 = num_low_2 + 1
    }
  }
  else{
    if (ToothGrowth$dose[i] == 0.5){
      num_high_0.5 = num_high_0.5 + 1
    }
    if (ToothGrowth$dose[i] == 1.0){
      num_high_1 = num_high_1 + 1
    }
    if (ToothGrowth$dose[i] == 2.0){
      num_high_2 = num_high_2 + 1
    }
  }
}

dose_level <- matrix(c(num_high_0.5,num_low_0.5,num_high_1,num_low_1,num_high_2,num_low_2), 3,2,byrow=TRUE)
colnames(dose_level) = c('High','Low')
rownames(dose_level) = c('dose_0.5','dose_1.0','dose_2.0')
fisher.test(dose_level)
# reject the null hypothesis, means the proportation of two groups are different.
