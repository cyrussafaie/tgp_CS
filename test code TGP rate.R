data1=read.csv("table_export2_20170420.csv")
###removing some divisions becasue of clusure or recent aquisition
data1=subset(data1,!(data1$DIV_NM %in% c('BALTIMORE','CHARLOTTE','LAKELAND','WAUKESHA')))

# exclude Iowa number for 2013 P01 and 2013 P02




names(data1)


dim(data1)

unique(data1$DIV_NM)

tgp=as.numeric(data1$TGP_PER_CS)

library(car)
library(MASS)
#testing with normal. tails seem to out of confidence
qqp(tgp, "norm")

qqp(tgp, "lnorm")
qqp(tgp, "chisq",df=6)

#test for multimodality
library(diptest)
dip.test(tgp)

# let's try Gaussian mixture model
library(mclust)
x.gmm = Mclust(tgp)
summary(x.gmm)
x.gmm2 = Mclust(tgp,G=2)
summary(x.gmm2)

data2=cbind(class2=x.gmm2$classification,class3=x.gmm$classification,data1)
#write.csv(data2,"ClassesSample.csv")
# now let's test if unimodality makes sense
x.gmm.1 = Mclust(x, G=1)
logLik(x.gmm.1)
# 'log Lik.' -1226.241 (df=2)
logLik(x.gmm)-logLik(x.gmm.1)
# 'log Lik.' 25.36657 (df=5)
1-pchisq(25.36657, df=3)  # [1] 1.294187e-05




plot(density(log(tgp)))
plot(tgp)
which.min(tgp)
data1[1531,]

skewness(words1)
kurtosis(words1)

skewness(words2)
kurtosis(words2)
library(moments)
skewness(log(tgp))
kurtosis(log(tgp))
shapiro.test((tgp))
qqnorm(log(tgp));qqline(log(tgp), col = 2)

## Generate two data sets
## First Normal, second from a t-distribution
words1 = rnorm(1000); 
words2 = rt(1000, df=3)

## Have a look at the densities
plot(density(words1));

plot(density(words2))

#shapiro null hypothesis:
# that the population is normally distributed. Thus, if the p-value is less than the chosen alpha level, then the null hypothesis is rejected 

## Perform the test
shapiro.test(words1);
shapiro.test(words2)

## Plot using a qqplot
qqnorm(words1);qqline(words1, col = 2)
qqnorm(words2);qqline(words2, col = 2)


# library(ggplot2)
# ggplot(data1, aes(x =FISC_YR_MTH , y = QUANTITY_SHIPPED_SUM , colour = DIV_NM)) +
#           geom_point() + 
#           facet_wrap( ~ DIV_NM)

# library(lattice)
# xyplot(QUANTITY_SHIPPED_SUM ~ FISC_YR_MTH | DIV_NM, data = data1, 
#        auto.key = list(corner = c(0, .98)), cex = 1.5)



# Kurtosis custome code

kurtosis.test <- function (x) {
          m4 <- sum((x-mean(x))^4)/length(x)
          s4 <- var(x)^2
          kurt <- (m4/s4) - 3
          sek <- sqrt(24/length(x))
          totest <- kurt/sek
          pvalue <- pt(totest,(length(x)-1))
          pvalue 
}
kurtosis.test(words1)


skew.test <- function (x) {
          m3 <- sum((x-mean(x))^3)/length(x)
          s3 <- sqrt(var(x))^3
          skew <- m3/s3
          ses <- sqrt(6/length(x))
          totest <- skew/ses
          pt(totest,(length(x)-1))
          pval <- pt(totest,(length(x)-1))
          pval
}