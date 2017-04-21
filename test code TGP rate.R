data1=read.csv("table_export2_20170420.csv")

dim(data1)
head(data1)
unique(data1$DIV_NM)

###removing some divisions



tgp=as.numeric(data1$TGP_PER_CS)

plot(density(tgp))
plot(tgp)
which.max(tgp)
data1[339,]

skewness(words1)
kurtosis(words1)

skewness(words2)
kurtosis(words2)
library(moments)
skewness(tgp)
kurtosis(tgp)-3
shapiro.test(tgp)

## Generate two data sets
## First Normal, second from a t-distribution
words1 = rnorm(100000000); 
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