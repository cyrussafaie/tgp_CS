### Create sample data:
set.seed(11221)
df=dt_selected[,-1]
df$log_y=log(df$tgp_cs_ind_nonmda)
dim(df)[1]
index <- sample(seq_len(nrow(df)), size = round(dim(df)[1]*0.8,0))
train <- df[index, ]
### Intialize loop
y <- "log_y"
available.x <- colnames(train)[-43]
chosen.x <- NULL
r2 <- NULL
### Reset the best r^2 for each run
while (length(available.x) > 0) {
          best.r2 <- 0
          ### Iterate model based on previous best r^2 
          for (this.x in available.x) {
                    rhs <- paste(c(chosen.x, this.x), collapse=" + ")
                    f <- as.formula(paste(y, rhs, sep=" ~ "))
                    this.r2 <- summary(lm(f, data=train))$r.square
                    ### Compare r^2 for this iteration
                    if (this.r2 > best.r2) {
                              best.r2 <- this.r2
                              best.x <- this.x
                    }
          }
          ### Add variable with the best r^2 to previous runs
          chosen.x <- c(chosen.x, best.x)
          available.x <- available.x[available.x != best.x]
          r2 <- c(r2, best.r2)
}
### Add the emplty model
chosen.x <- c("(Intercept)", chosen.x)
r2 <- c(summary(lm(log_y ~ 1, data=train))$r.square, r2)
### Plot results
plot(r2, type="l");points(r2)








## Set initial parameters
result <- NULL
df <- cbind(log_y = df$log_y,
            df[,names(df)%in%chosen.x[-c(1,84)]])
head(df)

x <- paste(chosen.x[-c(1,84)], collapse = " + ")
f <- as.formula(paste(y,x, sep = " ~ "))
n <- floor(0.8* nrow(df))
## Iterate 1,000 times
for (i in 1:1000) {
          ## Create train & test
          set.seed(i)
          index <- sample(seq_len(nrow(df)), size = n)
          train <- df[index, ]
          test <- df[-index, ]
          ## Run model and extract coeficients, r^2 for training data and confidence interval
          model <- lm(f, train)
          summary <- summary(model)
          coef <- summary$coef[,1]
          r.train <- summary$r.squared
          conf.int <- t(confint(model))
          ## Calculate r^2 in holdout
          predicted <- predict(model, newdata=test)
          r.test <- cor(test$log_y,predicted)^2
          ## Combine all values and save them
          result[[paste0("model.",i)]] <- list(model=model,
                                               summary=summary,
                                               coef=coef,
                                               conf.int=conf.int,
                                               r.train = r.train,
                                               r.test = r.test)
}



## Extract coef and caluclate mean and sd
result.coef <- t(sapply(result,"[[","coef"))
coef.mean <- apply(result.coef,2,mean)
coef.sd <- apply(result.coef,2,sd)
## Prepare plot
par(mfrow=c(3,3))
for (i in 1:9) {
          ## Define chart variables
          beta <- result.coef[,i]
          mean <- coef.mean[i]
          sd <- coef.sd[i]
          d <- density(beta)
          name <- colnames(result.coef)[i]
          ## Define chart labels
          main <- name
          xlab <- paste0("mean = ",mean,", sd = ", sd)
          ## Plot historgram and density
          hist(beta, prob = TRUE, main = main, xlab = xlab)
          lines(d, col = "red",lwd = 2)
          ## Show mean
          abline(v=mean, col = "darkgreen", lwd = 2)
          ## Show sd
          arrows(mean-sd, mean(d$y),
                 mean+sd, mean(d$y),
                 code=3, length=.05, angle = 90, col = "royalblue", lwd = 2)
}

dev.off()

## Extract r^2 for model and caluclate mean and sd
result.r.train <- sapply(result,"[[","r.train")
r.train.mean <- round(mean(result.r.train),2)
r.train.sd <- round(sd(result.r.train),2)
d <- density(result.r.train)
## Prepare plot
### Define chart labels
main <- "Training r^2"
xlab <- paste0("mean = ",r.train.mean,", sd = ", r.train.sd)
### Plot historgram and density
hist(result.r.train, prob = TRUE, main = main, xlab = xlab)
lines(d, col = "red",lwd = 2)
### Show mean
abline(v=r.train.mean, col = "darkgreen", lwd = 2)
### Show sd
arrows(r.train.mean-r.train.sd, mean(d$y),
       r.train.mean+r.train.sd, mean(d$y),
       code=3, length=.05, angle = 90, col = "royalblue", lwd = 2)


# Extract r^2 for holdout and caluclate mean and sd
result.r.test <- sapply(result,"[[","r.test")
r.test.mean <- round(mean(result.r.test),2)
r.test.sd <- round(sd(result.r.test),2)
d <- density(result.r.test)
## Prepare plot
### Define chart labels
main <- "Holdout r^2"
xlab <- paste0("mean = ",r.test.mean,", sd = ", r.test.sd)
### Plot historgram and density
hist(result.r.test, prob = TRUE, main = main, xlab = xlab)
lines(d, col = "red",lwd = 2)
### Show mean
abline(v=r.test.mean, col = "darkgreen", lwd = 2)
### Show sd
arrows(r.test.mean-r.test.sd, mean(d$y),
       r.test.mean+r.test.sd, mean(d$y),
       code=3, length=.05, angle = 90, col = "royalblue", lwd = 2)



## Collect change r^2 from training to test and calcualte mean and sd
change <-
          abs(result.r.test-result.r.train)/result.r.train
cange.mean <- round(mean(change),2)
chang.sd <- round(sd(change),2)
d <- density(change)
## Prepare plot
### Define chart labels
main <- "% Change in r^2"
xlab <- paste0("mean = ",cange.mean,", sd = ", chang.sd)
### Plot historgram and density
hist(change, prob = TRUE, main = main, xlab = xlab)
lines(d, col = "red",lwd = 2)
### Show mean
abline(v=cange.mean, col = "darkgreen", lwd = 2)
### Show sd
arrows(cange.mean-chang.sd, mean(d$y),
       cange.mean+chang.sd, mean(d$y),
       code=3, length=.05, angle = 90, col = "royalblue", lwd = 2)


# Compare training results to model with full sample

model <- lm(f, df)
summary <- summary(model)
coef <- round(summary$coef[,1],2)
diff <- coef.mean-coef
rbind(full.sample = coef, simulated.mean = coef.mean, diff = diff, pcent.diff =
                round(diff/coef,2))

## Scale CI down
x <- 1
## Extract CI from sampling result
beta.ci <- t(sapply(result,"[[","conf.int"))*x
## Calculate interval length
beta.ci.length <- matrix(nrow = 1000, ncol=4)
for (i in c(2,4,6,8)) {
          beta.ci.length[,i-i/2] <- round(beta.ci[,i]-beta.ci[,i-1],2)
}
## Extract CI from full sample result
conf.int <- confint(model)
conf.int.length <-round(conf.int[,2]-conf.int[,1],2)
## Compare CIs
train.ci.mean <- round(apply(beta.ci.length,2,mean),2)
full.ci <- conf.int.length
diff <- train.ci.mean - full.ci
pcent.diff = round(diff/full.ci,2)
rbind(full.ci, train.ci.mean, diff, pcent.diff)


