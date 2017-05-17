dt=read.csv("full_nonmda_ind20170502_complete - original.csv")
dim(dt)
names(dt)
str(dt,list.len = 999)
library(ggplot2)


#exclude salad and specialty meat
# exclude potentially customer type B and C
# exclude price source 1 and 4
#lost and new count seems a bit wierd may need to rerun and the counts
#tail(dt)

###################
###################
##Variable Extraction
###################
###################
#fixing 52 weeks issue of the 

for (i in c(14:22,27:63,88:89)){
dt[,i]=ifelse(dt$FISC_YR_MTH=='201512',round(dt[,i]*4/5,0),dt[,i])
}

dt$tgp_cs_ind_nonmda=round(dt$TGP_IND_NONMDA/dt$QTY_IND_NONMDA,4)#output

dt$ind_share=round(dt$QTY_IND/dt$QTY_TTL,4)#ind share of total DC volume sold

dt$sell_prc_ind_nonmda= round(dt$SALES_IND_NONMDA/dt$QTY_IND_NONMDA,4) #Avergae Sell Price
dt$tgp_per_drop=round(dt$TGP_IND_NONMDA_RT_TYPE/dt$DROP_CNT_IND_NONMDA,4) # TGP per drop: probably too obvious to use
dt$sales_per_drop=round(dt$SALES_IND_NONMDA/dt$DROP_CNT_IND_NONMDA,0) #sales per drop 
dt$ave_customer_size=round(dt$SALES_IND_NONMDA/dt$CUSTOMER_CNT_IND_NONMDA,0) #average customer sales size

dt$ind_nonmda_share=100*round(dt$QTY_IND_NONMDA/dt$QTY_IND,4) #mda share of IND

#dt$indnonmdalocal_share=dt$QTY_IND_NONMDA_LOCAL/dt$QTY_IND_NONMDA
dt$ind_nonmda_eb_share=100*round(dt$QTY_EB_IND_NONMDA/dt$QTY_IND_NONMDA,4) # EB voume share
dt$ind_nonmda_packer_share=100*round(dt$QTY_PACKER_IND_NONMDA/(dt$QTY_IND_NONMDA-dt$QTY_EB_IND_NONMDA),4) #packer share of MB
#shares  doesn't seem reasonable due to dependencies: Grouping to COP, Grocery and dry?,
#grouping the volumes
#COP:poultry,beef,pork,seafood,specialty meat
dt$poultry_share=100*round(dt$QTY_POULTRY_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$pork_share=100*round(dt$QTY_PORK_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$seafood_share=100*round(dt$QTY_SEAFOOD_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$beef_share=100*round(dt$QTY_BEEF_IND_NONMDA/dt$QTY_IND_NONMDA,4)
# 
dt$canned_share=100*round(dt$QTY_CANFRUITVEG_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$groceryfrozen_share=100*round(dt$QTY_GROCERYFROZEN_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$chemical_share=100*round(dt$QTY_CHEMICAL_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$dairy_share=100*round(dt$QTY_DAIRY_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$oil_share=100*round(dt$QTY_OIL_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$processedmeat_share=100*round(dt$QTY_PROCESSEDMEAT_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$disposable_share=100*round(dt$QTY_DISPOSABLE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$beverage_share=100*round(dt$QTY_BEVERAGE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$apperizer_share=100*round(dt$QTY_APPETIZER_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$cheese_share=100*round(dt$QTY_CHEESE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$produce_share=100*round(dt$QTY_PRODUCE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
# 
dt$american_share=100*round(dt$QTY_AMERICANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$classic_share=100*round(dt$QTY_CLASSICMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$italian_share=100*round(dt$QTY_ITALYPIT_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$mexican_share=100*round(dt$QTY_MEXIICANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$bar_share=100*round(dt$QTY_BARMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$deli_share=100*round(dt$QTY_DELIMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$steak_share=100*round(dt$QTY_STEAKMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$otherasian_share=100*round(dt$QTY_OTHASIANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$cop=dt$QTY_POULTRY_IND_NONMDA+dt$QTY_BEEF_IND_NONMDA+dt$QTY_SEAFOOD_IND_NONMDA+dt$QTY_PORK_IND_NONMDA+dt$QTY_SPECIALTYMEAT_IND_NONMDA+dt$QTY_PROCESSEDMEAT_IND_NONMDA
dt$cop_share=100*(dt$QTY_POULTRY_IND_NONMDA+dt$QTY_BEEF_IND_NONMDA+dt$QTY_SEAFOOD_IND_NONMDA+dt$QTY_PORK_IND_NONMDA+dt$QTY_SPECIALTYMEAT_IND_NONMDA+dt$QTY_PROCESSEDMEAT_IND_NONMDA)/dt$QTY_IND_NONMDA
          
dt$typeA_share=100*round(dt$QTY_CUSTA_IND_NONMDA/dt$QTY_IND_NONMDA,4) #type A colume share
#dt$typeC_share=round(dt$QTY_CUSTC_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$prime_share=100*round(dt$QTY_PRIME_IND_NONMDA/dt$QTY_IND_NONMDA,4) #prime share of IND non mda

dt$new_share=100*round(dt$QTY_CUSTNEW_IND_NONMDA/dt$QTY_IND_NONMDA,4) #new customer Volume share
#dt$prior_share=dt$QTY_CUSTPRIOR_IND_NONMDA/dt$QTY_IND_NONMDA

dt$account_per_tm=dt$CUSTOMER_CNT_IND_NONMDA/dt$TM_CNT_IND_NONMDA
dt$sales_per_tm= dt$SALES_IND_NONMDA/dt$TM_CNT_IND_NONMDA

dt$LIC_per_CS=round(dt$LIC_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$TMC_per_CS=round(dt$TMC_IND_NONMDA/dt$QTY_IND_NONMDA,4)

#trend line for tgp rate
dt$trend=rep(1:51,56)
###################
###################
##unit of measures changes to what we intend to interpret
###################
###################
dt$EcomPenetration=100*dt$EcomPenetration
dt$Correct_Invoices=100*dt$Correct_Invoices
dt$Complete_Orders=100*dt$Complete_Orders
dt$DamageFree_Orders=100*dt$DamageFree_Orders
dt$OnTime_Orders=100*dt$OnTime_Orders
dt$POI=100*dt$POI
dt$YOYgrowth=100*dt$YOYgrowth
dt$priceIndex=100*dt$priceIndex
dt$CustomMarketIndexCMI=100*dt$CustomMarketIndexCMI
dt$USFMarketShareStatic=100*dt$USFMarketShareStatic
dt$Churn_true=100*dt$Churn_true
dt$Investment.Spend.CS.participation=100*dt$Investment.Spend.CS.participation
dt$CBA_share=100*dt$CBA_share
dt$DonD._Share=100*dt$DonD._Share
dt$Fixed_sell_share=100*dt$Fixed_sell_share
dt$Price_approval_share=100*dt$Price_approval_share
dt$ind_share=100*dt$ind_share



names(dt)
str(dt,list.len = 999)
#a=dt[1:2,]
#write.csv(a,"Variables.csv")
summary(dt)
dim(dt)
#Excluding 2013 and 2017P03 for missing data
dt=subset(dt,dt$FISC_YR!='2013')
dt=subset(dt,dt$FISC_YR_MTH!='201703')

#Iowa 201401 and 201402 excluded due to missing data and skewed rate
#JAckson closed period ecluded 201405 to 201506
dt=subset(dt,!(dt$divPEriod %in% c('IOWA201401','IOWA201402','JACKSON201405','JACKSON201406','JACKSON201407'
                                   ,'JACKSON201408','JACKSON201409','JACKSON201410','JACKSON201411','JACKSON201412',
                                   'JACKSON201501','JACKSON201502','JACKSON201503','JACKSON201504',
                                   'JACKSON201505','JACKSON201506')))

#outlier identification for output: yet to decide whether want to include
#source("outlier_tukey.R")
dt$tgp_cs_ind_nonmda2=dt$tgp_cs_ind_nonmda
#outlierKD(dt,tgp_cs_ind_nonmda2)
dim(dt)
#length(as.data.frame(subset(dt,is.na(dt$tgp_cs_ind_nonmda2)))$divPEriod)
outliers=c('WEST VIRGINIA201508','WEST VIRGINIA201510','WEST VIRGINIA201601','WEST VIRGINIA201603'          
           ,'WEST VIRGINIA201605','NORTH DAKOTA - BISMARCK201512','NORTH DAKOTA - BISMARCK201610','NORTH DAKOTA - BISMARCK201612'
           ,'LITTLE ROCK201603','LITTLE ROCK201604','MEMPHIS201404','','MEMPHIS201702','LOS ANGELES201401','LOS ANGELES201402'
           ,'LOS ANGELES201501','LOS ANGELES201508','LOS ANGELES201509','LOS ANGELES201510','LOS ANGELES201511','LOS ANGELES201512'            
           ,'LOS ANGELES201601','LOS ANGELES201602','LOS ANGELES201603','LOS ANGELES201604','LOS ANGELES201605','LOS ANGELES201606'
           ,'LOS ANGELES201607','LOS ANGELES201608','LOS ANGELES201609','LOS ANGELES201610','LOS ANGELES201611','LOS ANGELES201612'
           ,'LOS ANGELES201701','LOS ANGELES201702')  


#as.data.frame(subset(dt,is.na(dt$TM_Tenure_mnth)))$divPEriod

#changin seattle ranking back to 1 as original data suggested
dt$USFRank_BCG[dt$DIV_NM=='SEATTLE']<-1

#####
#####all na's handled and outlier flag generated
#####

#followign script to impute tm and custoemr tenure with future values

for (j in 1:3) {
          for (i in 1:(length(dt$TM_Tenure_mnth)-1)) if(is.na(dt$TM_Tenure_mnth[i])) dt$TM_Tenure_mnth[i]<-dt$TM_Tenure_mnth[i+1] 
          for (i in 1:(length(dt$Cust_Tenure_mnth)-1)) if(is.na(dt$Cust_Tenure_mnth[i])) dt$Cust_Tenure_mnth[i]<-dt$Cust_Tenure_mnth[i+1] 
          for (i in 1:(length(dt$nps)-1)) if(is.na(dt$nps[i])) dt$nps[i]<-dt$nps[i+1] 
}
summary(dt)
names(dt)
dim(dt)

#saveRDS(dt,"data_full_20170514.rds")
#dt=readRDS("data_full_20170514.rds")

#write.csv(dt,"data_all_20170514.csv",row.names = F)
#write.csv(dt[1:2,],"var_20170514.csv")

#houston seems to have high price with low tgp/cs

########################################
########################################
########################################
# candidate variables to include in the model (despite correlation)
########################################
########################################
########################################
#write.csv(dt[1:3,],"variable_list.csv")
dim(dt)
dt_selected=dt[,c('divPEriod',
                  'FISC_YR_MTH',
                  'FISC_YR',
                  'Quarter_number',
                  'CUSTOMER_CNT_IND_NONMDA',
                  'TM_CNT_IND_NONMDA',
                  'QTY_POULTRY_IND_NONMDA',
                  'QTY_PORK_IND_NONMDA',
                  'QTY_CANFRUITVEG_IND_NONMDA',
                  'QTY_GROCERYFROZEN_IND_NONMDA',
                  'QTY_CHEMICAL_IND_NONMDA',
                  'QTY_DAIRY_IND_NONMDA',
                  'QTY_SALAD_IND_NONMDA',
                  'QTY_OIL_IND_NONMDA',
                  'QTY_BEEF_IND_NONMDA',
                  'QTY_SPECIALTYMEAT_IND_NONMDA',
                  'QTY_PROCESSEDMEAT_IND_NONMDA',
                  'QTY_DISPOSABLE_IND_NONMDA',
                  'QTY_SEAFOOD_IND_NONMDA',
                  'QTY_BEVERAGE_IND_NONMDA',
                  'QTY_APPETIZER_IND_NONMDA',
                  'QTY_CHEESE_IND_NONMDA',
                  'QTY_PRODUCE_IND_NONMDA',
                  'EcomPenetration',
                  'Correct_Invoices',
                  'Complete_Orders',
                  'DamageFree_Orders',
                  'OnTime_Orders',
                  'POI',
                  'YOYgrowth',
                  'priceIndex',
                  'nps',
                  'Organized',
                  'USFRank_BCG',
                  'CustomMarketIndexCMI',
                  'USFMarketShareStatic',
                  'Cust_Tenure_mnth',
                  'TM_Tenure_mnth',
                  'Churn_true',
                  'Investment.Spend.CS.participation',
                  'Investment.PerCS',
                  'CBA_share',
                  'DonD._Share',
                  'Fixed_sell_share',
                  'Price_approval_share',
                  'tgp_cs_ind_nonmda',
                  'ind_share',
                  'sell_prc_ind_nonmda',
                  'tgp_per_drop',
                  'sales_per_drop',
                  'ave_customer_size',
                  'ind_nonmda_share',
                  'ind_nonmda_eb_share',
                  'ind_nonmda_packer_share',
                  'poultry_share',
                  'pork_share',
                  'seafood_share',
                  'beef_share',
                  'canned_share',
                  'groceryfrozen_share',
                  'chemical_share',
                  'dairy_share',
                  'oil_share',
                  'processedmeat_share',
                  'disposable_share',
                  'beverage_share',
                  'apperizer_share',
                  'cheese_share',
                  'produce_share',
                  'american_share',
                  'classic_share',
                  'italian_share',
                  'mexican_share',
                  'bar_share',
                  'deli_share',
                  'steak_share',
                  'otherasian_share',
                  'cop_share',
                  'typeA_share',
                  'prime_share',
                  'new_share',
                  'account_per_tm',
                  'sales_per_tm',
                  'LIC_per_CS',
                  'TMC_per_CS',
                  'trend'
)]
dt_selected$Quarter_number=factor(dt_selected$Quarter_number)

########################################
########################################
########################################
#train-test split for 
########################################
########################################
########################################

# 1.initial split by time
test_ind_16q4after <- which(dt_selected$FISC_YR == 2017 | dt_selected$FISC_YR_MTH %in% c(201612,201611,201610))
test1<- dt_selected[test_ind_16q4after, ]
train1= dt_selected[-test_ind_16q4after, ]

# 2.random split
set.seed(60134)
test_random <-sample(seq_along(dt_selected$divPEriod),dim(dt_selected)[1]*0.2) 
test2=dt_selected[test_random,]
train2=dt_selected[-test_random,]

########################################
########################################
########################################
# variables to use
########################################
########################################
########################################
#write.csv(dt[1:3,],"variable_list.csv")

dim(dt)
covar_select=c(
          #'divPEriod',
                  # 'FISC_YR_MTH',
                  # 'FISC_YR',
                  #'Quarter_number',
                  # 'CUSTOMER_CNT_IND_NONMDA',
                  # 'TM_CNT_IND_NONMDA',
                  # 'QTY_POULTRY_IND_NONMDA',
                  # 'QTY_PORK_IND_NONMDA',
                  # 'QTY_CANFRUITVEG_IND_NONMDA',
                  # 'QTY_GROCERYFROZEN_IND_NONMDA',
                  # 'QTY_CHEMICAL_IND_NONMDA',
                  # 'QTY_DAIRY_IND_NONMDA',
                  # 'QTY_SALAD_IND_NONMDA',
                  # 'QTY_OIL_IND_NONMDA',
                  # 'QTY_BEEF_IND_NONMDA',
                  # 'QTY_SPECIALTYMEAT_IND_NONMDA',
                  # 'QTY_PROCESSEDMEAT_IND_NONMDA',
                  # 'QTY_DISPOSABLE_IND_NONMDA',
                  # 'QTY_SEAFOOD_IND_NONMDA',
                  # 'QTY_BEVERAGE_IND_NONMDA',
                  # 'QTY_APPETIZER_IND_NONMDA',
                  # 'QTY_CHEESE_IND_NONMDA',
                  # 'QTY_PRODUCE_IND_NONMDA',
                  'EcomPenetration',
                   # 'Correct_Invoices',
                   # 'Complete_Orders',
                   # 'DamageFree_Orders',
                   # 'OnTime_Orders',
                  'POI',
                  'YOYgrowth',
                  'priceIndex',
                  'nps',
                  'Organized',
                  #'USFRank_BCG',
                  'CustomMarketIndexCMI',
                  'USFMarketShareStatic',
                  'Cust_Tenure_mnth',
                  'TM_Tenure_mnth',
                  #'Churn_true',
                  'Investment.Spend.CS.participation',
                  #'Investment.PerCS',
                  #'CBA_share',
                  #'DonD._Share',
                  #'Fixed_sell_share',
                  'Price_approval_share',
                  #'tgp_cs_ind_nonmda',
                  #'ind_share',
                  #'sell_prc_ind_nonmda',
                  #'tgp_per_drop',
                  'sales_per_drop',
                  'ave_customer_size',
                  'ind_nonmda_share',
                  'ind_nonmda_eb_share',
                  'ind_nonmda_packer_share',
                  'poultry_share',
                  # 'pork_share',
                  # 'seafood_share',
                  # 'beef_share',
                  # 'canned_share',
                  # 'groceryfrozen_share',
                  # 'chemical_share',
                  # 'dairy_share',
                  # 'oil_share',
                  # 'processedmeat_share',
                  # 'disposable_share',
                  # 'beverage_share',
                  # 'apperizer_share',
                  # 'cheese_share',
                  # 'produce_share',
                  # 'american_share',
                  # 'classic_share',
                  # 'italian_share',
                  # 'mexican_share',
                  # 'bar_share',
                  # 'deli_share',
                  # 'steak_share',
                  # 'otherasian_share',
                  #'cop_share',
                  'typeA_share',
                  'prime_share',
                  #'new_share',
                  #'account_per_tm',
                  #'sales_per_tm',
                  'LIC_per_CS',
                  #'TMC_per_CS',
                  'trend'
)
dim(dt_selected)
f <- as.formula(paste('log(tgp_cs_ind_nonmda)~', paste(covar_select, collapse='+')))
model.Base=lm(f,data = dt_selected[-c(39:41,709),])
#summary(model.Base)
step.model=step(model.Base,trace = 0, direction = "both")
summary(step.model)

moddd=update(step.model,.~.  - poultry_share + cop_share + typeA_share -Churn_true + Quarter_number + new_share +ind_share )
summary(moddd)
exp(confint(moddd))-1
car::vif(moddd)
par(mfrow=c(2,2))
plot(moddd)

#summary(lm(log(tgp_cs_ind_nonmda)~factor(Quarter_number), data = dt_selected[-c(39:41,709),]))

# table(factor(dt_selected$Quarter_number),dt_selected$Quarter_number)
# mean(subset(dt_selected$tgp_cs_ind_nonmda,dt_selected$Quarter_number==1))
# mean(subset(dt_selected$tgp_cs_ind_nonmda,dt_selected$Quarter_number==2))
# mean(subset(dt_selected$tgp_cs_ind_nonmda,dt_selected$Quarter_number==3))
# mean(subset(dt_selected$tgp_cs_ind_nonmda,dt_selected$Quarter_number==4))
# exp(1.572e+00)
# exp(1.572e+00)*exp(.1032614)
# exp(1.572e+00)*exp(8.152e-02)
# exp(1.572e+00)*exp(7.346e-02)
#lev = hat(model.matrix(moddd))
# res=moddd$residuals
# which.max(res)
# which(lev>0.03)
step.model2=step(moddd,trace = 0, direction = "both")

summary(step.model2)
plot(step.model2)
car::vif(step.model2)
par(mfrow=c(2,2))
plot(step.model2)
dev.off()


#LIC has slightly high vif ~12
drop1(step.model2,test = 'Chisq')
varss=variable.names(step.model2)[-c(1,20:22)]
varss=c(varss,"Quarter_number","tgp_cs_ind_nonmda")
coef(step.model2)
confint(step.model2)
#saveRDS(step.model2,"model1_fulldata.rda")


dev.off()
########################################
########################################
########################################
#lasso and ridge
########################################
########################################
########################################

library(glmnet)

X<-model.matrix(log(tgp_cs_ind_nonmda)~., data=dt_selected[-c(39:41,709),varss])[,-1]
y=log(dt_selected$tgp_cs_ind_nonmda[-c(39:41,709)])


#glmnet by default standardize the variables
#model fit on the training set


#lasso :no variable reduction
set.seed(60134)
lasso.mod.train<-glmnet(X,y, alpha=1)
cv.lasso.mod.train<-cv.glmnet(X,y, alpha=1, nfold=10)
plot(cv.lasso.mod.train)
names(cv.lasso.mod.train)
cv.lasso.mod.train$lambda.min
#prediction.glmnet=predict(cv.lasso.mod.train,s=0.0001411853,as.matrix(traingSet[-1]),type="response")
#hist(prediction.glmnet)
set.seed(60134)
lasso.mod.train$beta
max(lasso.mod.train$dev.ratio) #this is similar to R squared

coef(lasso.mod.train,s=0.0001286428) #coef for lasso at optimum

#ridge
set.seed(60134)
ridge.mod<-glmnet(X,y, alpha=0)
cv.ridge.mod<-cv.glmnet(X,y, alpha=0, nfold=10)
cv.ridge.mod$lambda.min
set.seed(60134)
max(ridge.mod$dev.ratio) #this is similar to R squared
coef(ridge.mod,s=0.007192172) #coef for lasso at optimum

#elastic net
set.seed(60134)
elastinet.mod<-glmnet(X,y, alpha=0.5)
cv.elastinet.mod<-cv.glmnet(X,y, alpha=0.5, nfold=10)
cv.elastinet.mod$lambda.min
set.seed(60134)
max(elastinet.mod$dev.ratio) #this is similar to R squared
coef(elastinet.mod,s=0.0001341489) #coef for lasso at optimum



lasso.model<-glmnet(X,y,lambda =0.0001286428,  alpha=1)
ridge.model<-glmnet(X,y,lambda =0.007192172,  alpha=0)
elasticnet.model<-glmnet(X,y, lambda = 0.0001341489, alpha=0.5)
 # saveRDS(lasso.model,"lasso.rds")
 # saveRDS(ridge.model,"ridge.rds")
 # saveRDS(elasticnet.model,"e_net.rds")
# 

########################################
########################################
########################################
# now multiple train and test to get coefs and range
########################################
########################################
########################################

### Create sample data:
set.seed(60134)
df=dt_selected[,varss]
df$log_y=log(df$tgp_cs_ind_nonmda)
names(df)
df=df[,-22]
dim(df)[1]
index <- sample(seq_len(nrow(df)), size = round(dim(df)[1]*0.8,0))
train <- df[index, ]
### Intialize loop
yy <- "log_y"
available.x <- colnames(train)[-22]
chosen.x <- NULL
r2 <- NULL
### Reset the best r^2 for each run
while (length(available.x) > 0) {
          best.r2 <- 0
          ### Iterate model based on previous best r^2 
          for (this.x in available.x) {
                    rhs <- paste(c(chosen.x, this.x), collapse=" + ")
                    f <- as.formula(paste(yy, rhs, sep=" ~ "))
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
            df[,names(df)%in%chosen.x[-c(1)]])
dim(df)

x <- paste(chosen.x[-c(1)], collapse = " + ")
f <- as.formula(paste(yy,x, sep = " ~ "))
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
          predicted <- predict(model, newdata=test, interval="predict")
          conf.int.test=predicted[,2:3]
          fit=predicted[,1]
          r.test <- cor(test$log_y,fit)^2
          ## Combine all values and save them
          result[[paste0("model.",i)]] <- list(model=model,
                                               summary=summary,
                                               coef=coef,
                                               conf.int.train=conf.int,
                                               #conf.int.test=conf.int.test,
                                               r.train = r.train,
                                               r.test = r.test)
}


## Extract coef and caluclate mean and sd
result.coef <- t(sapply(result,"[[","coef"))
percentile95Percbeta=t(apply(result.coef,2,function(x){quantile(x,c(0.025,0.975))}))
coef.mean <- apply(result.coef,2,mean)
coef.sd <- apply(result.coef,2,sd)
output_MUTATE=cbind(coef.mean,percentile95Percbeta,coef.sd)
colnames(output_MUTATE)=c('coef_MuTaTe','MuTaTe2.5','MuTaTe97.5','coef_SD_MuTaTe')



########################################
########################################
########################################
# bootsrap
########################################
########################################
########################################
library(relaimpo)

#relative importance of input variables
relImportance <- calc.relimp(step.model2, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # relative importance
ave.coefs=apply(relImportance$ave.coeffs, 1, mean)
dim(as.matrix(ave.coefs))
# boot <- boot.relimp(step.model2, b = 1000, type = c("lmg"), rank = TRUE, 
#                     diff = TRUE, rela = TRUE)
# booteval.relimp(boot) # print result
#
remove(f)
# bootstrapping with 1000 replications 
library(car)

class(step.model2)
sa=Boot(step.model2,R=5000,labels=names(coef(step.model2)))

summary(sa, high.moments=TRUE)
conf_sa=confint(sa)



# Bootstrap 95% CI for regression coefficients 
# library(boot)
# # function to obtain regression weights 
# bs <- function(formula, data, indices) {
#           d <- data[indices,] # allows boot to select sample 
#           fit <- lm(formula, data=d)
#           return(coef(fit)) 
# } 
# # bootstrapping with 1000 replications 
# library(car)
# sa=Boot(model2,f=coef,5000,labels=names(coef(model2)))
# summary(sa)
# summary(sa, high.moments=TRUE)
# hist(sa, layout=c(1, 3))
# confint(sa)

########################################
########################################
########################################
# combining outputs
########################################
########################################
########################################


cc=cbind(coef_step=coef(step.model2),
      confint(step.model2))


a=coef(lasso.mod.train,s=0.0001286428)#coef for lasso at optimum
aa=matrix(a)
row.names(aa)=row.names(a)  
colnames(aa)='coef_lasso'

                 
b=coef(ridge.mod,s=0.007192172) #coef for ridge at optimum
bb=matrix(b)
row.names(bb)=row.names(b)  
colnames(bb)='coef_ridge'


d=coef(elastinet.mod,s=0.0001341489) #coef for elasticnet at optimum
dd=matrix(d)
row.names(dd)=row.names(d)  
colnames(dd)='coef_eNet'

f=merge(cc,aa,by="row.names",all.x=TRUE)
row.names(f)=f$Row.names
f=f[,-1]
f=merge(f,bb,by="row.names",all.x=TRUE)
row.names(f)=f$Row.names
f=f[,-1]
f=merge(f,dd,by="row.names",all.x=TRUE)
row.names(f)=f$Row.names
f=f[,-1]

boosts1=as.matrix(ave.coefs)
colnames(boosts1)='bootstrap1'
#boots
f=merge(f,boosts1,by="row.names",all.x=TRUE)
row.names(f)=f$Row.names
f=f[,-1]
#mutate
f=merge(f,output_MUTATE,by="row.names",all.x=TRUE)
row.names(f)=f$Row.names
f=f[,-1]
saveRDS(f,'results_coefs.rds')

write.csv(f,'results_coefs_20170516_1851.csv', row.names = F)


















########################################
########################################
########################################
# function designed to get rsq and rsquared adjusted
########################################
########################################
########################################
calculated_rsq=function(dt_selected,mods){
rsquared_calc=cor(log(dt_selected$tgp_cs_ind_nonmda),mods$fitted.values)^2
adj_rsquared_calc=1-(((1-rsquared_calc)*(dim(dt_selected)[1]-1))/(dim(dt_selected)[1]-dim(dt_selected)[2]-1))
rsqs=cbind(rsquared_calc,adj_rsquared_calc)
return(rsqs)
}
calculated_rsq(dt_selected,mods)


########################################
########################################
########################################
#now let's look at the correlation matrix and the significane leveles
########################################
########################################
########################################

nums=sapply(dt_selected, is.numeric)# identifying numeric variables for correlation
dt.numerics=dt_selected[ , nums] #selecting numeric variables in teh data
dim(dt.numerics)

#dt.numerics=dt.numerics[,-117]
names(dt.numerics)
summary(dt.numerics)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
          ut <- upper.tri(cormat)
          data.frame(
                    row = rownames(cormat)[row(cormat)[ut]],
                    column = rownames(cormat)[col(cormat)[ut]],
                    cor  =(cormat)[ut],
                    p = pmat[ut]
          )
}

library(Hmisc)

res2<-rcorr(as.matrix(dt.numerics),type = "pearson") #correlation matrix pearson
corPlusSign=flattenCorrMatrix(round(res2$r,2), round(res2$P,2)) #2 by 2 correlation by person

res3<-rcorr(as.matrix(dt.numerics),type = "spearman") #correlation matrix spearman
corPlusSign2=flattenCorrMatrix(round(res3$r,2),round(res3$P,2)) #2 by 2 correlation by spearman

colnames(corPlusSign)=c("row","column","pearson_cor","pearson_signif")
colnames(corPlusSign2)=c("row","column","spearman_cor","spearman_signif")

corPlusSign_joint=merge(corPlusSign,corPlusSign2,by = c("row","column"))
head(corPlusSign_joint)
#write.csv(corPlusSign_joint,"correlationMAtrixDetail.csv",row.names = F)

#write.csv(dt,"data_explore.csv",row.names = F)
########################################
########################################
########################################
# base model &initial VIF
########################################
########################################
########################################

#the variance inflation factor for the estimated coefficient bk —denoted VIFk —is just the factor by which the variance is inflated.
#  it is a measure of how much the variance of the estimated regression coefficient bk is "inflated" by the existence of correlation among the predictor variables in the model. A VIF of 1 means that there is no correlation among the kth predictor and the remaining predictor variables, and hence the variance of bk is not inflated at all. The general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity requiring correction.
# The VIF for the predictor , tells us that the variance of the estimated coefficient of Weight is inflated by a factor of 8.42 because Weight is highly correlated with at least one of the other predictors in the model.
#  solution for high VIF variable to either find the highly correlated variable and exclude it. if the variabke it self is correlated with many then it might be a good choice to be removed
# more variables usually increase the rate
# Centering a predictor merely entails subtracting the mean of the predictor values in the data set from each predictor value.

model1=lm(log(tgp_cs_ind_nonmda)~.
          -QTY_AMERICANMENU_IND_NONMDA
          -QTY_APPETIZER_IND_NONMDA
          -QTY_BARMENU_IND_NONMDA
          -QTY_BEEF_IND_NONMDA
          -QTY_BEVERAGE_IND_NONMDA
          -QTY_CANFRUITVEG_IND_NONMDA
          -QTY_CHEESE_IND_NONMDA
          -QTY_CHEMICAL_IND_NONMDA
          -QTY_CLASSICMENU_IND_NONMDA
          -QTY_DAIRY_IND_NONMDA
          -QTY_DELIMENU_IND_NONMDA
          -QTY_DISPOSABLE_IND_NONMDA
          -QTY_GROCERYFROZEN_IND_NONMDA
          -QTY_ITALYPIT_IND_NONMDA
          -QTY_MEXIICANMENU_IND_NONMDA
          -QTY_OIL_IND_NONMDA
          -QTY_OTHASIANMENU_IND_NONMDA
          -QTY_PORK_IND_NONMDA
          -QTY_POULTRY_IND_NONMDA
          -QTY_PROCESSEDMEAT_IND_NONMDA
          -QTY_PRODUCE_IND_NONMDA
          -QTY_SEAFOOD_IND_NONMDA
          -QTY_STEAKMENU_IND_NONMDA
          -cop
          #-POI
          -Quarter_number
          #-Investment.PerCS
          #-priceIndex
          #-cop_share
          #- ind_nonmda_share
          -Correct_Invoices
          -DamageFree_Orders
          -OnTime_Orders
          , data = dt.numerics)
summary(model1)
model2=step(model1,direction = "both")
summary(model2)
formula(model2)
drop1(model2,test = "Chisq")
library(rms)
vifs_data=rms::vif(model2)

plot(exp(model2$fitted.values),dt.numerics$tgp_cs_ind_nonmda)
# bootstrapping with 5000 replications 
library(car)
sa=Boot(model2,f=coef,5000,labels=names(coef(model2)))

summary(sa)
summary(sa, high.moments=TRUE)
hist(sa, layout=c(1, 3))
confint(sa,type='bca')

#names(dt.numerics)
#write.csv(vifs_data,"vif_dat a.csv")

########################################
########################################
########################################
#now let's look at distributions
########################################
########################################
########################################
dim(dt.numerics)

#function to plot distributions
plot.distrib=function(dt,col.start,col.end){
          par(mfrow=c(5,5),
              oma = c(2,1,0,0) + 0.1,
              mar = c(2,1,2,2) + 0.1)
          for (i in col.start:col.end)
          {
                    plot(density((dt[,i])),yaxt='n',main = colnames(dt[i]))
          }
}

#function to plot distributions logged
plot.distrib.logged=function(dt,col.start,col.end){
          par(mfrow=c(5,5),
              oma = c(2,1,0,0) + 0.1,
              mar = c(2,1,2,2) + 0.1)
          for (i in col.start:col.end)
          {
                    plot(density(log(dt[,i])),yaxt='n',main = paste("log",colnames(dt[i]),sep = " "))
          }
}

plot.distrib(dt.numerics,1,25)
plot.distrib(dt.numerics,26,50)
plot.distrib(dt.numerics,51,75)
plot.distrib(dt.numerics,76,100)
plot.distrib(dt.numerics,101,116)

plot.distrib.logged(dt.numerics,1,25)
plot.distrib.logged(dt.numerics,26,50)
plot.distrib.logged(dt.numerics,51,75)
plot.distrib.logged(dt.numerics,76,100)
plot.distrib.logged(dt.numerics,101,116)

dev.off()
names(dt1)
dim(dt)


#write.csv(dt,"dt_20170505_1824.csv",row.names = F)
# first basic model

#first replacing 0 in invetment spend case participation with a very small value
which.min(dt$Investment.Spend.CS.participation)
dt[948:949,]

dt1=dt
dt1$Investment.Spend.CS.participation[949]=0.0001
########################################
########################################
########################################
# USF BCG Rank dump 
# drop Investment.PerCS
# average account size
# LIC as an absolute value
########################################
########################################
########################################


#cor(dt_selected$priceIndex,dt_selected$LIC_per_CS)
a=(lm(log(tgp_cs_ind_nonmda)~
                priceIndex+
                
                LIC_per_CS+
                nps+
                trend+
                Investment.Spend.CS.participation+
                Churn_true+
                ave_customer_size+
                USFMarketShareStatic+
                sales_per_drop
      ,data = dt_selected))

b=(lm(log(tgp_cs_ind_nonmda)~
                priceIndex+
                LIC_per_CS+
                nps+
                poly(trend,5)+
                
                Investment.Spend.CS.participation+
                #POI+
                ave_customer_size+
                USFMarketShareStatic+
                #sales_per_drop+
                poultry_share+
                EcomPenetration
      #Cust_Tenure_mnth
      ,data = train2))


summary(b)
plot(b)
plot(predict(b,test2),log(test2$tgp_cs_ind_nonmda))^2
names(dt_selected)

#this is very much overfitted with many correlated variables
mod=lm(log(tgp_cs_ind_nonmda)~.
       -divPEriod
       -FISC_YR_MTH
       -FISC_YR, data = train2)
summary(mod)
mods=step(mod,direction = "both")
summary(mods)
yhat=predict(mods,newdata = test2)

cor(log(test2$tgp_cs_ind_nonmda),yhat)^2



#now let's try ridge and lasso
dim(dt_selected)
set.seed(60134)
mod2=lm(log(tgp_cs_ind_nonmda)~.-divPEriod, data = dt_selected[sample(1:2112,1500),])
summary(mod2)
mods2=step(mod2,direction = "both")
summary(mods2)
library(car)
vifs_data2=car::vif(mods2)
confint(mods)
par(mfrow=c(2,2))
plot(mods2)
par(mfrow=c(2,2))
plot(mods2)


dt_selected[786,]

names(dt_selected)
dim(dt_selected)
corrplot::corrplot.mixed(corr = cor(dt_selected[,c(45,2:16)]),number.cex=0.65,tl.cex = .5)
corrplot::corrplot.mixed(corr = cor(dt_selected[,58:80]),number.cex=0.65,tl.cex = .5)