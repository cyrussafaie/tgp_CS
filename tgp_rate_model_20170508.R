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
dt$sales_per_drop=round(dt$SALES_IND_NONMDA/dt$DROP_CNT_IND_NONMDA,4) #sales per drop 
dt$ave_customer_size=round(dt$SALES_IND_NONMDA,dt$CUSTOMER_CNT_IND_NONMDA) #average customer sales size

dt$ind_nonmda_share=round(dt$QTY_IND_NONMDA/dt$QTY_IND,4) #mda share of IND

#dt$indnonmdalocal_share=dt$QTY_IND_NONMDA_LOCAL/dt$QTY_IND_NONMDA
dt$ind_nonmda_eb_share=round(dt$QTY_EB_IND_NONMDA/dt$QTY_IND_NONMDA,4) # EB voume share
dt$ind_nonmda_packer_share=round(dt$QTY_PACKER_IND_NONMDA/(dt$QTY_IND_NONMDA-dt$QTY_EB_IND_NONMDA),4) #packer share of MB
dt$ind_nonmda_packer_share=round((dt$QTY_IND_NONMDA-dt$QTY_EB_IND_NONMDA-dt$qty)/(dt$QTY_IND_NONMDA-dt$QTY_EB_IND_NONMDA),4) #customer owned share of MB
#shares  doesn't seem reasonable due to dependencies: Grouping to COP, Grocery and dry?,
#grouping the volumes
#COP:poultry,beef,pork,seafood,specialty meat
dt$poultry_share=round(dt$QTY_POULTRY_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$pork_share=round(dt$QTY_PORK_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$seafood_share=round(dt$QTY_SEAFOOD_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$beef_share=round(dt$QTY_BEEF_IND_NONMDA/dt$QTY_IND_NONMDA,4)
# 
dt$canned_share=round(dt$QTY_CANFRUITVEG_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$groceryfrozen_share=round(dt$QTY_GROCERYFROZEN_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$chemical_share=round(dt$QTY_CHEMICAL_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$dairy_share=round(dt$QTY_DAIRY_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$oil_share=round(dt$QTY_OIL_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$processedmeat_share=round(dt$QTY_PROCESSEDMEAT_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$disposable_share=round(dt$QTY_DISPOSABLE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$beverage_share=round(dt$QTY_BEVERAGE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$apperizer_share=round(dt$QTY_APPETIZER_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$cheese_share=round(dt$QTY_CHEESE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$produce_share=round(dt$QTY_PRODUCE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
# 
dt$american_share=round(dt$QTY_AMERICANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$classic_share=round(dt$QTY_CLASSICMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$italian_share=round(dt$QTY_ITALYPIT_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$mexican_share=round(dt$QTY_MEXIICANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$bar_share=round(dt$QTY_BARMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$deli_share=round(dt$QTY_DELIMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$steak_share=round(dt$QTY_STEAKMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$otherasian_share=round(dt$QTY_OTHASIANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$cop=dt$QTY_POULTRY_IND_NONMDA+dt$QTY_BEEF_IND_NONMDA+dt$QTY_SEAFOOD_IND_NONMDA+dt$QTY_PORK_IND_NONMDA+dt$QTY_SPECIALTYMEAT_IND_NONMDA+dt$QTY_PROCESSEDMEAT_IND_NONMDA
dt$cop_share=(dt$QTY_POULTRY_IND_NONMDA+dt$QTY_BEEF_IND_NONMDA+dt$QTY_SEAFOOD_IND_NONMDA+dt$QTY_PORK_IND_NONMDA+dt$QTY_SPECIALTYMEAT_IND_NONMDA+dt$QTY_PROCESSEDMEAT_IND_NONMDA)/dt$QTY_IND_NONMDA
          
dt$typeA_share=round(dt$QTY_CUSTA_IND_NONMDA/dt$QTY_IND_NONMDA,4) #type A colume share
#dt$typeC_share=round(dt$QTY_CUSTC_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$prime_share=round(dt$QTY_PRIME_IND_NONMDA/dt$QTY_IND_NONMDA,4) #prime share of IND non mda

dt$new_share=round(dt$QTY_CUSTNEW_IND_NONMDA/dt$QTY_IND_NONMDA,4) #new customer Volume share
#dt$prior_share=dt$QTY_CUSTPRIOR_IND_NONMDA/dt$QTY_IND_NONMDA

dt$account_per_tm= dt$CUSTOMER_CNT_IND_NONMDA/dt$TM_CNT_IND_NONMDA
dt$sales_per_tm= dt$SALES_IND_NONMDA/dt$TM_CNT_IND_NONMDA

dt$LIC_per_CS=round(dt$LIC_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$TMC_per_CS=round(dt$TMC_IND_NONMDA/dt$QTY_IND_NONMDA,4)

#trend line for tgp rate
dt$trend=rep(1:51,56)
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



ggplot(dt, aes(x=sell_prc_ind_nonmda, y=tgp_cs_ind_nonmda)) +
          geom_point(shape=1) +    # Use hollow circles
          geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

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
dt_selected=dt[,c('Quarter_number',
'QTY_POULTRY_IND_NONMDA',
'QTY_PORK_IND_NONMDA',
'QTY_CANFRUITVEG_IND_NONMDA',
'QTY_GROCERYFROZEN_IND_NONMDA',
'QTY_CHEMICAL_IND_NONMDA',
'QTY_DAIRY_IND_NONMDA',
'QTY_OIL_IND_NONMDA',
'QTY_BEEF_IND_NONMDA',
'QTY_PROCESSEDMEAT_IND_NONMDA',
'QTY_DISPOSABLE_IND_NONMDA',
'QTY_SEAFOOD_IND_NONMDA',
'QTY_BEVERAGE_IND_NONMDA',
'QTY_APPETIZER_IND_NONMDA',
'QTY_CHEESE_IND_NONMDA',
'QTY_PRODUCE_IND_NONMDA',
'QTY_CLASSICMENU_IND_NONMDA',
'QTY_ITALYPIT_IND_NONMDA',
'QTY_AMERICANMENU_IND_NONMDA',
'QTY_MEXIICANMENU_IND_NONMDA',
'QTY_BARMENU_IND_NONMDA',
'QTY_DELIMENU_IND_NONMDA',
'QTY_STEAKMENU_IND_NONMDA',
'QTY_OTHASIANMENU_IND_NONMDA',
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
'DonD._Share',
'Price_approval_share',
'tgp_cs_ind_nonmda',
'ind_share',
'ind_nonmda_share',
'ind_nonmda_eb_share',
'ind_nonmda_packer_share',
'cop',
'cop_share',
'typeA_share',
'prime_share',
'new_share',
'account_per_tm',
'sales_per_tm',
'trend'
# ,
#added just for testing
# 'poultry_share',
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
# 'otherasian_share'
)]
names(dt_selected)
dim(dt_selected)
corrplot::corrplot.mixed(corr = cor(dt_selected[,c(45,2:16)]),number.cex=0.65,tl.cex = .5)
corrplot::corrplot.mixed(corr = cor(dt_selected[,58:80]),number.cex=0.65,tl.cex = .5)
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
