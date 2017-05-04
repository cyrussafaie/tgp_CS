dt=read.csv("full_nonmda_ind20170502_complete - original.csv")
dim(dt)
names(dt)
str(dt,list.len = 999)
library(ggplot2)

#cor(dt[,29:32])
#exclude salad and specialty meat
# exclude potentially customer type B
# exclude price source 1 and 4
#lost and new count seems a bit wierd may need to rerun and the counts
tail(dt)

dt$tgp_cs_ind_nonmda=round(dt$TGP_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$ind_share=round(dt$QTY_IND/dt$QTY_TTL,4)
dt$sell_prc_ind_nonmda= round(dt$SALES_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$tgp_per_drop=round(dt$TGP_IND_NONMDA_RT_TYPE/dt$DROP_CNT_IND_NONMDA,4)

dt$ind_mda_share=round((dt$QTY_IND-dt$QTY_IND_NONMDA)/dt$QTY_IND,4)
#dt$indnonmdalocal_share=dt$QTY_IND_NONMDA_LOCAL/dt$QTY_IND_NONMDA
dt$ind_nonmda_eb_share=round(dt$QTY_EB_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$ind_nonmda_packer_share=round(dt$QTY_PACKER_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$poultry_share=round(dt$QTY_POULTRY_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$pork_share=round(dt$QTY_PORK_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$canned_share=round(dt$QTY_CANFRUITVEG_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$groceryfrozen_share=round(dt$QTY_GROCERYFROZEN_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$chemical_share=round(dt$QTY_CHEMICAL_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$dairy_share=round(dt$QTY_DAIRY_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$oil_share=round(dt$QTY_OIL_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$beef_share=round(dt$QTY_BEEF_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$processedmeat_share=round(dt$QTY_PROCESSEDMEAT_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$disposable_share=round(dt$QTY_DISPOSABLE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$beverage_share=round(dt$QTY_BEVERAGE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$apperizer_share=round(dt$QTY_APPETIZER_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$seafood_share=round(dt$QTY_SEAFOOD_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$cheese_share=round(dt$QTY_CHEESE_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$produce_share=round(dt$QTY_PRODUCE_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$american_share=round(dt$QTY_AMERICANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$classic_share=round(dt$QTY_CLASSICMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$italian_share=round(dt$QTY_ITALYPIT_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$mexican_share=round(dt$QTY_MEXIICANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$bar_share=round(dt$QTY_BARMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$deli_share=round(dt$QTY_DELIMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$steak_share=round(dt$QTY_STEAKMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$otherasian_share=round(dt$QTY_OTHASIANMENU_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$typeA_share=round(dt$QTY_CUSTA_IND_NONMDA/dt$QTY_IND_NONMDA,4)
dt$typeC_share=round(dt$QTY_CUSTC_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$prime_share=round(dt$QTY_PRIME_IND_NONMDA/dt$QTY_IND_NONMDA,4)

dt$new_share=round(dt$QTY_CUSTNEW_IND_NONMDA/dt$QTY_IND_NONMDA,4)
#dt$prior_share=dt$QTY_CUSTPRIOR_IND_NONMDA/dt$QTY_IND_NONMDA

dt$account_per_tm= dt$CUSTOMER_CNT_IND_NONMDA/dt$TM_CNT_IND_NONMDA
dt$sales_per_tm= dt$SALES_IND_NONMDA/dt$TM_CNT_IND_NONMDA

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

#JAckson closed period ecluded 201410 to 201506

subset(dt,is.na(dt$nps))


VIF(lm(tgp_cs_ind_nonmda~sales_per_tm+account_per_tm+typeA_share+typeC_share,data = dt))


test=glm(tgp_cs_ind_nonmda~1,data = dt)
summary(test)
mean(dt$tgp_cs_ind_nonmda)
sd(dt$tgp_cs_ind_nonmda)
test=step(lm(tgp_cs_ind_nonmda~.,data = dt))
summary(test)


#variables of interest
selected_variables=c(#'DIV_NM',
#'FISC_YR_MTH',
'tgp_cs_ind_nonmda',
'FISC_YR',
'FISC_MTH_OF_YR',
'Quarter_number',
'EcomPenetration',
#'Correct_Invoices',
#'Complete_Orders',
#'DamageFree_Orders',
#'OnTime_Orders',
#'POI',
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
'ind_share',
#'tgp_per_drop',
'ind_mda_share',
'ind_nonmda_eb_share',
'ind_nonmda_packer_share',
'poultry_share',
'pork_share',
'canned_share',
'groceryfrozen_share',
'chemical_share',
'dairy_share',
'oil_share',
'beef_share',
'processedmeat_share',
'disposable_share',
'beverage_share',
'apperizer_share',
'seafood_share',
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
'typeA_share',
'typeC_share',
'prime_share',
'new_share',
'account_per_tm',
'sales_per_tm',
'trend') 

dt.model=subset(dt[,selected_variables],dt[,selected_variables]$FISC_YR!='2013')
dim(dt.model)

library(caret)
library(fmsb)
model.lm.full=lm(tgp_cs_ind_nonmda~.,data = dt.model)
summary(model.lm.full)
varImp(model.lm.full, scale = FALSE)
names(dt.model)

fin=dt.model[complete.cases(dt.model),]

a=step(model.lm.full, direction = "both")
summary(a)
str(dt.model)

summary(model.lm.full)
summary(dt.model)

drop1(model.lm.full,test = "Chisq")
vif_func(dt.model[,-1],5)
dim(fin)
corrplot::corrplot.mixed(corr = cor(fin[,1:30]),number.cex=0.65,tl.cex = .5)
corrplot::corrplot.mixed(corr = cor(fin[,31:55]),number.cex=0.65,tl.cex = .5)
corrplot::corrplot(corr = cor(fin[,1:55]),number.cex=0.75,tl.cex = .5, type="lower",order ="hclust")

cor.mtest <- function(mat, conf.level = 0.95){
          mat <- as.matrix(mat)
          n <- ncol(mat)
          p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
          diag(p.mat) <- 0
          diag(lowCI.mat) <- diag(uppCI.mat) <- 1
          for(i in 1:(n-1)){
                    for(j in (i+1):n){
                              tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
                              p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
                              lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
                              uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
                    }
          }
          return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(fin,0.95)
round(cor(fin),2)
dim(res1[[1]])
corrplot::corrplot(cor(fin), p.mat = res1[[1]], sig.level=0.95)

?cor.test()

library(Hmisc)

res2 <- rcorr(as.matrix(fin))
names(res2)

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
res2<-rcorr(as.matrix(fin))
flattenCorrMatrix(round(res2$r,2), round(res2$P,2))

