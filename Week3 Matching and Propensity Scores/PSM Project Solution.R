install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")

# Data loadin
library(tableone)
library(Matching)
library(MatchIt)
data(lalonde)

# Question1 SMD
xvars <- colnames(lalonde)
xvars <- xvars[xvars!='treat']
table1<- CreateTableOne(vars=xvars,strata="treat", data=lalonde, test=FALSE) 
print(table1,smd=TRUE) 


# Question2 unadjusted mean
library(dplyr)
treated_mean <- mean(lalonde[lalonde$treat==1,]$re78)
untreated_mean <- mean(lalonde[lalonde$treat==0,]$re78)
print(treated_mean-untreated_mean)

# Question3: propensity score estimation
psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(),data=lalonde) 

#show coefficients etc 
summary(psmodel) 

#create propensity score 
pscore<-psmodel$fitted.values 

# max and min of psc
max(pscore)
min(pscore)


# Question4&5: after PSM
set.seed(931139)

psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE) 
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences 

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE) 

print(matchedtab1, smd = TRUE) 

# Question6: Redo matching using caliper
set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE, caliper = 0.1) 
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences 

matchedtab2<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE) 

print(matchedtab2, smd = TRUE) 

# Question7:
treated_mean2 <- mean(matched[matched$treat==1,]$re78)
untreated_mean2 <- mean(matched[matched$treat==0,]$re78)
print(treated_mean2-untreated_mean2)

# Question8:
#outcome analysis 
y_trt<-matched$re78[matched$treat==1] 

y_con<-matched$re78[matched$treat==0] 

#pairwise difference 
diffy<-y_trt-y_con 

#paired t-test 
t.test(diffy) 


