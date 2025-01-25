##import data
myfile<-readr::read_tsv("myfile.txt",col_names = FALSE)
colnames(myfile)<-c( 'SubjectID_1','Sensor_1','NFIQ_1','Minutiae_1',
                  'SubjectID_2','Sensor_2','NFIQ_2','Minutiae_2',
                  'deltax','deltay','rotation','Match',
                  'Gradient','mean','std','prnu',
                  'a1le','d1le','v1le','h1le',
                  'a1se','d1se','v1se','h1se',
                  '5b11_dist', '5b11_min', '5b11_max', '5b11_std',
                  'Label')

subject_data_all<-readr::read_csv("subject data - all.csv")
subject_data_all[,4]<-as.Date(subject_data_all$DOB,"%m/%d/%Y")

#data prepare

##compute age
require(lubridate)
refDate<-"2020-02-25"  ##refDate 20200225
aged<- as.period(interval(subject_data_all$DOB,refDate),unit = "year")
age<-aged$year
subject_data_all<-cbind(subject_data_all,age)

##combine 2 dataset
file_comb<-left_join(myfile,subject_data_all,
                    by=c("SubjectID_1"="RID"))
file_comb2<-left_join(file_comb,subject_data_all,
                     by=c("SubjectID_2"="RID"))

workfile<-file_comb2[,c(1,2,5,6,12,29:31,34:36,39)]
colnames(workfile)<-c( "SubjectID_1","Sensor_1","SubjectID_2", "Sensor_2","Match","Label",
                      "Gender_1","Ethnicity_1","age_1","Gender_2","Ethnicity_2","age_2"  )

library(tidyverse)
workfile<-workfile %>% drop_na() #check if there is any na values

workfile_S<-workfile %>% filter(Sensor_1 ==Sensor_2)##filtering obs withsame sensor
##regoup ethnicity
### race frequency
library(plyr)
count(workfile_S,'Ethnicity_1')
##Ethnicity_1  freq
##African  4895
## African American 10840
##  American Indian   275
##     Asian  7650
##     Asian Indian  9425
##         Caucasian 58860
##     Hispanic  2600
##   Middle Eastern  6610
##            Other  2875
##         Unknown   545
workfile_S$Race1<-ifelse(workfile_S$Ethnicity_1=="Caucasian",0,1)
workfile_S$Race2<-ifelse(workfile_S$Ethnicity_2=="Caucasian",0,1)
workfile_S$Gender_1<-ifelse(workfile_S$Gender_1=="Female",0,1)
workfile_S$Gender_2<-ifelse(workfile_S$Gender_2=="Female",0,1)
workfile_S<-workfile_S[,-c(8,11)]
table(workfile_S,'Race1')

#########subset of 5 sensors
workfile_S0<- workfile_S %>% filter(Sensor_1 ==0)
workfile_S1<- workfile_S %>% filter(Sensor_1 ==1)
workfile_S2<- workfile_S %>% filter(Sensor_1 ==2)
workfile_S3<- workfile_S %>% filter(Sensor_1 ==3)
workfile_S4<- workfile_S %>% filter(Sensor_1 ==4)

#############model of gender
coef0_gender <- lm(Match~Label+Gender_1+Gender_2+Gender_1:Label, data=workfile_S0)
coef1_gender <- lm(Match~Label+Gender_1+Gender_2+Gender_1:Label, data=workfile_S1)
coef2_gender <- lm(Match~Label+Gender_1+Gender_2+Gender_1:Label, data=workfile_S2)
coef3_gender<- lm(Match~Label+Gender_1+Gender_2+Gender_1:Label, data=workfile_S3)
coef4_gender <- lm(Match~Label+Gender_1+Gender_2+Gender_1:Label, data=workfile_S4)
 
coeffient_gender<-rbind(coef0_gender$coefficients,coef1_gender$coefficients,
                 coef2_gender$coefficients,coef3_gender$coefficients,
                 coef4_gender$coefficients)
rownames(coeffient_gender)<-c('sensor0','sensor1','sensor2','sensor3','sensor4')


#############model of age
coef0_age <- lm(Match~Label+age_1+age_2+age_1:Label, data=workfile_S0)
coef1_age <- lm(Match~Label+age_1+age_2+age_1:Label, data=workfile_S1)
coef2_age <- lm(Match~Label+age_1+age_2+age_1:Label, data=workfile_S2)
coef3_age <- lm(Match~Label+age_1+age_2+age_1:Label, data=workfile_S3)
coef4_age <- lm(Match~Label+age_1+age_2+age_1:Label, data=workfile_S4)

coeffient_age<-rbind(coef0_age$coefficients,coef1_age$coefficients,
                 coef2_age$coefficients,coef3_age$coefficients,
                 coef4_age$coefficients)
rownames(coeffient_age)<-c('sensor0','sensor1','sensor2','sensor3','sensor4')



#################full model

coef0_full <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label+Race1+Race2+Race1:Label, data=workfile_S0)
coef1_full <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label+Race1+Race2+Race1:Label, data=workfile_S1)
coef2_full <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label+Race1+Race2+Race1:Label, data=workfile_S2)
coef3_full <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label++Race1+Race2+Race1:Label, data=workfile_S3)
coef4_full <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label+Race1+Race2+Race1:Label, data=workfile_S4)


coeffient_full<-rbind(coef0_full$coefficients,coef1_full$coefficients,
                     coef2_full$coefficients,coef3_full$coefficients,
                     coef4_full$coefficients)
rownames(coeffient_full)<-c('sensor0','sensor1','sensor2','sensor3','sensor4')

result_full0<-summary(coef0_full)$coefficients[,c(1,4)]##in sensor=0Label:Race1 is not significant
result_full1<-summary(coef1_full)$coefficients[,c(1,4)]
result_full2<-summary(coef2_full)$coefficients[,c(1,4)]
result_full3<-summary(coef3_full)$coefficients[,c(1,4)]
result_full4<-summary(coef4_full)$coefficients[,c(1,4)]
result_full<-cbind(result_full0,result_full1,result_full2,result_full3,result_full4)
write.table(result_full,file='result of full model')

#### for sensor=0,removing interaction of race and then removing race
###### the model without interaction of race and 
#######the moedel without race and also the interaction have the same R2
#######race is not a significant variable when sensor=0
coef0_redu <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label+Race1+Race2, data=workfile_S0)## race is not significant model
result_redu0<-summary(coef0_redu)$coefficients[,c(1,4)]
write.table(result_redu0,file='resut of redu0')

coef0_redu2 <- lm(Match~Label+age_1+age_2+age_1:Label+Gender_1+Gender_2
                 +Gender_1:Label, data=workfile_S0)
result_redu0_2<-summary(coef0_redu2)$coefficients[,c(1,4)]
write.table(result_redu0_2,file='resut of redu')

#####################roc
###sigma of variances for the Label=0 and Label=1 groups

sigma0_0<-aggregate(workfile_S0$Match, by=list(workfile_S0$Label), FUN=var)[1,2]
sigma1_0<-aggregate(workfile_S0$Match, by=list(workfile_S0$Label), FUN=var)[2,2]
sigma0_1<-aggregate(log(workfile_S1$Match + 1), by=list(workfile_S1$Label), FUN=var)[1,2]##########big diff of variance
sigma1_1<-aggregate(log(workfile_S1$Match + 1), by=list(workfile_S1$Label), FUN=var)[2,2]
sigma0_2<-aggregate(workfile_S2$Match, by=list(workfile_S2$Label), FUN=var)[1,2]
sigma1_2<-aggregate(workfile_S2$Match, by=list(workfile_S2$Label), FUN=var)[2,2]
sigma0_3<-aggregate(workfile_S3$Match, by=list(workfile_S3$Label), FUN=var)[1,2]
sigma1_3<-aggregate(workfile_S3$Match, by=list(workfile_S3$Label), FUN=var)[2,2]
sigma0_4<-aggregate(workfile_S4$Match, by=list(workfile_S4$Label), FUN=var)[1,2]
sigma1_4<-aggregate(workfile_S4$Match, by=list(workfile_S4$Label), FUN=var)[2,2]

hist(filter(workfile_S0, Label=='1')$Match)
hist(filter(workfile_S0, Label=='0')$Match)
hist(filter(workfile_S1, Label=='1')$Match)
hist(filter(workfile_S1, Label=='0')$Match)
hist(filter(workfile_S2, Label=='1')$Match)
hist(filter(workfile_S2, Label=='0')$Match)
hist(filter(workfile_S3, Label=='1')$Match)
hist(filter(workfile_S3, Label=='0')$Match)
hist(filter(workfile_S4, Label=='1')$Match)
hist(filter(workfile_S4, Label=='0')$Match)


hist(log(filter(workfile_S2, Label=='1')$Match))
hist(log(filter(workfile_S2, Label=='0')$Match))

tem<-log(workfile_S1$Match)
tem<-tem[tem > -Inf]
var(tem)
##roc function
#######Xij i=1,2,3 (1=age 2=gender 3=race) j=1,2(subject)

roc_fn <- function(p,x11,x12=0,x21,x22=0,x31,x32=0,
                   beta_0,beta_L,beta_11,beta_L11,beta_21,beta_L21,beta_31=0,beta_L31=0, beta_12=0, beta_L12=0,  beta_22=0, beta_L22=0, beta_32=0, beta_L32=0,
                   sigma0, sigma1){
  
  mu1 = beta_0+beta_L+(beta_11+beta_L11)*x11+(beta_12+beta_L12)*x12+(beta_21+beta_L21)*x21+(beta_22+beta_L22)*x22+(beta_31+beta_L31)*x31+(beta_32+beta_L32)*x32
  
  mu0 = beta_0+beta_11*x11+beta_12*x12+beta_21*x21+beta_22*x22+beta_31*x31+beta_32*x32
  
  a=(mu1-mu0)/sigma1
  
  b=sigma0/sigma1
  
  roc=1-pnorm(b*qnorm(1-p)-a)
  
  return(roc)
  
}



####
b <- function(n){
  fpr=vector()
  for (i in 1:n){
    fpr[i]<-i/n
  }
  return(fpr)
}

fpr <- b(1000)

# fpr<-runif(1000)
######sensor=1 full model

# roc_fn(p=0.99,x11=mean(workfile_S0$age_1),x12=0,x21=1,x22=0,x31=1,x32=0,
#        beta_0=result_full[1,3],beta_L=result_full[2,3],
#        beta_11=result_full[3,3],beta_L11=result_full[9,3],beta_21=result_full[5,3],beta_L21=result_full[10,3],beta_31=result_full[7,3],beta_L31=result_full[11,3], 
#        beta_12=result_full[4,3], beta_L12=0,  beta_22=result_full[6,3], beta_L22=0, beta_32=result_full[8,3], beta_L32=0,
#        sigma0=sigma0_1, sigma1=sigma1_1)

sensitivity<-function(fpr){
  sensitivity_1=vector()
  for (i in 1:length(fpr)){
    sensitivity_1[i]<-roc_fn(p=fpr[i],x11=mean(workfile_S1$age_1),x12=mean(workfile_S1$age_2),x21=1,x22=0,x31=1,x32=0,
                             beta_0=result_full[1,3],beta_L=result_full[2,3],
                             beta_11=result_full[3,3],beta_L11=result_full[9,3],beta_21=result_full[5,3],beta_L21=result_full[10,3],beta_31=result_full[7,3],beta_L31=result_full[11,3], 
                             beta_12=result_full[4,3], beta_L12=0,  beta_22=result_full[6,3], beta_L22=0, beta_32=result_full[8,3], beta_L32=0,
                             sigma0=sigma0_1, sigma1=sigma1_1)
  }
  return(sensitivity_1)
}

a1<-sensitivity(fpr)

AUC1<-round(sum(a1[1:length(a1)]*diff(c(0, fpr[1:length(a1)]))),2)
AUC1
######sensor=2 full model
sensitivity2<-function(fpr){
  sensitivity_2=vector()
  for (i in 1:length(fpr)){
    sensitivity_2[i]<-roc_fn(p=fpr[i],x11=mean(workfile_S2$age_1),x12=mean(workfile_S2$age_2),x21=1,x22=0,x31=1,x32=0,
                             beta_0=result_full[1,5],beta_L=result_full[2,5],
                             beta_11=result_full[3,5],beta_L11=result_full[9,5],beta_21=result_full[5,5],beta_L21=result_full[10,5],beta_31=result_full[7,5],beta_L31=result_full[11,5], 
                             beta_12=result_full[4,5], beta_L12=0,  beta_22=result_full[6,5], beta_L22=0, beta_32=result_full[8,5], beta_L32=0,
                             sigma0=sigma0_2, sigma1=sigma1_2)
  }
  return(sensitivity_2)
}

a2<-sensitivity2(fpr)

AUC2<-round(sum(a1[1:length(a1)]*diff(c(0, fpr[1:length(a1)]))),2)
AUC2

######sensor=3 full model




######sensor=4 full model






######sensor=0 reduced model

#####################moedel age
sensitivity_age<-function(fpr){
  sensitivity_age=vector()
  for (i in 1:length(fpr)){
    sensitivity_age[i]<-roc_fn(p=fpr[i],x11=mean(workfile_S1$age_1),x12=mean(workfile_S1$age_2),x21=0,x22=0,x31=0,x32=0,
                             beta_0=result_full[2,1],beta_L=result_full[2,2],
                             beta_11=result_full[2,3],beta_L11=result_full[2,5],beta_21=0,beta_L21=0,beta_31=0,beta_L31=0, 
                             beta_12=result_full[2,4], beta_L12=0,  beta_22=0, beta_L22=0, beta_32=0, beta_L32=0,
                             sigma0=sigma0_1, sigma1=sigma1_1)
  }
  return(sensitivity_age)
}

a_age<-sensitivity_age(fpr)

AUC_age<-round(sum(a_age[1:length(a_age)]*diff(c(0, fpr[1:length(a_age)]))),2)
AUC_age


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/home/menglinghe/Documents/menglinghe/bio")



