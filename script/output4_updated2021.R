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


####simple roc function with one covariate
# roc_fn_s <- function(p,x11,x12=0,beta_0,beta_L,beta_11,beta_L11, beta_12=0, sigma0, sigma1){
#   
#   mu1 = beta_0+beta_L+(beta_11+beta_L11)*x11+beta_12*x12
#   
#   mu0 = beta_0+beta_11*x11+beta_12*x12
#   
#   a=(mu1-mu0)/sigma1
#   
#   b=sigma0/sigma1
#   
#   roc=1-pnorm(b*qnorm(1-p)-a)
#   
#   return(roc)
# }

#####input data
#setwd("I:/Menglinghe/match")
output4<-readr::read_csv("output4.csv",col_names = TRUE)
output4<-output4[,-1]
colnames(output4)


###############data prepare
workfile_4<-output4[,c(2,9:15)]
workfile_4$match<-as.numeric(workfile_4$match=="match") ##### if match,then match=1, otherwise,match=0
#genuine score datasubset
workfile_4_genuine<-subset(workfile_4,match=='1')
workfile_4_genuine<-subset(workfile_4_genuine,max_score<200)##exculde outliers
workfile_4_genuine$ethnicity<-ifelse(workfile_4_genuine$ethnicity=="Caucasian","Caucasian","Non Caucasian")

workfile_4$gender<-as.numeric(workfile_4$gender=="Male")
workfile_4$max_id_gender<-as.numeric(workfile_4$max_id_gender=="Male")##### if gender=Male,then gender=1, otherwise,gender=0

library(plyr)
count(workfile_4,'ethnicity')
count(workfile_4,'max_id_ethnicity')
workfile_4$ethnicity<-as.numeric(workfile_4$ethnicity=="Caucasian")
workfile_4$max_id_ethnicity<-as.numeric(workfile_4$max_id_ethnicity=="Caucasian")##### if ethnicity=Caucasian,then ethnicity=1, otherwise,ethnicity=0


###############exculde outliers
# workfile_4_1<-subset(workfile_4,match=='1')
# hist(workfile_4_1$max_score)
# summary(workfile_4_1$max_score)
# 
# hist(subset(workfile_4,match=='0')$max_score)
# summary(subset(workfile_4,match=='0')$max_score)

# quantile(output4$max_score, probs = c(0.75, 0.85, 0.9, 0.95))
# 75%          85%          90%          95% 
#   3.973050e+01 5.205160e+01 6.458740e+01 2.850011e+21 
# > quantile(output4$max_score, probs = c(0.91,0.92,0.93,0.94))
# 91%          92%          93%          94% 
#   6.667718e+01 7.603592e+01 1.318015e+02 4.407066e+16 
# > quantile(output4$max_score, probs = c(seq(0.92,0.94, by=0.001)))
# 92%        92.1%        92.2%        92.3%        92.4%        92.5%        92.6%        92.7%        92.8%        92.9%          93%        93.1%        93.2% 
# 7.603592e+01 7.618995e+01 7.966358e+01 8.218962e+01 8.492298e+01 9.376670e+01 1.092987e+02 1.172908e+02 1.208933e+02 1.247775e+02 1.318015e+02 1.418606e+05 1.601793e+16 
# 93.3%        93.4%        93.5%        93.6%        93.7%        93.8%        93.9%          94% 
#   4.357477e+16 4.383531e+16 4.387406e+16 4.395509e+16 4.404327e+16 4.404327e+16 4.404327e+16 4.407066e+16 
workfile_4_extreme<-subset(workfile_4,max_score>200)

workfile_4_final<-subset(workfile_4,max_score<200)
workfile_4_final$Label<-ifelse(workfile_4_final$match=="1",'Genuine Score','Imposter Score')

library(plyr)
count(workfile_4_final,'Label')### create label for plot purpose
# Label freq
# 1  Genuine Score   19
# 2 Imposter Score  702
#hist(subset(workfile_4_final,match=='0')$max_score)
#hist(subset(workfile_4_final,match=='1')$max_score)

sigma_0<-sd(subset(workfile_4_final,match=='0')$max_score)
sigma_1<-sd(subset(workfile_4_final,match=='1')$max_score)
summary(subset(workfile_4_final,match=='0')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.38   21.82   27.63   30.13   36.08   82.91 
summary(subset(workfile_4_final,match=='1')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   24.96   46.45   58.90   85.18  133.44 

##########standardise score
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
workfile_4_final$max_score_standardized<-range01(workfile_4_final$max_score)












#############################################Data plot#############################################################
######density plot for match score
library(plyr)
mu <- ddply(workfile_4_final, "Label", summarise, grp.mean=mean(max_score))
set.seed(42)
library(ggplot2)
library(extrafont)
#font_import()
ggplot(workfile_4_final, aes(max_score, fill=Label, color=Label)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,140,5), alpha=0.8, 
                 position="identity", lwd=0.2) +
  theme_bw() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Label),linetype="dashed")+
  #ggtitle("Normalized Histogram of Match Score")+
  labs(x='Match Score',title = "Distributions of Match Scores")+labs(y='Density')+
  theme(#legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 14,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        axis.text = element_text(size=14, family="Arial")
        #plot.background = element_blank(),
        # panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank()
        )

ggplot(workfile_4_genuine, aes(age, max_score,size=max_score,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='Genuine Score',title ="Genuine Match Scores vs. Age" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 

ggplot(data=workfile_4_genuine, aes(x=max_score, color=gender,fill=gender)) +
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='Genuine Score',y='Density',title ="Distributions of Genuine Match Scores\n based on Gender" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 14,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(data=workfile_4_genuine, aes(x=max_score, color=ethnicity,fill=ethnicity)) +
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='Genuine Score',y='Density',title ="Distributions of Genuine Match Scores\n based on Ethnicity" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 14,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


  # theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
  #       legend.position = c(0.8,0.85),
  #       legend.title = element_blank(),
  #       legend.text = element_blank(),
  #       #text=element_text(size=14, family="Arial"),
  #       axis.text = element_text(size=14, family="Arial"))
  #       #plot.background = element_blank(),
  #       # panel.grid.major = element_blank(),
  #       # panel.grid.minor = element_blank())
  # 









############################################ model B C D A E ################################################################

#####################model with only age#####
model_age_output4 <- lm(max_score~match+age+max_id_age+age:match, data=workfile_4_final)
coef_age<-model_age_output4$coefficients#####use the interaction even though it is insignificant
# model_age_output4_2 <- lm(max_score~match+age+max_id_age, data=workfile_4_final)
# summary(model_age_output4_2)

#####################model with only gender#####
model_gender_output4 <- lm(max_score~match+gender+max_id_gender+gender:match, data=workfile_4_final)
coef_gender<-model_gender_output4$coefficients

#####################model with only ethnicity#####
model_ethnicity_output4 <- lm(max_score~match+ethnicity+max_id_ethnicity+ethnicity:match, data=workfile_4_final)
coef_ethnicity<-model_ethnicity_output4$coefficients

#####################model with age and gender (both with interaction with match) use this model
model_full <- lm(max_score~match+age+max_id_age+gender+max_id_gender+age:match+gender:match, data=workfile_4_final)
coef_age_gender_full<-model_full$coefficients

#####################model with age and gender (only interaction of gender and match) 
# model_output4 <- lm(max_score~match+age+max_id_age+gender+max_id_gender+gender:match, data=workfile_4_final)
# coef_age_gender<-model_output4$coefficients
# summary(model_output4)
# #####################model with age and gender (no interaction) 
# model_output4_s <- lm(max_score~match+age+max_id_age+gender+max_id_gender, data=workfile_4_final)
# coef<-model_output4_s$coefficients
# summary(model_output4_s)

#####################model with age gender and ethnicity (both with interaction with match) use this model
model_4 <- lm(max_score~match+age+max_id_age+gender+max_id_gender+ethnicity+max_id_ethnicity+
                age:match+gender:match+ethnicity:match, data=workfile_4_final)
coef_model_4<-model_4$coefficients












################################################### ROC Curve #########################################################

fpr<-seq(0,1,by=0.001)[-1]

# sensitivity<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = mean(workfile_4_final$age),x12=mean(workfile_4_final$max_id_age),x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
#                              beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# 
# sensitivity_all<-sensitivity(fpr)
# plot(x=fpr,y=sensitivity_all)
# 
# AUC<-round(sum(sensitivity_all[1:length(sensitivity_all)]*diff(c(0, fpr[1:length(sensitivity_all)]))),2)
# AUC
# 
# sensitivity_genuine<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 20,x12=0,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 = 0,
#                              beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# 
# sensitivity_g<-sensitivity_genuine(fpr)
# plot(x=fpr,y=sensitivity_g)
# 
# AUC<-round(sum(sensitivity_all[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC



##############pooled ROC
# mean0<-mean(subset(workfile_4_final,match=='0')$max_score)
# mean1<-mean(subset(workfile_4_final,match=='1')$max_score)
# roc_pooled <- vector()
#   for (i in 1:length(fpr)){
#     roc_pooled[i]<-1-pnorm(sigma_0/sigma_1*qnorm(1-fpr[i])-(mean1-mean0)/sigma_1)
#   }
# plot(x=fpr,y=roc_pooled,ylim = c(0,1),
#      main="Pooled ROC",ylab='Sensitivity',xlab='FPR',
#      type="l")

#######method 1
# library('PRROC')
# pooled_roc<-roc(workfile_4_final$match, workfile_4_final$max_score_standardized)
# plot(pooled_roc)
# rs <- smooth(pooled_roc, method="binormal")
# plot(rs,col="green")
# #######method 2
# library('ggplot2')
# library('plotROC')
# ggplot(workfile_4_final,aes(d=match,m=max_score))+geom_roc(n.cuts = 0)

##################method 3(use this method)########################################################################
###############pooled ROC
sensitivityfun_pool<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x12=0,x21=0,x22=0,x31=0,beta_0 =mu$grp.mean[1],beta_L =mu$grp.mean[1]-mu$grp.mean[2],beta_11 = 0,beta_L11 =0,
                             beta_21 = 0,beta_L21 = 0,beta_22 = 0,sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_pool<-sensitivityfun_pool(fpr)
AUCpool<-round(sum(sensitivity_pool[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCpool#0.75




###############roc of gender model_gender_output4
sensitivityfun_gender0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x12=0,x21=0,x22=1,x31=0,beta_0 = coef_gender[1],beta_L = coef_gender[2],beta_11 = 0,beta_L11 =0,
                             beta_21 = coef_gender[3],beta_L21 = coef_gender[5],beta_22 = coef_gender[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_gender0<-sensitivityfun_gender0(fpr)
# plot(x=fpr,y=sensitivity_gender0,ylim = c(0,1),
#      main="Gender Female",ylab='Sensitivity',xlab='FPR',
#      type="l")
AUCgender0<-round(sum(sensitivity_gender0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCgender0#######is 0.5 when gender1=0

sensitivityfun_gender1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x12=0,x21=1,x22=1,x31=0,
                             beta_0 = coef_gender[1],beta_L = coef_gender[2],beta_11 = 0,beta_L11 =0,
                             beta_21 = coef_gender[3],beta_L21 = coef_gender[5],beta_22 = coef_gender[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_gender1<-sensitivityfun_gender1(fpr)
# plot(x=fpr,y=sensitivity_gender1,ylim = c(0,1),
#      main="Gender Male",ylab='Sensitivity',xlab='FPR',
#      type="l")
AUCgender1<-round(sum(sensitivity_gender1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCgender1#######is 0.81 when gender1=1

rocdata_gender<-data.frame(sensitivity_gender1,sensitivity_gender0,sensitivity_pool,fpr)
#plot method2 
#no pool
# rocdata_gender_melt<-reshape2::melt(rocdata_gender, id.var='fpr')
# colnames(rocdata_gender_melt)[2]<-"Gender"
# rocdata_gender_melt$Gender<- ifelse(rocdata_gender_melt$Gender=="sensitivity_gender1","Male AUC=0.81","Female AUC=0.5")
# ggplot(rocdata_gender_melt, aes(x=fpr, y=value, type= Gender,col=Gender)) +
#   labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Gender))+
#   scale_linetype_manual(values=c("solid","longdash"))+
#   theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
#         legend.position = c(0.8,0.2),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 8),
#         plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
star<-c(0.25,0.50,0.75)
rocdata_gender_melt<-reshape2::melt(rocdata_gender, id.var='fpr')
colnames(rocdata_gender_melt)[2]<-"Gender"
rocdata_gender_melt$Gender<- ifelse(rocdata_gender_melt$Gender=="sensitivity_gender1","Male AUC=0.81",
                                    ifelse(rocdata_gender_melt$Gender=="sensitivity_gender0","Female AUC=0.5","Pooled AUC=0.75"))
rocdata_gender_melt$point<-ifelse(rocdata_gender_melt$fpr%in%star,rocdata_gender_melt$value,NA)
rocdata_gender_melt$point<-ifelse(rocdata_gender_melt$Gender=="Pooled AUC=0.75",NA,rocdata_gender_melt$point)  
  

ggplot(rocdata_gender_melt, aes(x=fpr,type= Gender,col=Gender)) +
    labs(x="FPR", y="Sensitivity")+ 
    geom_line(aes(y=value,linetype=Gender),size=1.2)+
    scale_linetype_manual(values=c("dotted","longdash","solid"))+
    geom_point(aes(y=point,shape=Gender),size=3)+
    scale_shape_manual(values=c(15,16,NA))+
    ggtitle("Gender-Adjusted ROC Curves")+
    theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
         text=element_text(size=14, family="Arial"),
          plot.title = element_text(size=14, family="Arial",hjust = 0.5),
          legend.position = c(0.75,0.2),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 14,family="Arial"),
          axis.text = element_text(size=14, family="Arial"),
          panel.background = element_blank(),#changed
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
         )




###############roc of age  18 20 25 40 60 

sensitivityfun_age18<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age18<-sensitivityfun_age18(fpr)
plot(x=fpr,y=sensitivity_age,ylim = c(0,1),main="age1=25")
AUCage18<-round(sum(sensitivity_age18[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage18#######0.79

# sensitivityfun_age20<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 20,x12=20,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
#                              beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity_age20<-sensitivityfun_age20(fpr)
# plot(x=fpr,y=sensitivity_age,ylim = c(0,1),main="age1=25")
# AUCage20<-round(sum(sensitivity_age20[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUCage20#######0.78

sensitivityfun_age25<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age25<-sensitivityfun_age25(fpr)
plot(x=fpr,y=sensitivity_age25,ylim = c(0,1),main="age1=25")
AUCage25<-round(sum(sensitivity_age25[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage25#0.77

sensitivityfun_age40<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age40<-sensitivityfun_age40(fpr)
plot(x=fpr,y=sensitivity_age40,ylim = c(0,1),main="age1=25")
AUCage40<-round(sum(sensitivity_age40[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage40#######0.75

sensitivityfun_age60<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 60,x12=60,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age60<-sensitivityfun_age60(fpr)
plot(x=fpr,y=sensitivity_age60,ylim = c(0,1),main="age1=25")
AUCage60<-round(sum(sensitivity_age60[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage60#######0.71

rocdata_age<-data.frame(sensitivity_age18,sensitivity_age25,
                           sensitivity_age40,sensitivity_age60,sensitivity_pool,fpr)####omit age=20
rocdata_age_melt<-reshape2::melt(rocdata_age, id.var='fpr')
colnames(rocdata_age_melt)[2]<-"Age"
rocdata_age_melt$Age<- ifelse(rocdata_age_melt$Age=="sensitivity_age18","Age=18 AUC=0.79",
                                 ifelse(rocdata_age_melt$Age=="sensitivity_age25","Age=25 AUC=0.77",
                                        ifelse(rocdata_age_melt$Age=="sensitivity_age40","Age=40 AUC=0.75",
                                              ifelse(rocdata_age_melt$Age=="sensitivity_age60","Age=60 AUC=0.71",
                                                     "Pooled AUC=0.75") )))
rocdata_age_melt$point<-ifelse(rocdata_age_melt$fpr%in%star,rocdata_age_melt$value,NA)

ggplot(rocdata_age_melt, aes(x=fpr, y=value, type= Age,col=Age)) +
  labs(x="FPR", y="Sensitivity")+
  geom_line(aes(linetype=Age),size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Age),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Age-Adjusted ROC Curves ")+
  theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        legend.position = c(0.75,0.2),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 14,family="Arial"),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

######################roc of ethnicity 
sensitivityfun_ethnicity1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x21=0,x31=1,x32=1,beta_0 = coef_ethnicity[1],beta_L = coef_ethnicity[2],
                             beta_11 = 0,beta_L11 =0,
                             beta_21 = 0,beta_L21 = 0,beta_22 =0,
                             beta_31 = coef_ethnicity[3],beta_L31 =  coef_ethnicity[5],beta_32= coef_ethnicity[4],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_ethnicity1<-sensitivityfun_ethnicity1(fpr)
AUC_ethn1<-round(sum(sensitivity_ethnicity1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_ethn1#######is 0.77 when ethnicity=1 causian

sensitivityfun_ethnicity0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x21=0,x31=0,x32=1,beta_0 = coef_ethnicity[1],beta_L = coef_ethnicity[2],
                             beta_11 = 0,beta_L11 =0,
                             beta_21 = 0,beta_L21 = 0,beta_22 =0,
                             beta_31 = coef_ethnicity[3],beta_L31 =  coef_ethnicity[5],beta_32= coef_ethnicity[4],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_ethnicity0<-sensitivityfun_ethnicity0(fpr)
AUC_ethn0<-round(sum(sensitivity_ethnicity0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_ethn0#######is 0.62 when ethnicity=0 non caucasian

rocdata_ethnicity<-data.frame(sensitivity_ethnicity1,sensitivity_ethnicity0,sensitivity_pool,fpr)
rocdata_ethnicity_melt<-reshape2::melt(rocdata_ethnicity, id.var='fpr')
colnames(rocdata_ethnicity_melt)[2]<-"Ethnicity"
rocdata_ethnicity_melt$Ethnicity<- ifelse(rocdata_ethnicity_melt$Ethnicity=="sensitivity_ethnicity1","Caucasian AUC=0.77",
                                    ifelse(rocdata_ethnicity_melt$Ethnicity=="sensitivity_ethnicity0","Non caucasian AUC=0.62",
                                           "Pooled AUC=0.75"))
rocdata_ethnicity_melt$point<-ifelse(rocdata_ethnicity_melt$fpr%in%star,rocdata_ethnicity_melt$value,NA)
ggplot(rocdata_ethnicity_melt, aes(x=fpr, y=value, type= Ethnicity,col=Ethnicity)) +
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Ethnicity),size=1.2)+
  scale_linetype_manual(values=c("dotted","longdash","solid"))+
  geom_point(aes(y=point,shape=Ethnicity),size=3)+
  scale_shape_manual(values=c(15,16,NA))+
  ggtitle("Ethnicity-Adjusted ROC Curves")+
  theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        legend.position = c(0.7,0.2),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 14,family="Arial"),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#####for gender and age
# sensitivity_gender_age<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = mean(workfile_4_final$age),x12=mean(workfile_4_final$max_id_age),x21=1,x22=0,x31=0,beta_0 = coef_age_gender[1],beta_L = coef_age_gender[2],beta_11 = coef_age_gender[3],beta_L11 =0,
#                              beta_21 = coef_age_gender[5],beta_L21 =  coef_age_gender[7],beta_12 =  coef_age_gender[4],beta_22=coef_age_gender[6],sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity<-sensitivity_gender_age(fpr)
# plot(x=fpr,y=sensitivity,ylim = c(0,1))
# AUC<-round(sum(sensitivity[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC
# 
# sensitivity_gender_age_2<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = mean(workfile_4_final$age),x12=mean(workfile_4_final$max_id_age),x21=0,x22=1,x31=0,beta_0 = coef_age_gender[1],beta_L = coef_age_gender[2],beta_11 = coef_age_gender[3],beta_L11 =0,
#                              beta_21 = coef_age_gender[5],beta_L21 =  coef_age_gender[7],beta_12 =  coef_age_gender[4],beta_22=coef_age_gender[6],sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity_2<-sensitivity_gender_age_2(fpr)
# plot(x=fpr,y=sensitivity_2,ylim = c(0,1))
# AUC<-round(sum(sensitivity_2[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC
# 
# ####no interaction
# sensitivity_s<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = mean(workfile_4_final$age),x12=mean(workfile_4_final$max_id_age),x21=0,x22=1,x31=0,beta_0 = coef[1],beta_L = coef[2],beta_11 = coef[3],beta_L11 =0,
#                              beta_21 = coef[5],beta_L21 = 0,beta_12 =  coef[4],beta_22=coef[6],sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity_s_nointer<-sensitivity_s(fpr)
# plot(x=fpr,y=sensitivity_s_nointer,ylim = c(0,1))
# AUC<-round(sum(sensitivity_s_nointer[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC

###both interaction  0.83(25,25,1,0) (25,25,1,1) 0.52(25,25,0,0)(25,25,0,1) 0.55(10,25,0,1)(10,10,0,1)
sensitivityfun_25_0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=0,x22=1,x31=0,
                             beta_0 = coef_age_gender_full[1],beta_L = coef_age_gender_full[2],
                             beta_11 = coef_age_gender_full[3],beta_L11 =coef_age_gender_full[7],
                             beta_21 = coef_age_gender_full[5],beta_L21 =  coef_age_gender_full[8],
                             beta_12 =  coef_age_gender_full[4],beta_22=coef_age_gender_full[6],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_25_0<-sensitivityfun_25_0(fpr)
#plot(x=fpr,y=sensitivity_full,ylim = c(0,1),main = "age1=25, gender1=female")
AUC_25_0<-round(sum(sensitivity_25_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_0##0.52

sensitivityfun_25_1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=1,x22=1,x31=0,
                             beta_0 = coef_age_gender_full[1],beta_L = coef_age_gender_full[2],
                             beta_11 = coef_age_gender_full[3],beta_L11 =coef_age_gender_full[7],
                             beta_21 = coef_age_gender_full[5],beta_L21 =  coef_age_gender_full[8],
                             beta_12 =  coef_age_gender_full[4],beta_22=coef_age_gender_full[6],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_25_1<-sensitivityfun_25_1(fpr)
#plot(x=fpr,y=sensitivity_full,ylim = c(0,1),main = "age1=25, gender1=female")
AUC_25_1<-round(sum(sensitivity_25_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_1##0.83

sensitivityfun_40_0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x22=1,x31=0,
                             beta_0 = coef_age_gender_full[1],beta_L = coef_age_gender_full[2],
                             beta_11 = coef_age_gender_full[3],beta_L11 =coef_age_gender_full[7],
                             beta_21 = coef_age_gender_full[5],beta_L21 =  coef_age_gender_full[8],
                             beta_12 =  coef_age_gender_full[4],beta_22=coef_age_gender_full[6],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_40_0<-sensitivityfun_40_0(fpr)
#plot(x=fpr,y=sensitivity_full,ylim = c(0,1),main = "age1=25, gender1=female")
AUC_40_0<-round(sum(sensitivity_40_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_0##0.5

sensitivityfun_40_1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=1,x22=1,x31=0,
                             beta_0 = coef_age_gender_full[1],beta_L = coef_age_gender_full[2],
                             beta_11 = coef_age_gender_full[3],beta_L11 =coef_age_gender_full[7],
                             beta_21 = coef_age_gender_full[5],beta_L21 =  coef_age_gender_full[8],
                             beta_12 =  coef_age_gender_full[4],beta_22=coef_age_gender_full[6],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_40_1<-sensitivityfun_40_1(fpr)
#plot(x=fpr,y=sensitivity_full,ylim = c(0,1),main = "age1=25, gender1=female")
AUC_40_1<-round(sum(sensitivity_40_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_1##0.81

# sensitivityfun_60_0<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 60,x12=60,x21=0,x22=1,x31=0,
#                              beta_0 = coef_age_gender_full[1],beta_L = coef_age_gender_full[2],
#                              beta_11 = coef_age_gender_full[3],beta_L11 =coef_age_gender_full[7],
#                              beta_21 = coef_age_gender_full[5],beta_L21 =  coef_age_gender_full[8],
#                              beta_12 =  coef_age_gender_full[4],beta_22=coef_age_gender_full[6],
#                              sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity_60_0<-sensitivityfun_60_0(fpr)
# plot(x=fpr,y=sensitivity_full,ylim = c(0,1),main = "age1=25, gender1=female")
# AUC<-round(sum(sensitivity_60_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC##0.46

# sensitivityfun_60_1<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 60,x12=60,x21=1,x22=1,x31=0,
#                              beta_0 = coef_age_gender_full[1],beta_L = coef_age_gender_full[2],
#                              beta_11 = coef_age_gender_full[3],beta_L11 =coef_age_gender_full[7],
#                              beta_21 = coef_age_gender_full[5],beta_L21 =  coef_age_gender_full[8],
#                              beta_12 =  coef_age_gender_full[4],beta_22=coef_age_gender_full[6],
#                              sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity_60_1<-sensitivityfun_60_1(fpr)
# plot(x=fpr,y=sensitivity_full,ylim = c(0,1),main = "age1=25, gender1=female")
# AUC<-round(sum(sensitivity_60_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC##0.78

rocdata_age_gender<-data.frame(sensitivity_40_1,sensitivity_40_0,
                               sensitivity_25_1,sensitivity_25_0,
                               sensitivity_pool,fpr)
rocdata_age_gender_melt<-reshape2::melt(rocdata_age_gender, id.var='fpr')
colnames(rocdata_age_gender_melt)[2]<-"Demographics"
rocdata_age_gender_melt$Demographics<- ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_40_1","Male Age=40 AUC=0.81",
                              ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_40_0","Female Age=40 AUC=0.5",
                                     ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_25_1","Male Age=25 AUC=0.83",
                                            ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_25_0","Female Age=25 AUC=0.52",
                                                   "Pooled AUC=0.75")
                                                   )))
rocdata_age_gender_melt$point<-ifelse(rocdata_age_gender_melt$fpr%in%star,rocdata_age_gender_melt$value,NA)
ggplot(rocdata_age_gender_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Gender and Age Ajusted ROC Curves")+
      theme(legend.background = element_rect(fill="transparent",size=10, linetype="solid"),
            text=element_text(size=14, family="Arial"),
            plot.title = element_text(size=14, family="Arial",hjust = 0.5),
      legend.position = c(0.7,0.2),
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 14,family="Arial"),
      axis.text = element_text(size=14, family="Arial"),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
#### as age increase AUC decrease, male's AUC> female's AUC quite a lot

################################################for age genderand ethnicity
sensitivityfun_25_0_0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=0,x22=1,x31=0,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_25_0_0<-sensitivityfun_25_0_0(fpr)
AUC_25_0_0<-round(sum(sensitivity_25_0_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_0_0##0.5(lower than only age and gender)

sensitivityfun_25_1_0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=1,x22=1,x31=0,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_25_1_0<-sensitivityfun_25_1_0(fpr)
AUC_25_1_0<-round(sum(sensitivity_25_1_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_1_0##0.81(lower than only age and gender)

sensitivityfun_40_0_0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=25,x21=0,x22=1,x31=0,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_40_0_0<-sensitivityfun_40_0_0(fpr)
AUC_40_0_0<-round(sum(sensitivity_40_0_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_0_0##0.47(lower than only age and gender)

sensitivityfun_40_1_0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=25,x21=1,x22=1,x31=0,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_40_1_0<-sensitivityfun_40_1_0(fpr)
AUC_40_1_0<-round(sum(sensitivity_40_1_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_1_0##0.79(lower than only age and gender)

sensitivityfun_25_0_1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=0,x22=1,x31=1,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_25_0_1<-sensitivityfun_25_0_1(fpr)
AUC_25_0_1<-round(sum(sensitivity_25_0_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_0_1##0.53(a little larger than only age and gender)

sensitivityfun_25_1_1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=1,x22=1,x31=1,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_25_1_1<-sensitivityfun_25_1_1(fpr)
AUC_25_1_1<-round(sum(sensitivity_25_1_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_1_1##0.83(same with only age and gender)

sensitivityfun_40_0_1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=25,x21=0,x22=1,x31=1,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_40_0_1<-sensitivityfun_40_0_1(fpr)
AUC_40_0_1<-round(sum(sensitivity_40_0_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_0_1##0.51(a bit larger than only age and gender)

sensitivityfun_40_1_1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=25,x21=1,x22=1,x31=1,x32 = 0,
                             beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
                             beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
                             beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
                             beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
                             beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_40_1_1<-sensitivityfun_40_1_1(fpr)
AUC_40_1_1<-round(sum(sensitivity_40_1_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_1_1##0.81(a little larfer than only age and gender)

# 
# 
# sensitivity_25_0_888<-roc_fn(p=fpr,x11=40,x12=25,x21=1,x22=0,x31=1,x32=0,
#                              beta_0 = coef_model_4[1],beta_L = coef_model_4[2],
#                              beta_11 = coef_model_4[3],beta_L11 =coef_model_4[9],
#                              beta_21 = coef_model_4[5],beta_L21 =  coef_model_4[10],
#                              beta_31 = coef_model_4[7],beta_L31 =  coef_model_4[11],
#                              beta_12 =  coef_model_4[4],beta_22=coef_model_4[6],beta_32=coef_model_4[8],
#                              sigma0 = sigma_0,sigma1 = sigma_1)
# AUC<-round(sum(sensitivity_25_0_888[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC

#######roc curve for ethnity=noncaucasian
rocdata_3demographics0<-data.frame(sensitivity_40_1_0,sensitivity_40_0_0,
                               sensitivity_25_1_0,sensitivity_25_0_0,
                               sensitivity_pool,fpr)
rocdata_3demographics0_melt<-reshape2::melt(rocdata_3demographics0, id.var='fpr')
colnames(rocdata_3demographics0_melt)[2]<-"Demographics"
rocdata_3demographics0_melt$Demographics<- ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_40_1_0","Male Non-caucasian Age=40 AUC=0.79",
                                           ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_40_0_0","Female Non-caucasian Age=40 AUC=0.47",
                                           ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_25_1_0","Male Non-caucasian Age=25 AUC=0.81",
                                           ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_25_0_0","Female Non-caucasian Age=25 AUC=0.5",
                                           "Pooled AUC=0.75"))))
rocdata_3demographics0_melt$point<-ifelse(rocdata_3demographics0_melt$fpr%in%star,rocdata_3demographics0_melt$value,NA)
ggplot(rocdata_3demographics0_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Gender, Age and Ethnicity Ajusted ROC Curves",subtitle = "Ethnicity=Non-caucasian")+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        legend.position = c(0.68,0.18),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#######roc curve for ethnity=caucasian
rocdata_3demographics1<-data.frame(sensitivity_40_1_1,sensitivity_40_0_1,
                                   sensitivity_25_1_1,sensitivity_25_0_1,
                                   sensitivity_pool,fpr)
rocdata_3demographics1_melt<-reshape2::melt(rocdata_3demographics1, id.var='fpr')
colnames(rocdata_3demographics1_melt)[2]<-"Demographics"
rocdata_3demographics1_melt$Demographics<- ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_40_1_1","Male Caucasian Age=40 AUC=0.81",
                                                  ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_40_0_1","Female Caucasian Age=40 AUC=0.51",
                                                         ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_25_1_1","Male Caucasian Age=25 AUC=0.83",
                                                                ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_25_0_1","Female Caucasian Age=25 AUC=0.53",
                                                                       "Pooled AUC=0.75"))))
rocdata_3demographics1_melt$point<-ifelse(rocdata_3demographics1_melt$fpr%in%star,rocdata_3demographics1_melt$value,NA)
ggplot(rocdata_3demographics1_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.8)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Gender, Age and Ethnicity Ajusted ROC Curves",subtitle = "Ethnicity=Caucasian")+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        legend.position = c(0.68,0.18),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# rocdata_3demographics<-data.frame(sensitivity_40_1_0,sensitivity_40_0_0,
#                                sensitivity_25_1_0,sensitivity_25_0_0,
#                                sensitivity_40_1_1,sensitivity_40_0_1,
#                                sensitivity_25_1_1,sensitivity_25_0_1,
#                                sensitivity_pool,fpr)
# rocdata_3demographics_melt<-reshape2::melt(rocdata_3demographics, id.var='fpr')
                                                                  
# colnames(rocdata_3demographics_melt)[2]<-"Demographics"
# rocdata_3demographics_melt$Demographics<- ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_40_1_0","Male Non-caucasian Age=40 AUC=0.79",
#                                           ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_40_0_0","Female Non-caucasian Age=40 AUC=0.47",
#                                          ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_25_1_0","Male Non-caucasian Age=25 AUC=0.81",
#                                          ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_25_0_0","Female Non-caucasian Age=25 AUC=0.5",
#                                          ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_40_1_1","Male Caucasian Age=40 AUC=0.81",
#                                          ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_40_0_1","Female Caucasian Age=40 AUC=0.51",
#                                          ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_25_1_1","Male Caucasian Age=25 AUC=0.83",
#                                          ifelse(rocdata_3demographics_melt$Demographics=="sensitivity_25_0_1","Female Caucasian Age=25 AUC=0.53",
#                                                                    "Pooled AUC=0.75")
#                                                      )))))))
# ggplot(rocdata_3demographics_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
#   labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.8)+
#   #scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
#   theme(legend.background = element_rect(fill="transparent",size=10, linetype="solid"),
#         text=element_text(size=14, family="Arial"),
#         legend.position = c(0.8,0.3),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 8,family="Arial"),
#         axis.text = element_text(size=14, family="Arial"),
#         plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

####################################### save plot ########################################### 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to=".")






####################################### Variance Part ###########################################

###bootstrap
iterations <- 5000
df_boot <- list()# each element of df_boot is a sampled data from the dataset
mu_boot <- list()
sigma_0_boot<- c()
sigma_1_boot<- c()
for (i in 1:iterations){
  set.seed(224+i)
  v <- sample(1:nrow(workfile_4_final), size=nrow(workfile_4_final), replace=TRUE)
  df_boot[[i]] <- workfile_4_final[v,]
  mu_boot[[i]] <- ddply(df_boot[[i]], "Label", summarise, grp.mean=mean(max_score))
  sigma_0_boot[i] <- sd(subset(df_boot[[i]],match=='0')$max_score)
  sigma_1_boot[i] <- sd(subset(df_boot[[i]],match=='1')$max_score)
}


##############################pooled ROC ################################################ 
sensitivityfun_pool_boot<- matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    sensitivityfun_pool_boot[i,j] <- roc_fn(p=fpr[j],x11 = 0,x12=0,x21=0,x22=0,x31=0,
                                            beta_0 =mu_boot[[i]]$grp.mean[1],
                                            beta_L =mu_boot[[i]]$grp.mean[1]-mu_boot[[i]]$grp.mean[2],
                                            beta_11 = 0,beta_L11 =0, beta_21 = 0,beta_L21 = 0,beta_22 = 0,
                                            sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
  }
}

AUC_pool_boot <- c()  
for (i in 1:iterations){
  AUC_pool_boot[i]<- round(sum(sensitivityfun_pool_boot[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}

sd_AUCpool<- sd(AUC_pool_boot)#[1] 0.05817698 
AUCpool_CI <- c(AUCpool-qnorm(0.975)*sd_AUCpool,AUCpool+qnorm(0.975)*sd_AUCpool)#[1] 0.6359752 0.8640248 ## use delong's method; shiny app

# delong
# AUC : 0.7070025
# var: 0.006771471
# sd: 0.08228895
# CI : 0.5457192 0.8682859

# ROC curve confidence intervel
sd_fpr <- apply(sensitivityfun_pool_boot, 2,sd)
tpr_pool <- data.frame(fpr=fpr,FPR=sensitivity_pool, FPR_L=sensitivity_pool-qnorm(0.975)*sd_fpr,FPR_H=sensitivity_pool+qnorm(0.975)*sd_fpr)

mycol <- rgb(0, 0, 255, max = 255, alpha = 50, names = "lightskyblue")
# plot(fpr,tpr_pool$FPR_L,type="l",xlab="FPR",ylab="Sensitivity",main="Pooled ROC Curve Confidence Intervel",col="white")
# points(fpr,tpr_pool$FPR_H,type="l",col="white")
# polygon(c(fpr,rev(fpr)),c(tpr_pool$FPR_H,rev(tpr_pool$FPR_L)),col=mycol)
# points(fpr,tpr_pool$FPR,type="l",col="blue")

plot(fpr,tpr_pool$FPR,xlab="FPR",ylab="Sensitivity",main="Pooled ROC Curve Confidence Intervel"
     ,ylim = c(0,1),type="l",col="blue")
polygon(c(fpr,rev(fpr)),c(tpr_pool$FPR_H,rev(tpr_pool$FPR_L)),col=mycol,border = mycol)
  
############################## ROC of gender: model_gender_output4 ################################################ 
coef_gender_boot<- list()
for (i in 1:iterations){
  coef_gender_boot[[i]] <-lm(max_score~match+gender+max_id_gender+gender:match, data=df_boot[[i]])$coefficients
}

sensitivityfun_gender0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_gender_boot[[i]][5])){sensitivityfun_gender0_boot[i,] <- NA}
    else{
    sensitivityfun_gender0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 0,x12=0,x21=0,x22=1,x31=0,
                                               beta_0 = coef_gender_boot[[i]][1],beta_L = coef_gender_boot[[i]][2],
                                               beta_11 = 0,beta_L11 =0,beta_21 = coef_gender_boot[[i]][3],
                                               beta_L21 = coef_gender_boot[[i]][5],beta_22 = coef_gender_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_gender0_boot2 <- na.omit(sensitivityfun_gender0_boot)# dim [1] 4918 1000
AUC_gender0_boot<- c()
for (i in 1:nrow(sensitivityfun_gender0_boot2)){
  AUC_gender0_boot[i]<-round(sum(sensitivityfun_gender0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCgender0<- sd(AUC_gender0_boot)#0.07884812
AUCgender0_CI <- c(AUCgender0-qnorm(0.975)*sd_AUCgender0,AUCgender0+qnorm(0.975)*sd_AUCgender0)#[1] 0.3454605 0.6545395

sensitivityfun_gender1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_gender_boot[[i]][5])){sensitivityfun_gender1_boot[i,] <- NA}
    else{
    sensitivityfun_gender1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 0,x12=0,x21=1,x22=1,x31=0,
                                               beta_0 = coef_gender_boot[[i]][1],beta_L = coef_gender_boot[[i]][2],
                                               beta_11 = 0,beta_L11 =0,beta_21 = coef_gender_boot[[i]][3],
                                               beta_L21 = coef_gender_boot[[i]][5],beta_22 = coef_gender_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameterss
sensitivityfun_gender1_boot2 <- na.omit(sensitivityfun_gender1_boot)
AUC_gender1_boot<- c()
for (i in 1:nrow(sensitivityfun_gender1_boot2)){
  AUC_gender1_boot[i]<-round(sum(sensitivityfun_gender1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCgender1<- sd(AUC_gender1_boot)#0.05724402
AUCgender1_CI <- c(AUCgender1-qnorm(0.975)*sd_AUCgender1,AUCgender1+qnorm(0.975)*sd_AUCgender1)#[1]0.6978038 0.9221962

# ROC curve confidence intervel
sd0_fpr <- apply(sensitivityfun_gender0_boot2, 2,sd);
sd1_fpr <- apply(sensitivityfun_gender1_boot2, 2,sd)
tpr_gender <- data.frame(fpr=fpr,FPR0=sensitivity_gender0, FPR0_L=sensitivity_gender0-qnorm(0.975)*sd0_fpr,
                         FPR0_H=sensitivity_gender0+qnorm(0.975)*sd0_fpr,FPR1=sensitivity_gender1, 
                         FPR1_L=sensitivity_gender1-qnorm(0.975)*sd1_fpr,FPR1_H=sensitivity_gender1+qnorm(0.975)*sd1_fpr)


# plot(fpr,tpr_gender$FPR0_L,type="l",xlab="FPR",ylab="Sensitivity",main="ROC Curve Confidence Intervel \n Gender= Female",ylim = c(0,1))
# points(fpr,tpr_gender$FPR0_H,type="l",col="black")
# polygon(c(fpr,rev(fpr)),c(tpr_gender$FPR0_H,rev(tpr_gender$FPR0_L)),col=mycol)
# points(fpr,tpr_gender$FPR0,type="l",col="blue")
# 
# plot(fpr,tpr_gender$FPR1_L,type="l",xlab="FPR",ylab="Sensitivity",main="ROC Curve Confidence Intervel \n Gender= Male",ylim = c(0,1))
# points(fpr,tpr_gender$FPR1_H,type="l",col="black")
# polygon(c(fpr,rev(fpr)),c(tpr_gender$FPR1_H,rev(tpr_gender$FPR1_L)),col=mycol)
# points(fpr,tpr_gender$FPR1,type="l",col="blue")


plot(fpr,tpr_gender$FPR0,xlab="FPR",ylab="Sensitivity",main="ROC Curve Confidence Intervel \n Gender= Female"
     ,ylim = c(0,1),type="l",col="blue")
polygon(c(fpr,rev(fpr)),c(tpr_gender$FPR0_H,rev(tpr_gender$FPR0_L)),col=mycol,border = mycol)

plot(fpr,tpr_gender$FPR1,xlab="FPR",ylab="Sensitivity",main="ROC Curve Confidence Intervel \n Gender= Male"
     ,ylim = c(0,1),type="l",col="blue")
polygon(c(fpr,rev(fpr)),c(tpr_gender$FPR1_H,rev(tpr_gender$FPR1_L)),col=mycol,border = mycol)

############################## ROC of age: model_age_output4 ################################################ 
#### age  18 25 40 60 
coef_age_boot<- list()
for (i in 1:iterations){
  coef_age_boot[[i]] <-lm(max_score~match+age+max_id_age+age:match, data=df_boot[[i]])$coefficients
}

### age  18
sensitivityfun_age18_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_age_boot[[i]][5])){sensitivityfun_age18_boot[i,] <- NA}
    else{
      sensitivityfun_age18_boot[i,j] <- roc_fn(p=fpr[j],x11 = 18,x12=18,x21=0,x22=1,x31=0,
                                                 beta_0 = coef_age_boot[[i]][1],beta_L = coef_age_boot[[i]][2],
                                                 beta_11 =coef_age_boot[[i]][3],beta_L11 =coef_age_boot[[i]][5],beta_21 = 0,
                                                 beta_L21 = 0,beta_22 = coef_age_boot[[i]][4],
                                                 sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_age18_boot2 <- na.omit(sensitivityfun_age18_boot)# dim [1] 
AUC_age18_boot<- c()
for (i in 1:nrow(sensitivityfun_age18_boot2)){
  AUC_age18_boot[i]<-round(sum(sensitivityfun_age18_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCage18<- sd(AUC_age18_boot)#0.0927852
AUCage18_CI <- c(AUCage18-qnorm(0.975)*sd_AUCage18,AUCage18+qnorm(0.975)*sd_AUCage18)#[1] 0.6081443 0.9718557

### age  25
sensitivityfun_age25_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_age_boot[[i]][5])){sensitivityfun_age25_boot[i,] <- NA}
    else{
      sensitivityfun_age25_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=0,x22=1,x31=0,
                                               beta_0 = coef_age_boot[[i]][1],beta_L = coef_age_boot[[i]][2],
                                               beta_11 =coef_age_boot[[i]][3],beta_L11 =coef_age_boot[[i]][5],beta_21 = 0,
                                               beta_L21 = 0,beta_22 = coef_age_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_age25_boot2 <- na.omit(sensitivityfun_age25_boot)# dim [1] 
AUC_age25_boot<- c()
for (i in 1:nrow(sensitivityfun_age25_boot2)){
  AUC_age25_boot[i]<-round(sum(sensitivityfun_age25_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCage25<- sd(AUC_age25_boot)#0.07016561
AUCage25_CI <- c(AUCage25-qnorm(0.975)*sd_AUCage25,AUCage25+qnorm(0.975)*sd_AUCage25)#[1] 0.6324779 0.9075221

### age  40
sensitivityfun_age40_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_age_boot[[i]][5])){sensitivityfun_age40_boot[i,] <- NA}
    else{
      sensitivityfun_age40_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=0,x22=1,x31=0,
                                               beta_0 = coef_age_boot[[i]][1],beta_L = coef_age_boot[[i]][2],
                                               beta_11 =coef_age_boot[[i]][3],beta_L11 =coef_age_boot[[i]][5],beta_21 = 0,
                                               beta_L21 = 0,beta_22 = coef_age_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_age40_boot2 <- na.omit(sensitivityfun_age40_boot)# dim [1] 
AUC_age40_boot<- c()
for (i in 1:nrow(sensitivityfun_age40_boot2)){
  AUC_age40_boot[i]<-round(sum(sensitivityfun_age40_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCage40<- sd(AUC_age40_boot)#0.06740006
AUCage40_CI <- c(AUCage40-qnorm(0.975)*sd_AUCage40,AUCage40+qnorm(0.975)*sd_AUCage40)#[1]  0.6178983 0.8821017

### age  60
sensitivityfun_age60_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_age_boot[[i]][5])){sensitivityfun_age60_boot[i,] <- NA}
    else{
      sensitivityfun_age60_boot[i,j] <- roc_fn(p=fpr[j],x11 = 60,x12=60,x21=0,x22=1,x31=0,
                                               beta_0 = coef_age_boot[[i]][1],beta_L = coef_age_boot[[i]][2],
                                               beta_11 =coef_age_boot[[i]][3],beta_L11 =coef_age_boot[[i]][5],beta_21 = 0,
                                               beta_L21 = 0,beta_22 = coef_age_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_age60_boot2 <- na.omit(sensitivityfun_age60_boot)# dim [1] 
AUC_age60_boot<- c()
for (i in 1:nrow(sensitivityfun_age60_boot2)){
  AUC_age60_boot[i]<-round(sum(sensitivityfun_age60_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCage60<- sd(AUC_age60_boot)#0.1430881
AUCage60_CI <- c(AUCage60-qnorm(0.975)*sd_AUCage60,AUCage60+qnorm(0.975)*sd_AUCage60)#[1] 0.4295525 0.9904475



############################## ROC of ethnicity: model_ethnicity_output4 ################################################ 
coef_ethn_boot<- list()
for (i in 1:iterations){
  coef_ethn_boot[[i]] <-lm(max_score~match+ethnicity+max_id_ethnicity+ethnicity:match, data=df_boot[[i]])$coefficients
}

sensitivityfun_ethn1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_ethn0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_ethn_boot[[i]][5])){
      sensitivityfun_ethn1_boot[i,] <- NA ;sensitivityfun_ethn0_boot[i,] <- NA}
    else{
      sensitivityfun_ethn1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 0,x21=0,x31=1,x32=1,
                                               beta_0 = coef_ethn_boot[[i]][1],beta_L = coef_ethn_boot[[i]][2],
                                               beta_11 =0,beta_L11 =0,beta_21 = 0,beta_L21 = 0,beta_22 = 0,
                                               beta_31 = coef_ethn_boot[[i]][3],beta_L31 = coef_ethn_boot[[i]][5],beta_32= coef_ethn_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_ethn0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 0,x21=0,x31=0,x32=1,
                                               beta_0 = coef_ethn_boot[[i]][1],beta_L = coef_ethn_boot[[i]][2],
                                               beta_11 =0,beta_L11 =0,beta_21 = 0,beta_L21 = 0,beta_22 = 0,
                                               beta_31 = coef_ethn_boot[[i]][3],beta_L31 = coef_ethn_boot[[i]][5],beta_32= coef_ethn_boot[[i]][4],
                                               sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_ethn1_boot2 <- na.omit(sensitivityfun_ethn1_boot)# dim [1] 
sensitivityfun_ethn0_boot2 <- na.omit(sensitivityfun_ethn0_boot)# dim [1] 
AUC_ethn1_boot<- c()
AUC_ethn0_boot<- c()
for (i in 1:nrow(sensitivityfun_ethn1_boot2)){
  AUC_ethn1_boot[i]<-round(sum(sensitivityfun_ethn1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
  AUC_ethn0_boot[i]<-round(sum(sensitivityfun_ethn0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUC_ethn1<- sd(AUC_ethn1_boot);sd_AUC_ethn0<- sd(AUC_ethn0_boot) #0.06138466; 0.1573005
AUCethn1_CI <- c(AUC_ethn1-qnorm(0.975)*sd_AUC_ethn1,AUC_ethn1+qnorm(0.975)*sd_AUC_ethn1)#[1] 0.6496883 0.8903117
AUCethn0_CI <- c(AUC_ethn0-qnorm(0.975)*sd_AUC_ethn0,AUC_ethn0+qnorm(0.975)*sd_AUC_ethn0)#[1] 0.3116967 0.9283033




############################## ROC of age&gender: model_full ################################################ 
coef_age_gender_boot<- list()
for (i in 1:iterations){
  coef_age_gender_boot[[i]] <-lm(max_score~match+age+max_id_age+gender+max_id_gender+age:match+gender:match, data=df_boot[[i]])$coefficients
}

sensitivityfun_25_0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_25_1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_40_0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_40_1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_age_gender_boot[[i]][7])|is.na(coef_age_gender_boot[[i]][8])){
      sensitivityfun_25_0_boot[i,] <- NA ;sensitivityfun_25_1_boot[i,] <- NA;
      sensitivityfun_40_0_boot[i,] <- NA ;sensitivityfun_40_1_boot[i,] <- NA}
    else{
      sensitivityfun_25_0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=0,x22=0,x31=0,
                                              beta_0 = coef_age_gender_boot[[i]][1],beta_L = coef_age_gender_boot[[i]][2],
                                              beta_11 = coef_age_gender_boot[[i]][3],beta_L11 =coef_age_gender_boot[[i]][7],
                                              beta_21 = coef_age_gender_boot[[i]][5],beta_L21 =  coef_age_gender_boot[[i]][8],
                                              beta_12 =  coef_age_gender_boot[[i]][4],beta_22=coef_age_gender_boot[[i]][6],
                                              sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_25_1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=1,x22=1,x31=0,
                                              beta_0 = coef_age_gender_boot[[i]][1],beta_L = coef_age_gender_boot[[i]][2],
                                              beta_11 = coef_age_gender_boot[[i]][3],beta_L11 =coef_age_gender_boot[[i]][7],
                                              beta_21 = coef_age_gender_boot[[i]][5],beta_L21 =  coef_age_gender_boot[[i]][8],
                                              beta_12 =  coef_age_gender_boot[[i]][4],beta_22=coef_age_gender_boot[[i]][6],
                                              sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_40_0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=0,x22=0,x31=0,
                                              beta_0 = coef_age_gender_boot[[i]][1],beta_L = coef_age_gender_boot[[i]][2],
                                              beta_11 = coef_age_gender_boot[[i]][3],beta_L11 =coef_age_gender_boot[[i]][7],
                                              beta_21 = coef_age_gender_boot[[i]][5],beta_L21 =  coef_age_gender_boot[[i]][8],
                                              beta_12 =  coef_age_gender_boot[[i]][4],beta_22=coef_age_gender_boot[[i]][6],
                                              sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_40_1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=1,x22=1,x31=0,
                                              beta_0 = coef_age_gender_boot[[i]][1],beta_L = coef_age_gender_boot[[i]][2],
                                              beta_11 = coef_age_gender_boot[[i]][3],beta_L11 =coef_age_gender_boot[[i]][7],
                                              beta_21 = coef_age_gender_boot[[i]][5],beta_L21 =  coef_age_gender_boot[[i]][8],
                                              beta_12 =  coef_age_gender_boot[[i]][4],beta_22=coef_age_gender_boot[[i]][6],
                                              sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_25_0_boot2<-na.omit(sensitivityfun_25_0_boot)
sensitivityfun_25_1_boot2<-na.omit(sensitivityfun_25_1_boot)
sensitivityfun_40_0_boot2<-na.omit(sensitivityfun_40_0_boot)
sensitivityfun_40_1_boot2<-na.omit(sensitivityfun_40_1_boot)
AUC_25_0_boot<- c();AUC_25_1_boot<- c();AUC_40_0_boot<- c();AUC_40_1_boot<- c();
for (i in 1:nrow(sensitivityfun_25_0_boot2)){
  AUC_25_0_boot[i]<-round(sum(sensitivityfun_25_0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_25_1_boot[i]<-round(sum(sensitivityfun_25_1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_40_0_boot[i]<-round(sum(sensitivityfun_40_0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_40_1_boot[i]<-round(sum(sensitivityfun_40_1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUC_25_0<- sd(AUC_25_0_boot);sd_AUC_25_1<- sd(AUC_25_1_boot);sd_AUC_40_0<- sd(AUC_40_0_boot);sd_AUC_40_1<- sd(AUC_40_1_boot); 
#0.1029855;  0.06803256; 0.102336; 0.0629028
AUC_25_0_CI <- c(AUC_25_0-qnorm(0.975)*sd_AUC_25_0,AUC_25_0+qnorm(0.975)*sd_AUC_25_0)#[1] 0.3181522 0.7218478
AUC_25_1_CI <- c(AUC_25_1-qnorm(0.975)*sd_AUC_25_1,AUC_25_1+qnorm(0.975)*sd_AUC_25_1)#[1] 0.6966586 0.9633414
AUC_40_0_CI <- c(AUC_40_0-qnorm(0.975)*sd_AUC_40_0,AUC_40_0+qnorm(0.975)*sd_AUC_40_0)#[1]  0.299425 0.700575
AUC_40_1_CI <- c(AUC_40_1-qnorm(0.975)*sd_AUC_40_1,AUC_40_1+qnorm(0.975)*sd_AUC_40_1)#[1] 0.6867128 0.9332872



############################## ROC of age&gender&ethnicity: model_4 ################################################ 
coef_age_gender_ethn_boot<- list()
for (i in 1:iterations){
  coef_age_gender_ethn_boot[[i]] <-lm(max_score~match+age+max_id_age+gender+max_id_gender+ethnicity+max_id_ethnicity+
                                        age:match+gender:match+ethnicity:match, data=df_boot[[i]])$coefficients
}
sensitivityfun_25_0_0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_25_1_0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_40_0_0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_40_1_0_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_25_0_1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_25_1_1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_40_0_1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
sensitivityfun_40_1_1_boot<-matrix(  , nrow = iterations, ncol = length(fpr))
for (i in 1:iterations){
  for (j in 1:length(fpr)){
    if (is.na(coef_age_gender_ethn_boot[[i]][9])|is.na(coef_age_gender_ethn_boot[[i]][10])|is.na(coef_age_gender_ethn_boot[[i]][11])){
      sensitivityfun_25_0_0_boot[i,] <- NA ;sensitivityfun_25_1_0_boot[i,] <- NA;
      sensitivityfun_40_0_0_boot[i,] <- NA ;sensitivityfun_40_1_0_boot[i,] <- NA;
      sensitivityfun_25_0_1_boot[i,] <- NA ;sensitivityfun_25_1_1_boot[i,] <- NA;
      sensitivityfun_40_0_1_boot[i,] <- NA ;sensitivityfun_40_1_1_boot[i,] <- NA}
    else{
      sensitivityfun_25_0_0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=0,x22=0,x31=0,x32 = 0,
                                              beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                              beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                              beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                              beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                              beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                              beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_25_1_0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=1,x22=1,x31=0,x32 = 0,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_40_0_0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=0,x22=0,x31=0,x32 = 0,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_40_1_0_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=1,x22=1,x31=0,x32 = 0,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_25_0_1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=0,x22=0,x31=1,x32 = 1,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_25_1_1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 25,x12=25,x21=1,x22=1,x31=1,x32 = 1,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_40_0_1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=0,x22=0,x31=1,x32 = 1,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i]);
      sensitivityfun_40_1_1_boot[i,j] <- roc_fn(p=fpr[j],x11 = 40,x12=40,x21=1,x22=1,x31=1,x32 = 1,
                                                beta_0 = coef_age_gender_ethn_boot[[i]][1],beta_L = coef_age_gender_ethn_boot[[i]][2],
                                                beta_11 = coef_age_gender_ethn_boot[[i]][3],beta_L11 =coef_age_gender_ethn_boot[[i]][9],
                                                beta_21 = coef_age_gender_ethn_boot[[i]][5],beta_L21 =  coef_age_gender_ethn_boot[[i]][10],
                                                beta_31 = coef_age_gender_ethn_boot[[i]][7],beta_L31 =  coef_age_gender_ethn_boot[[i]][11],
                                                beta_12 =  coef_age_gender_ethn_boot[[i]][4],beta_22=coef_age_gender_ethn_boot[[i]][6],
                                                beta_32=coef_age_gender_ethn_boot[[i]][8],sigma0 = sigma_0_boot[i],sigma1 = sigma_1_boot[i])
      
    }
  }
}
# delete those iterations with NA parameters
sensitivityfun_25_0_0_boot2<-na.omit(sensitivityfun_25_0_0_boot);sensitivityfun_25_1_0_boot2<-na.omit(sensitivityfun_25_1_0_boot);
sensitivityfun_40_0_0_boot2<-na.omit(sensitivityfun_40_0_0_boot);sensitivityfun_40_1_0_boot2<-na.omit(sensitivityfun_40_1_0_boot);
sensitivityfun_25_0_1_boot2<-na.omit(sensitivityfun_25_0_1_boot);sensitivityfun_25_1_1_boot2<-na.omit(sensitivityfun_25_1_1_boot);
sensitivityfun_40_0_1_boot2<-na.omit(sensitivityfun_40_0_1_boot);sensitivityfun_40_1_1_boot2<-na.omit(sensitivityfun_40_1_1_boot);
AUC_25_0_0_boot<- c();AUC_25_1_0_boot<- c();AUC_40_0_0_boot<- c();AUC_40_1_0_boot<- c();
AUC_25_0_1_boot<- c();AUC_25_1_1_boot<- c();AUC_40_0_1_boot<- c();AUC_40_1_1_boot<- c();
for (i in 1:nrow(sensitivityfun_25_0_0_boot2)){
  AUC_25_0_0_boot[i]<-round(sum(sensitivityfun_25_0_0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_25_1_0_boot[i]<-round(sum(sensitivityfun_25_1_0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_40_0_0_boot[i]<-round(sum(sensitivityfun_40_0_0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_40_1_0_boot[i]<-round(sum(sensitivityfun_40_1_0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_25_0_1_boot[i]<-round(sum(sensitivityfun_25_0_1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_25_1_1_boot[i]<-round(sum(sensitivityfun_25_1_1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_40_0_1_boot[i]<-round(sum(sensitivityfun_40_0_1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
  AUC_40_1_1_boot[i]<-round(sum(sensitivityfun_40_1_1_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2);
}
sd_AUC_25_0_0<- sd(AUC_25_0_0_boot);sd_AUC_25_1_0<- sd(AUC_25_1_0_boot);sd_AUC_40_0_0<- sd(AUC_40_0_0_boot);sd_AUC_40_1_0<- sd(AUC_40_1_0_boot); 
#0.131158；0.108817；0.109511； 0.08901758
sd_AUC_25_0_1<- sd(AUC_25_0_1_boot);sd_AUC_25_1_1<- sd(AUC_25_1_1_boot);sd_AUC_40_0_1<- sd(AUC_40_0_1_boot);sd_AUC_40_1_1<- sd(AUC_40_1_1_boot);
# 0.1167872； 0.07676762； 0.127697； 0.07677276
AUC_25_0_0_CI <- c(AUC_25_0_0-qnorm(0.975)*sd_AUC_25_0_0,AUC_25_0_0+qnorm(0.975)*sd_AUC_25_0_0)#[1] 0.242935 0.757065
AUC_40_0_0_CI <- c(AUC_40_0_0-qnorm(0.975)*sd_AUC_40_0_0,AUC_40_0_0+qnorm(0.975)*sd_AUC_40_0_0)#[1] 0.2553623 0.6846377
AUC_25_1_0_CI <- c(AUC_25_1_0-qnorm(0.975)*sd_AUC_25_1_0,AUC_25_1_0+qnorm(0.975)*sd_AUC_25_1_0)#[1] 0.5967226 1.0232774
AUC_40_1_0_CI <- c(AUC_40_1_0-qnorm(0.975)*sd_AUC_40_1_0,AUC_40_1_0+qnorm(0.975)*sd_AUC_40_1_0)#[1] 0.6155288 0.9644712
AUC_25_0_1_CI <- c(AUC_25_0_1-qnorm(0.975)*sd_AUC_25_0_0,AUC_25_0_0+qnorm(0.975)*sd_AUC_25_0_0)#[1] 0.272935 0.757065
AUC_40_0_1_CI <- c(AUC_40_0_1-qnorm(0.975)*sd_AUC_40_0_1,AUC_40_0_1+qnorm(0.975)*sd_AUC_40_0_1)#[1] 0.2597185 0.7602815
AUC_25_1_1_CI <- c(AUC_25_1_1-qnorm(0.975)*sd_AUC_25_1_1,AUC_25_1_1+qnorm(0.975)*sd_AUC_25_1_1)#[1] 0.6795382 0.9804618
AUC_40_1_1_CI <- c(AUC_40_1_1-qnorm(0.975)*sd_AUC_40_1_1,AUC_40_1_1+qnorm(0.975)*sd_AUC_40_1_1)#[1] 0.6595281 0.9604719




############## Delong's method for pooled ROC ####################################

SUM_AUC_0<- function(x,y){#x,y are vectors
  z<-c(x,y)
  m<-length(x)
  n<-length(y)
  t<-rank(z)[1:m]
  AUC0<-sum(t)/(m*n)-(m+1)/n/2
  return(AUC0)
}

Var_d_0<-function(x,y){
  z<-c(x,y)
  m<-length(x)
  n<-length(y)
  
  t_x<-rank(x)
  t_y<-rank(y)
  
  t_zx<-rank(z)[1:m]
  t_zy<-rank(z)[m+1:n]
  
  v10_x<-(t_zx-t_x)/n
  v01_y<-1-(t_zy-t_y)/m
  
  s10<-sum((v10_x-SUM_AUC_0(x,y))^2)/(m-1)
  s01<-sum((v01_y-SUM_AUC_0(x,y))^2)/(n-1)
  
  v<-s10/m+s01/n
  return(v)
}

Xi <-workfile_4_final[workfile_4_final[,2]=="1",]$max_score
Yi <-workfile_4_final[workfile_4_final[,2]=="0",]$max_score
AUC_pool_delong<-SUM_AUC_0(x=Xi,y=Yi)
var_delong<-Var_d_0(x=Xi,y=Yi)
AUC_pool_delong-qnorm(0.975)*sqrt(var_delong);AUC_pool_delong+qnorm(0.975)*sqrt(var_delong)

library(pROC)
ROC1 <- roc( workfile_4_final$match,workfile_4_final$max_score)
auc(ROC1)#Area under the curve: 0.707







##################################extract data for test in shiny######################################################
output4_final_shiny <- workfile_4_final[,c(1,2)]
output4_final_shiny$subject_ID <- row.names(output4_final_shiny) 
output4_final_shiny <- output4_final_shiny[,c(1,3,2)]
write.csv(output4_final_shiny,'output4_final_shiny.csv',row.names = F) # test this dataset in shiny app






##################################test output9 in shiny######################################################
output9<-readr::read_csv("output9.csv",col_names = TRUE)
output9<-output9[,-1]
colnames(output9)
workfile_9<-output9[,c(2,9:15)]
workfile_9$match<-as.numeric(workfile_9$match=="match") ##### if match,then match=1, otherwise,match=0
workfile_9_final<-subset(workfile_9,max_score<500)
output9_final_shiny <- workfile_9_final[,c(1,2)]
output9_final_shiny$subject_ID <- row.names(output9_final_shiny) 
output9_final_shiny <- output9_final_shiny[,c(1,3,2)]
write.csv(output9_final_shiny,'output9_final_shiny.csv',row.names = F) # test this dataset in shiny app
workfile_9_final$Label<-ifelse(workfile_9_final$match=="1",'Genuine Score','Imposter Score')
library(plyr)
count(workfile_9_final,'Label')
library(pROC)
ROC1 <- roc( workfile_9_final$match,workfile_9_final$max_score)
auc(ROC1)#A
Xi <-workfile_9_final[workfile_9_final[,2]=="1",]$max_score
Yi <-workfile_9_final[workfile_9_final[,2]=="0",]$max_score
AUC_pool_delong<-SUM_AUC_0(x=Xi,y=Yi)
var_delong<-Var_d_0(x=Xi,y=Yi)
auc(ROC1)-qnorm(0.975)*sqrt(var_delong);auc(ROC1)+qnorm(0.975)*sqrt(var_delong)
