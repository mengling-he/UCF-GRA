########################part 1##################################################

workfile_4_final <- readr::read_csv("./updated_matchscore/index_clean.csv",col_names = TRUE)
sigma_0<-sd(subset(workfile_4_final,match=='0')$max_score)
sigma_1<-sd(subset(workfile_4_final,match=='1')$max_score)
mu <- ddply(workfile_4_final, "Label", summarise, grp.mean=mean(max_score))

########################Part 2##################################################

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


############################################ model B C D A E ################################################################

#####################model with only age#####
model_age_output4 <- lm(max_score~match+age+max_id_age+age:match, data=workfile_4_final)
coef_age<-model_age_output4$coefficients#####use the interaction even though it is insignificant
 
#####################model with only gender#####
model_gender_output4 <- lm(max_score~match+gender+max_id_gender+gender:match, data=workfile_4_final)
coef_gender<-model_gender_output4$coefficients

#####################model with only ethnicity#####
model_ethnicity_output4 <- lm(max_score~match+ethnicity+max_id_ethnicity+ethnicity:match, data=workfile_4_final)
coef_ethnicity<-model_ethnicity_output4$coefficients

#####################model with age and gender (both with interaction with match) use this model
model_full <- lm(max_score~match+age+max_id_age+gender+max_id_gender+age:match+gender:match, data=workfile_4_final)
coef_age_gender_full<-model_full$coefficients

#####################model with age gender and ethnicity (both with interaction with match) use this model
model_4 <- lm(max_score~match+age+max_id_age+gender+max_id_gender+ethnicity+max_id_ethnicity+
                age:match+gender:match+ethnicity:match, data=workfile_4_final)
coef_model_4<-model_4$coefficients


fpr<-seq(0,1,by=0.001)[-1]

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

# 
# rocdata_gender<-data.frame(sensitivity_gender1,sensitivity_gender0,sensitivity_pool,fpr)
# star<-c(0.25,0.50,0.75)
# rocdata_gender_melt<-reshape2::melt(rocdata_gender, id.var='fpr')
# colnames(rocdata_gender_melt)[2]<-"Gender"
# rocdata_gender_melt$Gender<- ifelse(rocdata_gender_melt$Gender=="sensitivity_gender1",paste0("Male AUC=",AUCgender1),
#                                     ifelse(rocdata_gender_melt$Gender=="sensitivity_gender0",paste0("Female AUC=",AUCgender0),paste0("Pooled AUC=",AUCpool)))
# rocdata_gender_melt$point<-ifelse(rocdata_gender_melt$fpr%in%star,rocdata_gender_melt$value,NA)
# rocdata_gender_melt$point<-ifelse(rocdata_gender_melt$Gender=="Pooled AUC=0.75",NA,rocdata_gender_melt$point)  
# 
# 
# ggplot(rocdata_gender_melt, aes(x=fpr,type= Gender,col=Gender)) +
#   labs(x="FPR", y="Sensitivity")+ 
#   geom_line(aes(y=value,linetype=Gender),size=1.2)+
#   scale_linetype_manual(values=c("dotted","longdash","solid"))+
#   geom_point(aes(y=point,shape=Gender),size=3)+
#   scale_shape_manual(values=c(15,16,NA))+##point shape
#   ggtitle("Gender-Adjusted ROC Curves")+
#   theme(#legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
#     text=element_text(size=14, family="Arial"),
#     plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#     legend.position = c(0.75,0.2),
#     legend.title = element_blank(),
#     legend.text = element_text(color = "black", size = 10,family="Arial"),
#     axis.text = element_text(size=10, family="Arial"),
#     axis.line = element_line(colour = 'black'),
#     panel.background = element_blank()
#   )


###############roc of age  18  25 40 60 
sensitivityfun_age18<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age18<-sensitivityfun_age18(fpr)
AUCage18<-round(sum(sensitivity_age18[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage18


sensitivityfun_age25<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 25,x12=25,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age25<-sensitivityfun_age25(fpr)
#plot(x=fpr,y=sensitivity_age25,ylim = c(0,1),main="age1=25")
AUCage25<-round(sum(sensitivity_age25[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage25#

sensitivityfun_age40<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age40<-sensitivityfun_age40(fpr)
AUCage40<-round(sum(sensitivity_age40[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage40###

sensitivityfun_age60<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 60,x12=60,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age60<-sensitivityfun_age60(fpr)

AUCage60<-round(sum(sensitivity_age60[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage60##

# rocdata_age<-data.frame(sensitivity_age18,sensitivity_age25,
#                         sensitivity_age40,sensitivity_age60,sensitivity_pool,fpr)####omit age=20
# rocdata_age_melt<-reshape2::melt(rocdata_age, id.var='fpr')
# colnames(rocdata_age_melt)[2]<-"Age"
# rocdata_age_melt$Age<- ifelse(rocdata_age_melt$Age=="sensitivity_age18",paste0("Age=18 AUC=",AUCage18),
#                               ifelse(rocdata_age_melt$Age=="sensitivity_age25",paste0("Age=25 AUC=",AUCage25),
#                                      ifelse(rocdata_age_melt$Age=="sensitivity_age40",paste0("Age=40 AUC=",AUCage40),
#                                             ifelse(rocdata_age_melt$Age=="sensitivity_age60",paste0("Age=60 AUC=",AUCage60),
#                                                    paste0("Pooled AUC=",AUCpool)) )))
# rocdata_age_melt$point<-ifelse(rocdata_age_melt$fpr%in%star,rocdata_age_melt$value,NA)
# 
# ggplot(rocdata_age_melt, aes(x=fpr, y=value, type= Age,col=Age)) +
#   labs(x="FPR", y="Sensitivity")+
#   geom_line(aes(linetype=Age),size=1.2)+
#   scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
#   geom_point(aes(y=point,shape=Age),size=3)+
#   scale_shape_manual(values=c(15,16,17,18,NA))+
#   ggtitle("Age-Adjusted ROC Curves ")+
#   theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
#         text=element_text(size=14, family="Arial"),
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         legend.position = c(0.75,0.2),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 10,family="Arial"),
#         axis.text = element_text(size=10, family="Arial"),
#         axis.line = element_line(colour = 'black'),
#         panel.background = element_blank()
#   )

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
AUC_ethn1###

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
AUC_ethn0####

# rocdata_ethnicity<-data.frame(sensitivity_ethnicity1,sensitivity_ethnicity0,sensitivity_pool,fpr)
# rocdata_ethnicity_melt<-reshape2::melt(rocdata_ethnicity, id.var='fpr')
# colnames(rocdata_ethnicity_melt)[2]<-"Ethnicity"
# rocdata_ethnicity_melt$Ethnicity<- ifelse(rocdata_ethnicity_melt$Ethnicity=="sensitivity_ethnicity1",
#                                           paste0("Caucasian AUC=",AUC_ethn1),
#                                           ifelse(rocdata_ethnicity_melt$Ethnicity=="sensitivity_ethnicity0",
#                                                  paste0("Non caucasian AUC=",AUC_ethn0),
#                                                  paste0("Pooled AUC=",AUCpool)))
# rocdata_ethnicity_melt$point<-ifelse(rocdata_ethnicity_melt$fpr%in%star,rocdata_ethnicity_melt$value,NA)
# ggplot(rocdata_ethnicity_melt, aes(x=fpr, y=value, type= Ethnicity,col=Ethnicity)) +
#   labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Ethnicity),size=1.2)+
#   scale_linetype_manual(values=c("dotted","longdash","solid"))+
#   geom_point(aes(y=point,shape=Ethnicity),size=3)+
#   scale_shape_manual(values=c(15,16,NA))+
#   ggtitle("Ethnicity-Adjusted ROC Curves")+
#   theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
#         text=element_text(size=14, family="Arial"),
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         legend.position = c(0.7,0.2),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 10,family="Arial"),
#         axis.text = element_text(size=10, family="Arial"),
#         axis.line = element_line(colour = 'black'),
#         panel.background = element_blank()
#   )

#####for gender and age
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
AUC_25_0<-round(sum(sensitivity_25_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_0#

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
AUC_25_1<-round(sum(sensitivity_25_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_25_1##

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
AUC_40_0<-round(sum(sensitivity_40_0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_0##

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
AUC_40_1<-round(sum(sensitivity_40_1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_40_1#

# 
# rocdata_age_gender<-data.frame(sensitivity_40_1,sensitivity_40_0,
#                                sensitivity_25_1,sensitivity_25_0,
#                                sensitivity_pool,fpr)
# rocdata_age_gender_melt<-reshape2::melt(rocdata_age_gender, id.var='fpr')
# colnames(rocdata_age_gender_melt)[2]<-"Demographics"
# rocdata_age_gender_melt$Demographics<- ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_40_1",paste0("Male Age=40 AUC=",AUC_40_1),
#                                               ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_40_0",paste0("Female Age=40 AUC=",AUC_40_0),
#                                                      ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_25_1",paste0("Male Age=25 AUC=",AUC_25_1),
#                                                             ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_25_0",paste0("Female Age=25 AUC=",AUC_25_0),
#                                                                    paste0("Pooled AUC=",AUCpool))
#                                                      )))
# rocdata_age_gender_melt$point<-ifelse(rocdata_age_gender_melt$fpr%in%star,rocdata_age_gender_melt$value,NA)
# ggplot(rocdata_age_gender_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
#   labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
#   scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
#   geom_point(aes(y=point,shape=Demographics),size=3)+
#   scale_shape_manual(values=c(15,16,17,18,NA))+
#   ggtitle("Gender and Age Ajusted ROC Curves")+
#   theme(legend.background = element_rect(fill="transparent",size=10, linetype="solid"),
#         text=element_text(size=14, family="Arial"),
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         legend.position = c(0.7,0.2),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 10,family="Arial"),
#         axis.text = element_text(size=10, family="Arial"),
#         axis.line = element_line(colour = 'black'),
#         panel.background = element_blank()
#   )


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
AUC_25_0_0##

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
AUC_25_1_0#

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
AUC_40_0_0##

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
AUC_40_1_0##

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
AUC_25_0_1##

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
AUC_25_1_1##

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
AUC_40_0_1#

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
AUC_40_1_1##

# #######roc curve for ethnity=noncaucasian
# rocdata_3demographics0<-data.frame(sensitivity_40_1_0,sensitivity_40_0_0,
#                                    sensitivity_25_1_0,sensitivity_25_0_0,
#                                    sensitivity_pool,fpr)
# rocdata_3demographics0_melt<-reshape2::melt(rocdata_3demographics0, id.var='fpr')
# colnames(rocdata_3demographics0_melt)[2]<-"Demographics"
# rocdata_3demographics0_melt$Demographics<- ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_40_1_0",paste0("Male Non-caucasian Age=40 AUC=",AUC_40_1_0),
#                                                   ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_40_0_0",paste0("Female Non-caucasian Age=40 AUC=",AUC_40_0_0),
#                                                          ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_25_1_0",paste0("Male Non-caucasian Age=25 AUC=",AUC_25_1_0),
#                                                                 ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_25_0_0",paste0("Female Non-caucasian Age=25 AUC=",AUC_25_0_0),
#                                                                        paste0("Pooled AUC=",AUCpool)))))
# rocdata_3demographics0_melt$point<-ifelse(rocdata_3demographics0_melt$fpr%in%star,rocdata_3demographics0_melt$value,NA)
# ggplot(rocdata_3demographics0_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
#   labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
#   scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
#   geom_point(aes(y=point,shape=Demographics),size=3)+
#   scale_shape_manual(values=c(15,16,17,18,NA))+
#   ggtitle("Gender, Age and Ethnicity Ajusted ROC Curves",subtitle = "Ethnicity=Non-caucasian")+
#   theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
#         text=element_text(size=14, family="Arial"),
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
#         legend.position = c(0.68,0.25),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 10,family="Arial"),
#         axis.text = element_text(size=10, family="Arial"),
#         axis.line = element_line(colour = 'black'),
#         panel.background = element_blank()
#   )

# #######roc curve for ethnity=caucasian
# rocdata_3demographics1<-data.frame(sensitivity_40_1_1,sensitivity_40_0_1,
#                                    sensitivity_25_1_1,sensitivity_25_0_1,
#                                    sensitivity_pool,fpr)
# rocdata_3demographics1_melt<-reshape2::melt(rocdata_3demographics1, id.var='fpr')
# colnames(rocdata_3demographics1_melt)[2]<-"Demographics"
# rocdata_3demographics1_melt$Demographics<- ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_40_1_1",paste0("Male Caucasian Age=40 AUC=",AUC_40_1_1),
#                                                   ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_40_0_1",paste0("Female Caucasian Age=40 AUC=",AUC_40_0_1),
#                                                          ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_25_1_1",paste0("Male Caucasian Age=25 AUC=",AUC_25_1_1),
#                                                                 ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_25_0_1",paste0("Female Caucasian Age=25 AUC=",AUC_25_0_1),
#                                                                        paste0( "Pooled AUC=",AUCpool)))))
# rocdata_3demographics1_melt$point<-ifelse(rocdata_3demographics1_melt$fpr%in%star,rocdata_3demographics1_melt$value,NA)
# ggplot(rocdata_3demographics1_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
#   labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
#   scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
#   geom_point(aes(y=point,shape=Demographics),size=3)+
#   scale_shape_manual(values=c(15,16,17,18,NA))+
#   ggtitle("Gender, Age and Ethnicity Ajusted ROC Curves",subtitle = "Ethnicity=Caucasian")+
#   theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
#         text=element_text(size=14, family="Arial"),
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
#         legend.position = c(0.68,0.25),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 10,family="Arial"),
#         axis.text = element_text(size=10, family="Arial"),
#         axis.line = element_line(colour = 'black'),
#         panel.background = element_blank()
#   )



####################################### Part 3: Variance Part ###########################################

###bootstrap
iterations <- 5000
df_boot <- list()# each element of df_boot is a sampled data from the dataset
mu_boot <- list() # the mean value of genuine and imposter in each dataset
sigma_0_boot<- c() #the sd of of genuine in each dataset 
sigma_1_boot<- c() # the sd of of genuine in each dataset
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

AUC_pool_boot <- c()  # pooled AUC in each bootstrap
for (i in 1:iterations){
  AUC_pool_boot[i]<- round(sum(sensitivityfun_pool_boot[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}

sd_AUCpool<- sd(AUC_pool_boot)# pooled AUC sd
AUCpool_CI <- c(AUCpool-qnorm(0.975)*sd_AUCpool,AUCpool+qnorm(0.975)*sd_AUCpool)#[1]  ## use delong's method; shiny app


############## check: Delong's method for pooled ROC ####################################

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

Xi <-workfile_4_final[workfile_4_final$match=="1",]$max_score
Yi <-workfile_4_final[workfile_4_final$match=="0",]$max_score
AUC_pool_delong<-SUM_AUC_0(x=Xi,y=Yi)
var_delong <-Var_d_0(x=Xi,y=Yi)
AUC_CI_delong<-c(AUC_pool_delong-qnorm(0.975)*sqrt(var_delong),AUC_pool_delong+qnorm(0.975)*sqrt(var_delong))


library(pROC)
ROC1 <- roc( workfile_4_final$match,workfile_4_final$max_score)
auc(ROC1)









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
sensitivityfun_gender0_boot2 <- na.omit(sensitivityfun_gender0_boot)# dim [1] 
AUC_gender0_boot<- c()
for (i in 1:nrow(sensitivityfun_gender0_boot2)){
  AUC_gender0_boot[i]<-round(sum(sensitivityfun_gender0_boot2[i,]*diff(c(0, fpr[1:length(fpr)]))),2)
}
sd_AUCgender0<- sd(AUC_gender0_boot)#
AUCgender0_CI <- c(AUCgender0-qnorm(0.975)*sd_AUCgender0,AUCgender0+qnorm(0.975)*sd_AUCgender0)#[1] 

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
sd_AUCgender1<- sd(AUC_gender1_boot)#
AUCgender1_CI <- c(AUCgender1-qnorm(0.975)*sd_AUCgender1,AUCgender1+qnorm(0.975)*sd_AUCgender1)#[1]

# ROC curve confidence intervel
sd0_fpr <- apply(sensitivityfun_gender0_boot2, 2,sd);
sd1_fpr <- apply(sensitivityfun_gender1_boot2, 2,sd)
tpr_gender <- data.frame(fpr=fpr,FPR0=sensitivity_gender0, FPR0_L=sensitivity_gender0-qnorm(0.975)*sd0_fpr,
                         FPR0_H=sensitivity_gender0+qnorm(0.975)*sd0_fpr,FPR1=sensitivity_gender1, 
                         FPR1_L=sensitivity_gender1-qnorm(0.975)*sd1_fpr,FPR1_H=sensitivity_gender1+qnorm(0.975)*sd1_fpr)


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
sd_AUC_25_0_1<- sd(AUC_25_0_1_boot);sd_AUC_25_1_1<- sd(AUC_25_1_1_boot);sd_AUC_40_0_1<- sd(AUC_40_0_1_boot);sd_AUC_40_1_1<- sd(AUC_40_1_1_boot);
AUC_25_0_0_CI <- c(AUC_25_0_0-qnorm(0.975)*sd_AUC_25_0_0,AUC_25_0_0+qnorm(0.975)*sd_AUC_25_0_0)#[1] 
AUC_40_0_0_CI <- c(AUC_40_0_0-qnorm(0.975)*sd_AUC_40_0_0,AUC_40_0_0+qnorm(0.975)*sd_AUC_40_0_0)#[1] 
AUC_25_1_0_CI <- c(AUC_25_1_0-qnorm(0.975)*sd_AUC_25_1_0,AUC_25_1_0+qnorm(0.975)*sd_AUC_25_1_0)#[1] 
AUC_40_1_0_CI <- c(AUC_40_1_0-qnorm(0.975)*sd_AUC_40_1_0,AUC_40_1_0+qnorm(0.975)*sd_AUC_40_1_0)
AUC_25_0_1_CI <- c(AUC_25_0_1-qnorm(0.975)*sd_AUC_25_0_0,AUC_25_0_0+qnorm(0.975)*sd_AUC_25_0_0)
AUC_40_0_1_CI <- c(AUC_40_0_1-qnorm(0.975)*sd_AUC_40_0_1,AUC_40_0_1+qnorm(0.975)*sd_AUC_40_0_1)
AUC_25_1_1_CI <- c(AUC_25_1_1-qnorm(0.975)*sd_AUC_25_1_1,AUC_25_1_1+qnorm(0.975)*sd_AUC_25_1_1)
AUC_40_1_1_CI <- c(AUC_40_1_1-qnorm(0.975)*sd_AUC_40_1_1,AUC_40_1_1+qnorm(0.975)*sd_AUC_40_1_1)




covariate<- c('Age=25 Gender=Female','Age=40 Gender=Female',
              'Age=25 Gender=Male','Age=40 Gender=Male',
              'Age= 18','Age= 25','Age= 40','Age= 60',
              'Gender=Female','Gender=Male','Ethnicity= Caucasian','Ethnicity= Non-caucasian',
              'Age=25 Gender=Female Ethnicity= Caucasian','Age=40 Gender=Female Ethnicity= Caucasian',
              'Age=25 Gender=Male Ethnicity= Caucasian','Age=40 Gender=Male Ethnicity= Caucasian',
              'Age=25 Gender=Female Ethnicity= Non-caucasian','Age=40 Gender=Female Ethnicity= Non-caucasian',
              'Age=25 Gender=Male Ethnicity= Non-caucasian','Age=40 Gender=Male Ethnicity= Non-caucasian')
model <- c(rep('ModelA',4),rep('ModelB',4),rep('ModelC',2),rep('ModelD',2),rep('ModelE',8))
Result_variance <- c(sd_AUC_25_0^2,sd_AUC_40_0^2,sd_AUC_25_1^2,sd_AUC_40_1^2,
                     sd_AUCage18^2,sd_AUCage25^2,sd_AUCage40^2,sd_AUCage60^2,
                     sd_AUCgender0^2,sd_AUCgender1^2,sd_AUC_ethn1^2,sd_AUC_ethn0^2,
                     sd_AUC_25_0_1^2,sd_AUC_40_0_1^2,sd_AUC_25_1_1^2,sd_AUC_40_1_1^2,
                     sd_AUC_25_0_0^2,sd_AUC_40_0_0^2,sd_AUC_25_1_0^2,sd_AUC_40_1_0^2)
Result_CI_L <- c(AUC_25_0_CI[1],AUC_40_0_CI[1],AUC_25_1_CI[1],AUC_40_1_CI[1],
               AUCage18_CI[1],AUCage25_CI[1],AUCage40_CI[1],AUCage60_CI[1],
               AUCgender1_CI[1],AUCgender0_CI[1],AUCethn1_CI[1],AUCethn0_CI[1],
               AUC_25_0_1_CI[1],AUC_40_0_1_CI[1],AUC_25_1_1_CI[1],AUC_40_1_1_CI[1],
               AUC_25_0_0_CI[1],AUC_40_0_0_CI[1],AUC_25_1_0_CI[1],AUC_40_1_0_CI[1])
Result_CI_H <- c(AUC_25_0_CI[2],AUC_40_0_CI[2],AUC_25_1_CI[2],AUC_40_1_CI[2],
                 AUCage18_CI[2],AUCage25_CI[2],AUCage40_CI[2],AUCage60_CI[2],
                 AUCgender1_CI[2],AUCgender0_CI[2],AUCethn1_CI[2],AUCethn0_CI[2],
                 AUC_25_0_1_CI[2],AUC_40_0_1_CI[2],AUC_25_1_1_CI[2],AUC_40_1_1_CI[2],
                 AUC_25_0_0_CI[2],AUC_40_0_0_CI[2],AUC_25_1_0_CI[2],AUC_40_1_0_CI[2])

###save plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to=".")

### save results
result <- data.frame(model,covariate,Result_variance,Result_CI_L,Result_CI_H)
write.csv(result,'result.csv',row.names = F) 

