####read the raw dataset#####
# index_LFIQ <- readr::read_csv("./WACV2022/Right_index_f2.csv",col_names = TRUE)#full dataset
# thumb_LFIQ <- readr::read_csv("./WACV2022/Right_thumb_f1.csv",col_names = TRUE)#full dataset
#colnames(index_LFIQ)
#colnames(thumb_LFIQ)# max_score is not the maximum score, just name it to keep consistent with previous code in CVIP
####extract the LFIQ and give it to the cleaned dataset
# LFIQ_list_index <- index_LFIQ[,c("full_latent_ID","LFIQ")];
# LFIQ_list_index <- LFIQ_list_index[!duplicated(LFIQ_list_index),]
# LFIQ_list_thumb <- thumb_LFIQ[,c("full_latent_ID","LFIQ")];
# LFIQ_list_thumb <- LFIQ_list_thumb[!duplicated(LFIQ_list_thumb),]
# index_clean <- readr::read_csv("./updated_matchscore/index_clean.csv",col_names = TRUE)#full dataset
# thumb_clean <- readr::read_csv("./updated_matchscore/thumb_clean.csv",col_names = TRUE)#f
# index_clean_lfiq <- merge(index_clean,LFIQ_list_index,by='full_latent_ID') ##some rows are deleted
# thumb_clean_lfiq <- merge(thumb_clean,LFIQ_list_thumb,by='full_latent_ID')
# 
# write.csv(index_clean_lfiq,'./WACV2022/index_clean_lfiq.csv',row.names = F)
# write.csv(thumb_clean_lfiq,'./WACV2022/thumb_clean_lfiq.csv',row.names = F)

#### read the dataset: cleaned#####
#index_clean_lfiq <- readr::read_csv("./WACV2022/index_clean_lfiq0.csv",col_names = TRUE)#cleaned dataset with LFIQ
right_index_lfiq <- readr::read_csv("./ACM2022/right_index_clean_lfiq.csv",col_names = TRUE)#cleaned dataset with LFIQ
right_thumb_lfiq <- readr::read_csv("./ACM2022/right_thumb_clean_lfiq.csv",col_names = TRUE)#cleaned datasetwith LFIQ
finger_list_index <- right_index_lfiq[,"full_latent_ID"];
finger_list_index <- finger_list_index[!duplicated(finger_list_index),]
finger_list_thumb <- right_thumb_lfiq[,"full_latent_ID"];
finger_list_thumb <- finger_list_thumb[!duplicated(finger_list_thumb),]

####data cleaning
summary(right_index_lfiq$max_score)
right_index_clean_lfiq <- right_index_lfiq
#workfile_4_outlier1<-right_index_clean_lfiq[(right_index_clean_lfiq$full_latent_ID=='9491504_F02_NI_PA_#2'),] ###age=0
right_index_clean_lfiq<-right_index_clean_lfiq[!(right_index_clean_lfiq$full_latent_ID=='9491504_F02_NI_PA_#2'),]# delete the subject whose age is unknown
right_index_clean_lfiq <-  subset(right_index_clean_lfiq,max_score > 0)
right_index_clean_lfiq<-subset(right_index_clean_lfiq,max_score<200)
right_index_clean_lfiq<-right_index_clean_lfiq[right_index_clean_lfiq$max_score >exp(-1.25),]# delete those too small outlier
summary(right_index_clean_lfiq$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.447   3.992   6.302   7.616   9.488 190.874 
#####obs after cleaning: 40689

summary(right_thumb_lfiq$max_score)
right_thumb_clean_lfiq <- right_thumb_lfiq
right_thumb_clean_lfiq <-  subset(right_thumb_clean_lfiq,max_score > exp(-1))
right_thumb_clean_lfiq<-subset(right_thumb_clean_lfiq,max_score<300)
#right_thumb_clean_lfiq<-right_thumb_clean_lfiq[!(right_thumb_clean_lfiq$full_latent_ID=='9491504_F02_NI_PA_#2'),]# delete the subject whose age is unknown
#right_thumb_clean_lfiq<-right_thumb_clean_lfiq[right_thumb_clean_lfiq$max_score >exp(-1.25),]# delete those too small outlier
summary(right_thumb_clean_lfiq$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.535   5.579   9.094  11.304  14.243 294.412 
####obs after cleaning: 41563

library(plyr)
###add some description to demographics for plot
# change ethnicity group
right_index_clean_lfiq$ethnicity<-ifelse(right_index_clean_lfiq$ethnicity=="Caucasian","Caucasian","Non Caucasian")
right_index_clean_lfiq$max_id_ethnicity<-ifelse(right_index_clean_lfiq$max_id_ethnicity=="Caucasian","Caucasian","Non Caucasian")
right_thumb_clean_lfiq$ethnicity<-ifelse(right_thumb_clean_lfiq$ethnicity=="Caucasian","Caucasian","Non Caucasian")
right_thumb_clean_lfiq$max_id_ethnicity<-ifelse(right_thumb_clean_lfiq$max_id_ethnicity=="Caucasian","Caucasian","Non Caucasian")
#count(right_thumb_clean_lfiq,'ethnicity')

right_index_clean_lfiq$gender0 <- right_index_clean_lfiq$gender;
right_index_clean_lfiq$max_id_gender0 <- right_index_clean_lfiq$max_id_gender
right_index_clean_lfiq$gender <- as.numeric(right_index_clean_lfiq$gender=="Male")
right_index_clean_lfiq$max_id_gender <- as.numeric(right_index_clean_lfiq$max_id_gender=="Male");
right_index_clean_lfiq$ethnicity0<- right_index_clean_lfiq$ethnicity
right_index_clean_lfiq$max_id_ethnicity0<- right_index_clean_lfiq$max_id_ethnicity
right_index_clean_lfiq$ethnicity <- as.numeric(right_index_clean_lfiq$ethnicity=="Caucasian")
right_index_clean_lfiq$max_id_ethnicity <-as.numeric(right_index_clean_lfiq$max_id_ethnicity=="Caucasian");
right_index_clean_lfiq$Label <- ifelse(right_index_clean_lfiq$match==0,'Imposter','Genuine')


right_thumb_clean_lfiq$gender0 <- right_thumb_clean_lfiq$gender;
right_thumb_clean_lfiq$max_id_gender0 <- right_thumb_clean_lfiq$max_id_gender
right_thumb_clean_lfiq$gender <- as.numeric(right_thumb_clean_lfiq$gender=="Male")
right_thumb_clean_lfiq$max_id_gender <- as.numeric(right_thumb_clean_lfiq$max_id_gender=="Male");
right_thumb_clean_lfiq$ethnicity0<- right_thumb_clean_lfiq$ethnicity
right_thumb_clean_lfiq$max_id_ethnicity0<- right_thumb_clean_lfiq$max_id_ethnicity
right_thumb_clean_lfiq$ethnicity <- as.numeric(right_thumb_clean_lfiq$ethnicity=="Caucasian")
right_thumb_clean_lfiq$max_id_ethnicity <-as.numeric(right_thumb_clean_lfiq$max_id_ethnicity=="Caucasian");
right_thumb_clean_lfiq$Label <- ifelse(right_thumb_clean_lfiq$match==0,'Imposter','Genuine')

# discard mnt<5
index_clean_lfiq <-subset(right_index_clean_lfiq,mnt>=5)
thumb_clean_lfiq <-subset(right_thumb_clean_lfiq,mnt>=5)

### dataset for genuine score
index_genuine_lfiq <- subset(index_clean_lfiq,match=='1')
thumb_genuine_lfiq <- subset(thumb_clean_lfiq,match=='1')

####Dataset for patent: LFIQ; demographics
LFIQ_list_index <- index_clean_lfiq[,c("full_latent_ID","LFIQ","gender","age","ethnicity","gender0","ethnicity0")];
LFIQ_list_index <- LFIQ_list_index[!duplicated(LFIQ_list_index),]
LFIQ_list_thumb <- thumb_clean_lfiq[,c("full_latent_ID","LFIQ","gender","age","ethnicity","gender0","ethnicity0")];
LFIQ_list_thumb <- LFIQ_list_thumb[!duplicated(LFIQ_list_thumb),]

summary(LFIQ_list_index$LFIQ)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1772  5.6664  9.0009 11.1886 12.9702 67.0064 
summary(LFIQ_list_thumb$LFIQ)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.00306   7.25664  12.41672  17.36693  21.16960 112.72150 
######after deleting the MNT smaller than 5, there are 186 obs for index;
######and 437 obs for thumb

# response is log-transformed during modeling
#log transform #max_score0 is the original score, max_score is the log-transformed score;
index_clean_lfiq$max_score0<- index_clean_lfiq$max_score;
index_clean_lfiq$max_score<- log(index_clean_lfiq$max_score0)
thumb_clean_lfiq$max_score0<- thumb_clean_lfiq$max_score;
thumb_clean_lfiq$max_score<- log(thumb_clean_lfiq$max_score0)

####calculate the mean of each group 
mu_index <- ddply(index_clean_lfiq, "Label", summarise, grp.mean=mean(max_score))
mu_thumb <- ddply(thumb_clean_lfiq, "Label", summarise, grp.mean=mean(max_score))

######################match score dist#######
library(ggplot2)
ggplot(index_clean_lfiq, aes(max_score, fill=Label, color=Label)) +
  geom_histogram(aes(y=..density..), breaks=seq(-1,6,1), alpha=0.8, 
                 position="identity", lwd=0.2) +
  theme_bw() +
  geom_vline(data=mu_index, aes(xintercept=grp.mean, color=Label),linetype="dashed")+
  #ggtitle("Normalized Histogram of Match Score")+
  labs(x='Match Score',title = "Distributions of Match Scores")+labs(y='Density')+
  theme(#legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
    legend.position = c(0.8,0.85),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 8,family="Arial"),
    text=element_text(size=14, family="Arial"),
    plot.title = element_text(size=14, family="Arial",hjust = 0.5),
    axis.text = element_text(size=14, family="Arial")
    #plot.background = element_blank(),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )


ggplot(thumb_clean_lfiq, aes(max_score, fill=Label, color=Label)) +
  geom_histogram(aes(y=..density..), breaks=seq(-1,6,1), alpha=0.8, 
                 position="identity", lwd=0.2) +
  theme_bw() +
  geom_vline(data=mu_thumb, aes(xintercept=grp.mean, color=Label),linetype="dashed")+
  #ggtitle("Normalized Histogram of Match Score")+
  labs(x='Match Score',title = "Distributions of Match Scores")+labs(y='Density')+
  theme(#legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
    legend.position = c(0.8,0.85),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 8,family="Arial"),
    text=element_text(size=14, family="Arial"),
    plot.title = element_text(size=14, family="Arial",hjust = 0.5),
    axis.text = element_text(size=14, family="Arial")
    #plot.background = element_blank(),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )

######score vs. LFIQ. age
ggplot(index_clean_lfiq, aes(age, max_score,size=max_score,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='Genuine Score',title ="Genuine Match Scores vs. Age" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 
ggplot(index_genuine_lfiq, aes(age, max_score,size=max_score,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='Genuine Score',title ="Genuine Match Scores vs. Age" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 
ggplot(thumb_genuine_lfiq, aes(age, max_score,size=max_score,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='Genuine Score',title ="Genuine Match Scores vs. Age" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 





############################ plot of LFIQ vs. demographics ############################
###index
ggplot(LFIQ_list_index, aes(LFIQ)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,70,1), alpha=0.8,
                 position="identity", lwd=0.2) +
  theme_bw() +
  labs(x='LFIQ Score',title = "Distributions of LFIQ Scores")+labs(y='Density')+
  theme(#legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
    legend.position = c(0.8,0.85),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 8,family="Arial"),
    text=element_text(size=14, family="Arial"),
    plot.title = element_text(size=14, family="Arial",hjust = 0.5),
    axis.text = element_text(size=14, family="Arial")
  )

ggplot(LFIQ_list_index, aes(age, LFIQ,size=LFIQ,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='LFIQ Score')+
  ggtitle ("LFIQ Score vs. Age" , subtitle = "Right Index")+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 
ggplot(data=LFIQ_list_index, aes(x=LFIQ, color=gender0,fill=gender0)) +
  xlim(-2,70)+
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='LFIQ Score',y='Density')+
  ggtitle ("Distributions of LFIQ Score\n based on Gender", subtitle = "Right Index" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggplot(data=LFIQ_list_index, aes(x=LFIQ, color=ethnicity0,fill=ethnicity0)) +
  xlim(-2,70)+
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='LFIQ Score',y='Density')+
  ggtitle ("Distributions of LFIQ Score\n based on Ethnicity", subtitle = "Right Index" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

### thumb
ggplot(LFIQ_list_thumb, aes(LFIQ)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,120,1), alpha=0.8,
                 position="identity", lwd=0.2) +
  theme_bw() +
  labs(x='LFIQ Score',title = "Distributions of LFIQ Scores")+labs(y='Density')+
  theme(#legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
    legend.position = c(0.8,0.85),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 8,family="Arial"),
    text=element_text(size=14, family="Arial"),
    plot.title = element_text(size=14, family="Arial",hjust = 0.5),
    axis.text = element_text(size=14, family="Arial")
  )

ggplot(LFIQ_list_thumb, aes(age, LFIQ,size=LFIQ,color=age)) +
  geom_point()+
  theme_bw() + 
  #xlim(-5,120)+
  labs(x='Age', y='LFIQ Score')+
  ggtitle ("LFIQ Score vs. Age" , subtitle = "Right Thumb")+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 

ggplot(data=LFIQ_list_thumb, aes(x=LFIQ, color=gender0,fill=gender0)) +
  xlim(-5,120)+
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='LFIQ Score',y='Density')+
  ggtitle ("Distributions of LFIQ Score\n based on Gender", subtitle = "Right Thumb" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(data=LFIQ_list_thumb, aes(x=LFIQ, color=ethnicity0,fill=ethnicity0)) +
  xlim(-5,120)+
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='LFIQ Score',y='Density')+
  ggtitle ("Distributions of LFIQ Score\n based on Ethnicity", subtitle = "Right Thumb" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="./ACM2022/plots")
dev.off()
                             
# nrow(index_clean_lfiq[index_clean_lfiq$LFIQ==0,])#3814
# nrow(thumb_clean_lfiq[thumb_clean_lfiq$LFIQ==0,])#1357
# count(unique(index_clean_lfiq[index_clean_lfiq$LFIQ==0,]$full_latent_ID))#26
# count(unique(thumb_clean_lfiq[thumb_clean_lfiq$LFIQ==0,]$full_latent_ID))#18
# summary(index_clean_lfiq[index_clean_lfiq$LFIQ!=0,]$LFIQ)
############################ regression of LFIQ vs. demographics ############################
# model_l <- lm(LFIQ~gender+age+ethnicity+gender:age+gender:ethnicity+age:ethnicity, data=LFIQ_list_index)
# coef_l<-model_l$coefficients#####use the interaction even though it is insignificant
# summary(model_l)
# 
# model_ <- lm(LFIQ~gender+age+ethnicity, data=LFIQ_list_index)
# coef_l<-model_l$coefficients#####use the interaction even though it is insignificant
# summary(model_l)
# 
# model_li <- lm(LFIQ~gender+age+ethnicity, data=LFIQ_list_index)
# coef_li<-model_li$coefficients#####use the interaction even though it is insignificant
# summary(model_li)
# 
# model_lt <- lm(LFIQ~gender+age+ethnicity, data=LFIQ_list_thumb)
# coef_lt<-model_lt$coefficients#####use the interaction even though it is insignificant
# summary(model_lt)
# 
# reg_lfiq <- data.frame(coef_li,coef_lt)
# write.csv(reg_lfiq,'./AVM2022/reg_lfiq.csv',row.names = F) 


############################ plot of match vs. LFIQ  ############################
# ggplot(index_genuine_lfiq, aes(LFIQ, max_score,size=max_score,color=LFIQ)) +
#   geom_point()+
#   theme_bw() + 
#   labs(x='LFIQ', y='Genuine Score')+
#   ggtitle ("Genuine Match Scores vs. LFIQ", subtitle = "Right Index" )+
#   theme(legend.position = "none",
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
#         text=element_text(size=14, family="Arial"),
#         axis.text = element_text(size=14, family="Arial")) 

ggplot(index_clean_lfiq, aes(LFIQ, max_score,color=Label,shape=Label)) +
  geom_point(alpha=0.5)+
  scale_shape_manual(values=c(16, 3))+
  #scale_color_manual(values = c('#999999','#E69F00')) +
  theme_bw() + 
  labs(x='LFIQ', y='Match Score')+
  ggtitle ("Match Scores vs. LFIQ", subtitle = "Right Index" )+
  theme(legend.position = c(0.8,0.15),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8,family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 

ggplot(thumb_genuine_lfiq, aes(LFIQ, max_score,size=max_score,color=LFIQ)) +
  geom_point()+
  theme_bw() + 
  labs(x='LFIQ', y='Genuine Score')+
  ggtitle ("Genuine Match Scores vs. LFIQ", subtitle = "Right Thumb" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 

# ggplot(thumb_clean_lfiq, aes(LFIQ, max_score,color=Label,shape=Label)) +
#   geom_point(alpha=0.5)+
#   scale_shape_manual(values=c(16, 3))+
#   #scale_color_manual(values = c('#999999','#E69F00')) +
#   theme_bw() + 
#   labs(x='LFIQ', y='Match Score')+
#   ggtitle ("Match Scores vs. LFIQ", subtitle = "Right Thumb" )+
#   theme(legend.position = c(0.8,0.15),
#         legend.title = element_blank(),
#         legend.text = element_text(color = "black", size = 8,family="Arial"),
#         plot.title = element_text(size=14, family="Arial",hjust = 0.5),
#         plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
#         text=element_text(size=14, family="Arial"),
#         axis.text = element_text(size=14, family="Arial")) 


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="./ACM2022/plots")
dev.off()



















############################################ modeling ################################################################
##############
##############
##############change workfile: index/thumb
workfile <- index_clean_lfiq
workfile <- thumb_clean_lfiq
##############
##############
##############
#####################model with only lfiq#####
model_lfiq <- lm(max_score~match+LFIQ+match:LFIQ, data=workfile)
coef_lfiq<-model_lfiq$coefficients#####use the interaction even though it is insignificant
summary(model_lfiq)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.5756475  0.0050396 312.655  < 2e-16 ***
#   match       0.4419057  0.0745129   5.931 3.05e-09 ***
#   LFIQ        0.0227597  0.0003274  69.519  < 2e-16 ***
#   match:LFIQ  0.0257109  0.0049141   5.232 1.69e-07 ***
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.8730814  0.0046529  402.56   <2e-16 ***
#   match       0.6196122  0.0447335   13.85   <2e-16 ***
#   LFIQ        0.0177249  0.0001931   91.81   <2e-16 ***
#   match:LFIQ  0.0216367  0.0018828   11.49   <2e-16 ***
# #####################model with only age#####
# model_age <- lm(max_score~match+age+max_id_age+age:match, data=workfile)
# coef_age<-model_age$coefficients#####use the interaction even though it is insignificant
# 
# #####################model with only gender#####
# model_gender <- lm(max_score~match+gender+max_id_gender+gender:match, data=workfile)
# coef_gender<-model_gender$coefficients
# 
# #####################model with only ethnicity#####
# model_ethnicity<- lm(max_score~match+ethnicity+max_id_ethnicity+ethnicity:match, data=workfile)
# coef_ethnicity<-model_ethnicity$coefficients
# 
# #####################model with age and gender (both with interaction with match) 
# model_ga <- lm(max_score~match+age+max_id_age+gender+max_id_gender+age:match+gender:match, data=workfile)
# coef_age_gender<-model_ga$coefficients
# 
# #####################model with age gender and ethnicity (both with interaction with match) use this model
# model_gae <- lm(max_score~match+age+max_id_age+gender+max_id_gender+ethnicity+max_id_ethnicity+
#                 age:match+gender:match+ethnicity:match, data=workfile)
# coef_age_gender_eth<-model_gae$coefficients

#####################model with age gender,ethnicity and lfiq (both with interaction with match) use this model
model_gael <- lm(max_score~match+age+max_id_age+gender+max_id_gender+ethnicity+max_id_ethnicity+LFIQ+
                  age:match+gender:match+ethnicity:match+LFIQ:match, data=workfile)
coef_full<-model_gael$coefficients
summary(model_gael)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       1.3016846  0.0169922  76.605  < 2e-16 ***
#   match            -0.3021831  0.1868195  -1.618  0.10578    
# age               0.0015261  0.0003412   4.473 7.75e-06 ***
#   max_id_age        0.0021285  0.0003170   6.715 1.91e-11 ***
#   gender            0.0796509  0.0065996  12.069  < 2e-16 ***
#   max_id_gender     0.1056504  0.0066967  15.777  < 2e-16 ***
#   ethnicity         0.0624639  0.0079157   7.891 3.08e-15 ***
#   max_id_ethnicity  0.0250866  0.0079036   3.174  0.00150 ** 
#   LFIQ              0.0229107  0.0003260  70.269  < 2e-16 ***
#   match:age         0.0090510  0.0049736   1.820  0.06880 .  
# match:gender      0.4077305  0.0964644   4.227 2.38e-05 ***
#   match:ethnicity   0.3453381  0.1159081   2.979  0.00289 ** 
#   match:LFIQ        0.0265940  0.0048865   5.442 5.29e-08 ***
#   ---
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       1.8833283  0.0172443 109.214  < 2e-16 ***
#   match             0.4666905  0.1235999   3.776  0.00016 ***
#   age              -0.0025350  0.0003489  -7.266 3.76e-13 ***
#   max_id_age        0.0019836  0.0003363   5.898 3.72e-09 ***
#   gender            0.0166703  0.0064854   2.570  0.01016 *  
#   max_id_gender     0.0929992  0.0064567  14.404  < 2e-16 ***
#   ethnicity        -0.0827157  0.0074905 -11.043  < 2e-16 ***
#   max_id_ethnicity  0.0117330  0.0074899   1.567  0.11724    
# LFIQ              0.0175094  0.0001934  90.544  < 2e-16 ***
#   match:age         0.0034452  0.0034121   1.010  0.31264    
# match:gender      0.2534803  0.0629225   4.028 5.62e-05 ***
#   match:ethnicity  -0.0742349  0.0730815  -1.016  0.30974    
# match:LFIQ        0.0210160  0.0018871  11.136  < 2e-16 ***

###residual plot for full model
fitt <- fitted.values(model_gael)
res <- resid(model_gael)
qqnorm(res)
qqline(res) 

plot(fitt, res, 
            ylab="Residuals", xlab="Fitted Value", 
            main="Residuals vs. Fitted") 
abline(0, 0)           
plot(model_gael)
###save result
result <- merge(data.frame(coef_full),data.frame(coef_lfiq),by=0,all=TRUE)
##########
##########
regres_result_index <- result
write.csv(regres_result_index,'./ACM2022/regres_result_index.csv',row.names = F) 
regres_result_thumb <- result
write.csv(regres_result_thumb,'./ACM2022/regres_result_thumb.csv',row.names = F)
#########
##########

################################################### ROC Curve #########################################################

fpr<-seq(0,1,by=0.001)[-1]
sigma_0<-sd(subset(workfile,match=='0')$max_score)
sigma_1<-sd(subset(workfile,match=='1')$max_score)
mu <- ddply(workfile, "Label", summarise, grp.mean=mean(max_score))

################analysis################
roc_fn <- function(p,x11,x12=0,x21,x22=0,x31,x32=0,x4,
                   beta_0,beta_L,beta_11,beta_L11,beta_21,beta_L21,beta_31=0,beta_L31=0, beta_4=0,beta_L4=0,
                   beta_12=0, beta_L12=0,  beta_22=0, beta_L22=0, beta_32=0, beta_L32=0,
                   sigma0, sigma1){
  
  mu1 = beta_0+beta_L+(beta_11+beta_L11)*x11+(beta_12+beta_L12)*x12+(beta_21+beta_L21)*x21+(beta_22+beta_L22)*x22+
    (beta_31+beta_L31)*x31+(beta_32+beta_L32)*x32+(beta_4+beta_L4)*x4
  
  mu0 = beta_0+beta_11*x11+beta_12*x12+beta_21*x21+beta_22*x22+beta_31*x31+beta_32*x32+beta_4*x4
  
  a=(mu1-mu0)/sigma1
  
  b=sigma0/sigma1
  
  roc=1-pnorm(b*qnorm(1-p)-a)
  
  return(roc)
  
}

###############pooled ROC
sensitivityfun_pool<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x12=0,x21=0,x22=0,x31=0,x32=0,x4=0,
                             beta_0 =mu$grp.mean[2],beta_L =mu$grp.mean[1]-mu$grp.mean[2],
                             beta_11 = 0,beta_L11 =0,beta_21 = 0,beta_L21 = 0,beta_22 = 0,
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_pool<-sensitivityfun_pool(fpr)
AUCpool<-round(sum(sensitivity_pool[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCpool#[1] 0.71
#0.74--->0.75????

star<-c(0.25,0.50,0.75)
##############roc of lfiq; 1st QU; median; 3rd qu
lfiq_value<-round(quantile(LFIQ_list_index$LFIQ, probs = c(0.25,0.5, 0.75,0.9)),2)
# 10%   25%   50%   75%   90% 
# 2.37  5.74  9.03 13.00 22.48 
lfiq_value<-round(quantile(LFIQ_list_thumb$LFIQ, probs = c(0.25,0.5, 0.75,0.9)),2)
# 25%   50%   75%   90% 
#   7.26 12.42 21.17 38.24

# ###############roc of lfiq 
sensitivityfun_lfiq1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x21=0,x22=0,x31=0,x4=lfiq_value[1],
                             beta_0 = coef_lfiq[1],beta_L = coef_lfiq[2],beta_11 = 0,beta_L11 =0,
                             beta_21 = 0,beta_L21 = 0,beta_31 = 0,beta_L31 = 0,
                             beta_4 = coef_lfiq[3],beta_L4=coef_lfiq[4],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_lfiq1<-sensitivityfun_lfiq1(fpr)
AUC_lfiq1<-round(sum(sensitivity_lfiq1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_lfiq1#######

sensitivityfun_lfiq2<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x21=0,x22=0,x31=0,x4=lfiq_value[2],
                             beta_0 = coef_lfiq[1],beta_L = coef_lfiq[2],beta_11 = 0,beta_L11 =0,
                             beta_21 = 0,beta_L21 = 0,beta_31 = 0,beta_L31 = 0,
                             beta_4 = coef_lfiq[3],beta_L4=coef_lfiq[4],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_lfiq2<-sensitivityfun_lfiq2(fpr)
AUC_lfiq2<-round(sum(sensitivity_lfiq2[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_lfiq2#######0.69 for index

sensitivityfun_lfiq3<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x21=0,x22=0,x31=0,x4=lfiq_value[3],
                             beta_0 = coef_lfiq[1],beta_L = coef_lfiq[2],beta_11 = 0,beta_L11 =0,
                             beta_21 = 0,beta_L21 = 0,beta_31 = 0,beta_L31 = 0,
                             beta_4 = coef_lfiq[3],beta_L4=coef_lfiq[4],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_lfiq3<-sensitivityfun_lfiq3(fpr)
AUC_lfiq3<-round(sum(sensitivity_lfiq3[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_lfiq3#######0.71 for index

# sensitivityfun_lfiq4<-function(prob){
#   sensitivity_1=vector()
#   for (i in 1:length(prob)){
#     sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 0,x21=0,x22=0,x31=0,x4=lfiq_value[4],
#                              beta_0 = coef_lfiq[1],beta_L = coef_lfiq[2],beta_11 = 0,beta_L11 =0,
#                              beta_21 = 0,beta_L21 = 0,beta_31 = 0,beta_L31 = 0,
#                              beta_4 = coef_lfiq[3],beta_L4=coef_lfiq[4],
#                              sigma0 = sigma_0,sigma1 = sigma_1)
#   }
#   return(sensitivity_1)
# }
# sensitivity_lfiq4<-sensitivityfun_lfiq4(fpr)
# AUC_lfiq4<-round(sum(sensitivity_lfiq4[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
# AUC_lfiq4#######0.78 for index


rocdata_lfiq<-data.frame(sensitivity_lfiq1,sensitivity_lfiq2,
                         sensitivity_lfiq3,
                         #sensitivity_lfiq4,
                         sensitivity_pool,fpr)
rocdata_lfiq_melt<-reshape2::melt(rocdata_lfiq, id.var='fpr')
colnames(rocdata_lfiq_melt)[2]<-"LFIQ"
rocdata_lfiq_melt$LFIQ<- ifelse(rocdata_lfiq_melt$LFIQ=="sensitivity_lfiq1",paste0("LFIQ=",lfiq_value[1]," AUC=",AUC_lfiq1),
                              ifelse(rocdata_lfiq_melt$LFIQ=="sensitivity_lfiq2",paste0("LFIQ=",lfiq_value[2]," AUC=",AUC_lfiq2),
                                     ifelse(rocdata_lfiq_melt$LFIQ=="sensitivity_lfiq3",paste0("LFIQ=",lfiq_value[3]," AUC=",AUC_lfiq3),
                                            #ifelse(rocdata_lfiq_melt$LFIQ=="sensitivity_lfiq4",paste0("LFIQ=",lfiq_value[4]," AUC=",AUC_lfiq4),
                                                   paste0("Pooled AUC=",AUCpool)) ))#)
rocdata_lfiq_melt$point<-ifelse(rocdata_lfiq_melt$fpr%in%star,rocdata_lfiq_melt$value,NA)

ggplot(rocdata_lfiq_melt, aes(x=fpr, y=value, type= LFIQ,col=LFIQ)) +
  labs(x="FPR", y="Sensitivity")+
  #geom_vline(data=mu_index, aes(xintercept=grp.mean, color=Label),linetype="solid")+
  geom_line(aes(linetype=LFIQ),size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "solid"))+
  geom_point(aes(y=point,shape=LFIQ),size=3)+
  scale_shape_manual(values=c(15,16,17,NA))+
  ggtitle("Quality Adjusted ROC Curves")+
  theme(legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        legend.position = c(0.75,0.25),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )





###############roc of lfiq & demographics ###########
sensitivityfun_l1_a18_g0_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=0,x22=0,x31=0,x32=0,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a18_g0_e0<-sensitivityfun_l1_a18_g0_e0(fpr)
AUC_l1_a18_g0_e0<-round(sum(sensitivity_l1_a18_g0_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a18_g0_e0#######

sensitivityfun_l1_a18_g0_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=0,x22=0,x31=1,x32=1,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a18_g0_e1<-sensitivityfun_l1_a18_g0_e1(fpr)
AUC_l1_a18_g0_e1<-round(sum(sensitivity_l1_a18_g0_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a18_g0_e1######

sensitivityfun_l1_a40_g0_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x22=0,x31=0,x32=0,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a40_g0_e0<-sensitivityfun_l1_a40_g0_e0(fpr)
AUC_l1_a40_g0_e0<-round(sum(sensitivity_l1_a40_g0_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a40_g0_e0#######

sensitivityfun_l1_a40_g0_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x22=0,x31=1,x32=1,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a40_g0_e1<-sensitivityfun_l1_a40_g0_e1(fpr)
AUC_l1_a40_g0_e1<-round(sum(sensitivity_l1_a40_g0_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a40_g0_e1#######





sensitivityfun_l1_a18_g1_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=1,x22=1,x31=0,x32=0,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a18_g1_e0<-sensitivityfun_l1_a18_g1_e0(fpr)
AUC_l1_a18_g1_e0<-round(sum(sensitivity_l1_a18_g1_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a18_g1_e0#######

sensitivityfun_l1_a18_g1_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=1,x22=1,x31=1,x32=1,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a18_g1_e1<-sensitivityfun_l1_a18_g1_e1(fpr)
AUC_l1_a18_g1_e1<-round(sum(sensitivity_l1_a18_g1_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a18_g1_e1######

sensitivityfun_l1_a40_g1_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=1,x22=1,x31=0,x32=0,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a40_g1_e0<-sensitivityfun_l1_a40_g1_e0(fpr)
AUC_l1_a40_g1_e0<-round(sum(sensitivity_l1_a40_g1_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a40_g1_e0#######

sensitivityfun_l1_a40_g1_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=1,x22=1,x31=1,x32=1,x4=lfiq_value[1],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l1_a40_g1_e1<-sensitivityfun_l1_a40_g1_e1(fpr)
AUC_l1_a40_g1_e1<-round(sum(sensitivity_l1_a40_g1_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l1_a40_g1_e1#######



sensitivityfun_l2_a18_g0_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=0,x22=0,x31=0,x32=0,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a18_g0_e0<-sensitivityfun_l2_a18_g0_e0(fpr)
AUC_l2_a18_g0_e0<-round(sum(sensitivity_l2_a18_g0_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a18_g0_e0#######

sensitivityfun_l2_a18_g0_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=0,x22=0,x31=1,x32=1,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a18_g0_e1<-sensitivityfun_l2_a18_g0_e1(fpr)
AUC_l2_a18_g0_e1<-round(sum(sensitivity_l2_a18_g0_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a18_g0_e1######

sensitivityfun_l2_a40_g0_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x22=0,x31=0,x32=0,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a40_g0_e0<-sensitivityfun_l2_a40_g0_e0(fpr)
AUC_l2_a40_g0_e0<-round(sum(sensitivity_l2_a40_g0_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a40_g0_e0#######

sensitivityfun_l2_a40_g0_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x22=0,x31=1,x32=1,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a40_g0_e1<-sensitivityfun_l2_a40_g0_e1(fpr)
AUC_l2_a40_g0_e1<-round(sum(sensitivity_l2_a40_g0_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a40_g0_e1#######





sensitivityfun_l2_a18_g1_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=1,x22=1,x31=0,x32=0,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a18_g1_e0<-sensitivityfun_l2_a18_g1_e0(fpr)
AUC_l2_a18_g1_e0<-round(sum(sensitivity_l2_a18_g1_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a18_g1_e0#######

sensitivityfun_l2_a18_g1_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 18,x12=18,x21=1,x22=1,x31=1,x32=1,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a18_g1_e1<-sensitivityfun_l2_a18_g1_e1(fpr)
AUC_l2_a18_g1_e1<-round(sum(sensitivity_l2_a18_g1_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a18_g1_e1######

sensitivityfun_l2_a40_g1_e0<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=1,x22=1,x31=0,x32=0,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a40_g1_e0<-sensitivityfun_l2_a40_g1_e0(fpr)
AUC_l2_a40_g1_e0<-round(sum(sensitivity_l2_a40_g1_e0[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a40_g1_e0#######

sensitivityfun_l2_a40_g1_e1<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=1,x22=1,x31=1,x32=1,x4=lfiq_value[3],
                             beta_0 = coef_full[1],beta_L = coef_full[2],
                             beta_11 = coef_full[3],beta_12 = coef_full[4],beta_L11 =coef_full[10],
                             beta_21 = coef_full[5],beta_22 = coef_full[6],beta_L21 = coef_full[11],
                             beta_31 = coef_full[7],beta_32=coef_full[8],beta_L31 = coef_full[12],
                             beta_4 = coef_full[9],beta_L4=coef_full[13],
                             sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_l2_a40_g1_e1<-sensitivityfun_l2_a40_g1_e1(fpr)
AUC_l2_a40_g1_e1<-round(sum(sensitivity_l2_a40_g1_e1[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUC_l2_a40_g1_e1#######


########################## roc curve full: LFIQ & Gender##########################
Gender<- c('Female','Male')

####### LFIQ=1 & Gender=0
rocdata_full_10<-data.frame(sensitivity_l1_a18_g0_e0, sensitivity_l1_a18_g0_e1,
                                   sensitivity_l1_a40_g0_e0, sensitivity_l1_a40_g0_e1,
                                   sensitivity_pool,fpr)
rocdata_full_10_melt<-reshape2::melt(rocdata_full_10, id.var='fpr')
colnames(rocdata_full_10_melt)[2]<-"Demographics"
rocdata_full_10_melt$Demographics<- ifelse(rocdata_full_10_melt$Demographics=="sensitivity_l1_a18_g0_e0",
                                           paste0("Age=18 Non-caucasian AUC=",AUC_l1_a18_g0_e0),
                                           ifelse(rocdata_full_10_melt$Demographics=="sensitivity_l1_a18_g0_e1",
                                                  paste0("Age=18 Caucasian AUC=",AUC_l1_a18_g0_e1),
                                                  ifelse(rocdata_full_10_melt$Demographics=="sensitivity_l1_a40_g0_e0",
                                                         paste0("Age=40 Non-caucasian AUC=",AUC_l1_a40_g0_e0),
                                                         ifelse(rocdata_full_10_melt$Demographics=="sensitivity_l1_a40_g0_e1",
                                                                paste0("Age=40 Caucasian AUC=",AUC_l1_a40_g0_e1),
                                                                       paste0( "Pooled AUC=",AUCpool)))))
rocdata_full_10_melt$point<-ifelse(rocdata_full_10_melt$fpr%in%star,rocdata_full_10_melt$value,NA)
ggplot(rocdata_full_10_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Quality and Demographic Adjusted ROC Curves",subtitle = paste0("LFIQ= ",lfiq_value[1]," Gender=",Gender[1]))+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=11, family="Arial",hjust = 1),
        legend.position = c(0.75,0.3),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )




####### LFIQ=1 & Gender=1
rocdata_full_11<-data.frame(sensitivity_l1_a18_g1_e0, sensitivity_l1_a18_g1_e1,
                            sensitivity_l1_a40_g1_e0, sensitivity_l1_a40_g1_e1,
                            sensitivity_pool,fpr)
rocdata_full_11_melt<-reshape2::melt(rocdata_full_11, id.var='fpr')
colnames(rocdata_full_11_melt)[2]<-"Demographics"
rocdata_full_11_melt$Demographics<- ifelse(rocdata_full_11_melt$Demographics=="sensitivity_l1_a18_g1_e0",
                                           paste0("Age=18 Non-caucasian AUC=",AUC_l1_a18_g1_e0),
                                           ifelse(rocdata_full_11_melt$Demographics=="sensitivity_l1_a18_g1_e1",
                                                  paste0("Age=18 Caucasian AUC=",AUC_l1_a18_g1_e1),
                                                  ifelse(rocdata_full_11_melt$Demographics=="sensitivity_l1_a40_g1_e0",
                                                         paste0("Age=40 Non-caucasian AUC=",AUC_l1_a40_g1_e0),
                                                         ifelse(rocdata_full_11_melt$Demographics=="sensitivity_l1_a40_g1_e1",
                                                                paste0("Age=40 Caucasian AUC=",AUC_l1_a40_g1_e1),
                                                                paste0( "Pooled AUC=",AUCpool)))))
rocdata_full_11_melt$point<-ifelse(rocdata_full_11_melt$fpr%in%star,rocdata_full_11_melt$value,NA)
ggplot(rocdata_full_11_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Quality and Demographic Adjusted ROC Curves",subtitle = paste0("LFIQ= ",lfiq_value[1]," Gender=",Gender[2]))+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=11, family="Arial",hjust = 1),
        legend.position = c(0.75,0.3),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )


####### LFIQ=[4] & Gender=0
rocdata_full_20<-data.frame(sensitivity_l2_a18_g0_e0, sensitivity_l2_a18_g0_e1,
                            sensitivity_l2_a40_g0_e0, sensitivity_l2_a40_g0_e1,
                            sensitivity_pool,fpr)
rocdata_full_20_melt<-reshape2::melt(rocdata_full_20, id.var='fpr')
colnames(rocdata_full_20_melt)[2]<-"Demographics"
rocdata_full_20_melt$Demographics<- ifelse(rocdata_full_20_melt$Demographics=="sensitivity_l2_a18_g0_e0",
                                           paste0("Age=18 Non-caucasian AUC=",AUC_l2_a18_g0_e0),
                                           ifelse(rocdata_full_20_melt$Demographics=="sensitivity_l2_a18_g0_e1",
                                                  paste0("Age=18 Caucasian AUC=",AUC_l2_a18_g0_e1),
                                                  ifelse(rocdata_full_20_melt$Demographics=="sensitivity_l2_a40_g0_e0",
                                                         paste0("Age=40 Non-caucasian AUC=",AUC_l2_a40_g0_e0),
                                                         ifelse(rocdata_full_20_melt$Demographics=="sensitivity_l2_a40_g0_e1",
                                                                paste0("Age=40 Caucasian AUC=",AUC_l2_a40_g0_e1),
                                                                paste0( "Pooled AUC=",AUCpool)))))
rocdata_full_20_melt$point<-ifelse(rocdata_full_20_melt$fpr%in%star,rocdata_full_20_melt$value,NA)
ggplot(rocdata_full_20_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Quality and Demographic Adjusted ROC Curves",subtitle = paste0("LFIQ= ",lfiq_value[3]," Gender=",Gender[1]))+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=11, family="Arial",hjust = 1),
        legend.position = c(0.75,0.3),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )

####### LFIQ=[3] & Gender=1
rocdata_full_21<-data.frame(sensitivity_l2_a18_g1_e0, sensitivity_l2_a18_g1_e1,
                            sensitivity_l2_a40_g1_e0, sensitivity_l2_a40_g1_e1,
                            sensitivity_pool,fpr)
rocdata_full_21_melt<-reshape2::melt(rocdata_full_21, id.var='fpr')
colnames(rocdata_full_21_melt)[2]<-"Demographics"
rocdata_full_21_melt$Demographics<- ifelse(rocdata_full_21_melt$Demographics=="sensitivity_l2_a18_g1_e0",
                                           paste0("Age=18 Non-caucasian AUC=",AUC_l2_a18_g1_e0),
                                           ifelse(rocdata_full_21_melt$Demographics=="sensitivity_l2_a18_g1_e1",
                                                  paste0("Age=18 Caucasian AUC=",AUC_l2_a18_g1_e1),
                                                  ifelse(rocdata_full_21_melt$Demographics=="sensitivity_l2_a40_g1_e0",
                                                         paste0("Age=40 Non-caucasian AUC=",AUC_l2_a40_g1_e0),
                                                         ifelse(rocdata_full_21_melt$Demographics=="sensitivity_l2_a40_g1_e1",
                                                                paste0("Age=40 Caucasian AUC=",AUC_l2_a40_g1_e1),
                                                                paste0( "Pooled AUC=",AUCpool)))))
rocdata_full_21_melt$point<-ifelse(rocdata_full_21_melt$fpr%in%star,rocdata_full_21_melt$value,NA)
ggplot(rocdata_full_21_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Quality and Demographic Adjusted ROC Curves",subtitle = paste0("LFIQ= ",lfiq_value[3]," Gender=",Gender[2]))+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=11, family="Arial",hjust = 1),
        legend.position = c(0.75,0.3),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="./ACM2022/plots")
dev.off()
