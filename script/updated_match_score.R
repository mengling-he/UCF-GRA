#####input data
#setwd("I:/Menglinghe/match")
#right_index_df<-readr::read_csv("./updated_matchscore/Right_index.csv",col_names = TRUE)

##### reform the dataset #####
### index ####
right_index <- right_index_df
#check the row ID duplicated
sum(duplicated(right_index$RID)*1)# no duplicated subject ID

# save the subject ID and demographics
right_index[,1] <- substr(right_index$file_name,1,20)
demo_right_index<- right_index[,c(1,215:218)]
names(demo_right_index)[names(demo_right_index) == 'RID'] <- 'latent_ID';
demo_right_index$roll_ID<-demo_right_index$latent_ID
right_index_score<- right_index[,-c(1,215:218)]
names(right_index_score) <- gsub(",","",substr(names(right_index_score),1,9))
rownames(right_index_score) <- demo_right_index$latent_ID

# dataset for analysis
right_index_final <- as.data.frame(as.table(as.matrix(right_index_score)))
colnames(right_index_final) <- c("latent_ID","roll_ID","max_score")
right_index_final$latent_ID <- as.character(right_index_final$latent_ID);right_index_final$roll_ID <- as.character(right_index_final$roll_ID)
right_index_final$match <- 0 # imposter score
right_index_final[right_index_final$latent_ID==right_index_final$roll_ID,]$match <- 1 # genuine score
library(plyr)
right_index_final <- join(right_index_final, demo_right_index, by = "latent_ID")
right_index_final <- join(right_index_final, demo_right_index, by = "roll_ID")
right_index_final <- right_index_final[,-c(9,10,11)]
colnames(right_index_final) <- c("latent_ID","roll_ID","max_score", "match","full_latent_ID",
                                 "gender","age","ethnicity","max_id_gender","max_id_age","max_id_ethnicity")#max_id_ is the demographics of rolledsubjects 
write.csv(right_index_final,'right_index_final.csv',row.names = F) 





#### right thum ####
#####input data
right_thumb_1_df<-readr::read_csv("./updated_matchscore/Right_thumb_part1.csv",col_names = TRUE)
right_thumb_2_df<-readr::read_csv("./updated_matchscore/Right_thumb_part2.csv",col_names = TRUE)
right_thumb_3_df<-readr::read_csv("./updated_matchscore/Right_thumb_part3.csv",col_names = TRUE)
right_thumb_4_df<-readr::read_csv("./updated_matchscore/Right_thumb_part4.csv",col_names = TRUE)
right_thumb_5_df<-readr::read_csv("./updated_matchscore/Right_thumb_part5.csv",col_names = TRUE)
right_thumb_df2<-readr::read_csv("./updated_matchscore/Right_thumb_of.csv",col_names = TRUE)

right_thumb_1 <- right_thumb_1_df
right_thumb_2 <- right_thumb_2_df
right_thumb_3 <- right_thumb_3_df
right_thumb_4 <- right_thumb_4_df
right_thumb_5 <- right_thumb_5_df
#check the row ID duplicated
sum(duplicated(right_thumb_4$RID)*1)# no duplicated subject ID

# save the subject ID and demographics
demo_right_thumb_1<- right_thumb_1[,c(1,102:105)];names(demo_right_thumb_1)[names(demo_right_thumb_1) == 'RID'] <- 'latent_ID';demo_right_thumb_1$roll_ID<-demo_right_thumb_1$latent_ID;
demo_right_thumb_2<- right_thumb_2[,c(1,102:105)];names(demo_right_thumb_2)[names(demo_right_thumb_2) == 'RID'] <- 'latent_ID';demo_right_thumb_2$roll_ID<-demo_right_thumb_2$latent_ID
demo_right_thumb_3<- right_thumb_3[,c(1,102:105)];names(demo_right_thumb_3)[names(demo_right_thumb_3) == 'RID'] <- 'latent_ID';demo_right_thumb_3$roll_ID<-demo_right_thumb_3$latent_ID
demo_right_thumb_4<- right_thumb_4[,c(1,102:105)];names(demo_right_thumb_4)[names(demo_right_thumb_4) == 'RID'] <- 'latent_ID';demo_right_thumb_4$roll_ID<-demo_right_thumb_4$latent_ID;
demo_right_thumb_5<- right_thumb_5[,c(1,58:61)];names(demo_right_thumb_5)[names(demo_right_thumb_5) == 'RID'] <- 'latent_ID';demo_right_thumb_5$roll_ID<-demo_right_thumb_5$latent_ID

demo_right_thumb <- rbind(demo_right_thumb_1,demo_right_thumb_2,demo_right_thumb_3,demo_right_thumb_4,demo_right_thumb_5)
# 
right_thumb_score_1<- right_thumb_1[,-c(1,102:105)]
names(right_thumb_score_1) <- gsub(",","",substr(names(right_thumb_score_1),1,9))
rownames(right_thumb_score_1) <- demo_right_thumb_1$latent_ID
# dataset for analysis
right_thumb_final_1 <- as.data.frame(as.table(as.matrix(right_thumb_score_1)))
colnames(right_thumb_final_1 ) <- c("latent_ID","roll_ID","max_score")
right_thumb_final_1$latent_ID <- as.character(right_thumb_final_1 $latent_ID);right_thumb_final_1$roll_ID <- as.character(right_thumb_final_1$roll_ID)
right_thumb_final_1$match <- 0 # imposter score
right_thumb_final_1[right_thumb_final_1$latent_ID==right_thumb_final_1$roll_ID,]$match <- 1 # genuine score
library(plyr)
right_thumb_final_1 <- join(right_thumb_final_1, demo_right_thumb_1, by = "latent_ID")
right_thumb_final_1 <- join(right_thumb_final_1, demo_right_thumb_1, by = "roll_ID")
right_thumb_final_1  <-right_thumb_final_1[,-c(9,10,11)]

colnames(right_thumb_final_1) <- c("latent_ID","roll_ID","max_score", "match","full_latent_ID",
                                 "gender","age","ethnicity","max_id_gender","max_id_age","max_id_ethnicity")#max_id_ is the demographics of rolledsubjects 

right_thumb_final_1$full_latent_ID <- substr(right_thumb_final_1$full_latent_ID,1,20)

# 
right_thumb_score_2<- right_thumb_2[,-c(1,102:105)]
names(right_thumb_score_2) <- gsub(",","",substr(names(right_thumb_score_2),1,9))
rownames(right_thumb_score_2) <- demo_right_thumb_2$latent_ID
# dataset for analysis
right_thumb_final_2 <- as.data.frame(as.table(as.matrix(right_thumb_score_2)))
colnames(right_thumb_final_2) <- c("latent_ID","roll_ID","max_score")
right_thumb_final_2$latent_ID <- as.character(right_thumb_final_2$latent_ID);right_thumb_final_2$roll_ID <- as.character(right_thumb_final_2$roll_ID)
right_thumb_final_2$match <- 0 # imposter score
right_thumb_final_2[right_thumb_final_2$latent_ID==right_thumb_final_2$roll_ID,]$match <- 1 # genuine score
library(plyr)
right_thumb_final_2 <- join(right_thumb_final_2, demo_right_thumb_2, by = "latent_ID")
right_thumb_final_2 <- join(right_thumb_final_2, demo_right_thumb_2, by = "roll_ID")
right_thumb_final_2  <-right_thumb_final_2[,-c(9,10,11)]
colnames(right_thumb_final_2) <- c("latent_ID","roll_ID","max_score", "match","full_latent_ID",
                                   "gender","age","ethnicity","max_id_gender","max_id_age","max_id_ethnicity")#max_id_ is the demographics of rolledsubjects 

right_thumb_final_2$full_latent_ID <- substr(right_thumb_final_2$full_latent_ID,1,20)

# 
right_thumb_score_3<- right_thumb_3[,-c(1,102:105)]
names(right_thumb_score_3) <- gsub(",","",substr(names(right_thumb_score_3),1,9))
rownames(right_thumb_score_3) <- demo_right_thumb_3$latent_ID
# dataset for analysis
right_thumb_final_3 <- as.data.frame(as.table(as.matrix(right_thumb_score_3)))
colnames(right_thumb_final_3) <- c("latent_ID","roll_ID","max_score")
right_thumb_final_3$latent_ID <- as.character(right_thumb_final_3$latent_ID);right_thumb_final_3$roll_ID <- as.character(right_thumb_final_3$roll_ID)
right_thumb_final_3$match <- 0 # imposter score
right_thumb_final_3[right_thumb_final_3$latent_ID==right_thumb_final_3$roll_ID,]$match <- 1 # genuine score
library(plyr)
right_thumb_final_3 <- join(right_thumb_final_3, demo_right_thumb_3, by = "latent_ID")
right_thumb_final_3 <- join(right_thumb_final_3, demo_right_thumb_3, by = "roll_ID")
right_thumb_final_3  <-right_thumb_final_3[,-c(9,10,11)]
colnames(right_thumb_final_3) <- c("latent_ID","roll_ID","max_score", "match","full_latent_ID",
                                   "gender","age","ethnicity","max_id_gender","max_id_age","max_id_ethnicity")#max_id_ is the demographics of rolledsubjects 

right_thumb_final_3$full_latent_ID <- substr(right_thumb_final_3$full_latent_ID,1,20)

# 
right_thumb_score_4<- right_thumb_4[,-c(1,102:105)]
names(right_thumb_score_4) <- gsub(",","",substr(names(right_thumb_score_4),1,9))
rownames(right_thumb_score_4) <- demo_right_thumb_4$latent_ID
# dataset for analysis
right_thumb_final_4<- as.data.frame(as.table(as.matrix(right_thumb_score_4)))
colnames(right_thumb_final_4) <- c("latent_ID","roll_ID","max_score")
right_thumb_final_4$latent_ID <- as.character(right_thumb_final_4$latent_ID);right_thumb_final_4$roll_ID <- as.character(right_thumb_final_4$roll_ID)
right_thumb_final_4$match <- 0 # imposter score
right_thumb_final_4[right_thumb_final_4$latent_ID==right_thumb_final_4$roll_ID,]$match <- 1 # genuine score
library(plyr)
right_thumb_final_4 <- join(right_thumb_final_4, demo_right_thumb_4, by = "latent_ID")
right_thumb_final_4 <- join(right_thumb_final_4, demo_right_thumb_4, by = "roll_ID")
right_thumb_final_4  <-right_thumb_final_4[,-c(9,10,11)]
colnames(right_thumb_final_4) <- c("latent_ID","roll_ID","max_score", "match","full_latent_ID",
                                   "gender","age","ethnicity","max_id_gender","max_id_age","max_id_ethnicity")#max_id_ is the demographics of rolledsubjects 

right_thumb_final_4$full_latent_ID <- substr(right_thumb_final_4$full_latent_ID,1,20)

#
right_thumb_score_5<- right_thumb_5[,-c(1,58:61)]
names(right_thumb_score_5) <- gsub(",","",substr(names(right_thumb_score_5),1,9))
rownames(right_thumb_score_5) <- demo_right_thumb_5$latent_ID
# dataset for analysis
right_thumb_final_5<- as.data.frame(as.table(as.matrix(right_thumb_score_5)))
colnames(right_thumb_final_5) <- c("latent_ID","roll_ID","max_score")
right_thumb_final_5$latent_ID <- as.character(right_thumb_final_5$latent_ID);right_thumb_final_5$roll_ID <- as.character(right_thumb_final_5$roll_ID)
right_thumb_final_5$match <- 0 # imposter score
right_thumb_final_5[right_thumb_final_5$latent_ID==right_thumb_final_5$roll_ID,]$match <- 1 # genuine score
library(plyr)
right_thumb_final_5 <- join(right_thumb_final_5, demo_right_thumb_5, by = "latent_ID")
right_thumb_final_5 <- join(right_thumb_final_5, demo_right_thumb_5, by = "roll_ID")
right_thumb_final_5  <-right_thumb_final_5[,-c(9,10,11)]
colnames(right_thumb_final_5) <- c("latent_ID","roll_ID","max_score", "match","full_latent_ID",
                                   "gender","age","ethnicity","max_id_gender","max_id_age","max_id_ethnicity")#max_id_ is the demographics of rolledsubjects 

right_thumb_final_5$full_latent_ID <- substr(right_thumb_final_5$full_latent_ID,1,20)

##ombine 5 dataset in 1 
right_thumb_final<- rbind(right_thumb_final_1,right_thumb_final_2,right_thumb_final_3,right_thumb_final_4,right_thumb_final_5)
write.csv(right_thumb_final,'right_thumb_final.csv',row.names = F) 




























### data cleaning ####

### for index data ####
right_index_final <- readr::read_csv("./updated_matchscore/right_index_final.csv",col_names = TRUE)
#summary(right_index_final$max_score)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -1.825e+36  3.000e+00  6.000e+00 -4.023e+31  9.000e+00  2.154e+08 

# discard negative score #
workfile_4_neg<-subset(right_index_final,max_score <=0) # the negative dataset
workfile_4_extreme<-subset(right_index_final,max_score>=200); # the extreme score dataset
workfile_4_outlier2 <-right_index_final[(right_index_final$full_latent_ID=='9491504_F02_NI_PA_#2'),]
workfile_4_outlier<-right_index_final[right_index_final$max_score <=exp(-1.25)&right_index_final$max_score >0,]# delete those too small outlier
count(workfile_4_outlier2,'match')
workfile_4 <- right_index_final
workfile_4_outlier2<-right_index_final[(right_index_final$full_latent_ID=='9491504_F02_NI_PA_#2'),]
count(workfile_4_outlier2,'match')
workfile_4<-workfile_4[!(workfile_4$full_latent_ID=='9491504_F02_NI_PA_#2'),]

workfile_4_neg<-subset(workfile_4,max_score <=0) # the negative dataset
count(workfile_4_outlier,'match')
workfile_4 <- subset(workfile_4,max_score > 0)##########!!!!! just refit this dataset
workfile_4_extreme<-subset(workfile_4,max_score>=200);
workfile_4<-subset(workfile_4,max_score<200)
#workfile_4<-workfile_4[!(workfile_4$full_latent_ID=='9491504_F02_NI_PA_#2'),]# delete the subject whose age is unknown
workfile_4_outlier<-workfile_4[workfile_4$max_score <=exp(-1.25),]
workfile_4<-workfile_4[workfile_4$max_score >exp(-1.25),]# delete those too small outlier


hist(subset(workfile_4,match=='1')$max_score)
summary(subset(workfile_4,match=='1')$max_score)

hist(subset(workfile_4,match=='0')$max_score)
summary(subset(workfile_4,match=='0')$max_score)


#log transform #max_score0 is the original score, max_score is the log-transformed score.
workfile_4$max_score0<- workfile_4$max_score;
workfile_4$max_score<- log(workfile_4$max_score0)

workfile_4$ethnicity<-ifelse(workfile_4$ethnicity=="Caucasian","Caucasian","Non Caucasian")
workfile_4$max_id_ethnicity<-ifelse(workfile_4$max_id_ethnicity=="Caucasian","Caucasian","Non Caucasian")
count(workfile_4,'ethnicity')
count(workfile_4,'max_id_ethnicity')

workfile_4_genuine <- subset(workfile_4,match=='1')
workfile_4_imposter <- subset(workfile_4,match=='0')

workfile_4_final<-workfile_4
workfile_4_final$gender<-as.numeric(workfile_4_final$gender=="Male")
workfile_4_final$max_id_gender<-as.numeric(workfile_4_final$max_id_gender=="Male")##### if gender=Male,then gender=1, otherwise,gender=0
workfile_4_final$ethnicity<-as.numeric(workfile_4$ethnicity=="Caucasian")
workfile_4_final$max_id_ethnicity<-as.numeric(workfile_4$max_id_ethnicity=="Caucasian")##### if ethnicity=Caucasian,then ethnicity=1, otherwise,ethnicity=0

workfile_4_final$Label<-ifelse(workfile_4_final$match=="1",'Genuine Score','Imposter Score')

count(workfile_4_final,'Label')### delete those negtive values, delete extreme values greater than 200
## index
# Label  freq
# 1  Genuine Score   193
# 2 Imposter Score 40574
summary(subset(workfile_4_final,match=='0')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.8052  1.3835  1.8399  1.8037  2.2476  4.2177
summary(subset(workfile_4_final,match=='1')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1345  1.6400  2.2157  2.4576  3.2227  5.2516 

write.csv(workfile_4_final,'index_clean.csv',row.names = F) 






library(tidyr)
#### for thumb data ####
right_thumb_final <- readr::read_csv("./updated_matchscore/right_thumb_final.csv",col_names = TRUE)

summary(right_thumb_final$max_score)#have NA values
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
# -1.021e+38  6.000e+00  9.000e+00 -4.898e+33  1.400e+01  7.892e+19       1453 
count(right_thumb_final,'match')

right_thumb_NA <- right_thumb_final[is.na(right_thumb_final$max_score),]
# match freq
# 1     0 1449
# 2     1    4
# discard negative score #
workfile_4_neg <-subset(right_thumb_final,max_score <=0) # the negative dataset
workfile_4_outlier <- subset(right_thumb_final,max_score >0& max_score<exp(-1) )
workfile_4 <- subset(right_thumb_final,max_score > exp(-1))##########!!!!! just refit this dataset
workfile_4_extreme<-subset(workfile_4,max_score>=300); # the extreme score dataset
workfile_4<-subset(workfile_4,max_score<300)

hist(subset(workfile_4,match=='1')$max_score)
summary(subset(workfile_4,match=='1')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.332   7.715  22.309  49.038  67.395 294.412 
hist(subset(workfile_4,match=='0')$max_score)
summary(subset(workfile_4,match=='0')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.020   5.559   9.049  10.888  14.134  74.436 


#log transform #max_score0 is the original score, max_score is the log-transformed score.
workfile_4$max_score0<- workfile_4$max_score;
workfile_4$max_score<- log(workfile_4$max_score0)

workfile_4$ethnicity<-ifelse(workfile_4$ethnicity=="Caucasian","Caucasian","Non Caucasian")
workfile_4$max_id_ethnicity<-ifelse(workfile_4$max_id_ethnicity=="Caucasian","Caucasian","Non Caucasian")
count(workfile_4,'ethnicity')
# ethnicity  freq
# 1     Caucasian 31757
# 2 Non Caucasian  9070
count(workfile_4,'max_id_ethnicity')
# max_id_ethnicity  freq
# 1        Caucasian 32040
# 2    Non Caucasian  8787


workfile_4_genuine <- subset(workfile_4,match=='1')
workfile_4_imposter <- subset(workfile_4,match=='0')

workfile_4_final<-workfile_4
workfile_4_final$gender<-as.numeric(workfile_4_final$gender=="Male")
workfile_4_final$max_id_gender<-as.numeric(workfile_4_final$max_id_gender=="Male")##### if gender=Male,then gender=1, otherwise,gender=0
workfile_4_final$ethnicity<-as.numeric(workfile_4$ethnicity=="Caucasian")
workfile_4_final$max_id_ethnicity<-as.numeric(workfile_4$max_id_ethnicity=="Caucasian")##### if ethnicity=Caucasian,then ethnicity=1, otherwise,ethnicity=0

workfile_4_final$Label<-ifelse(workfile_4_final$match=="1",'Genuine Score','Imposter Score')

count(workfile_4_final,'Label')#
# 1  Genuine Score   446
# 2 Imposter Score 41142
summary(subset(workfile_4_final,match=='0')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.908   1.378   1.837   1.790   2.246   4.218 
summary(subset(workfile_4_final,match=='1')$max_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -5.809   1.637   2.215   2.415   3.214   5.252 
write.csv(workfile_4_final,'thumb_clean.csv',row.names = F) 








#############################################Data plot#############################################################
workfile_4_final <- readr::read_csv("./updated_matchscore/index_clean.csv",col_names = TRUE)
workfile_4_final <- readr::read_csv("./updated_matchscore/thumb_clean.csv",col_names = TRUE)
######density plot for match score
sigma_0<-sd(subset(workfile_4_final,match=='0')$max_score)
sigma_1<-sd(subset(workfile_4_final,match=='1')$max_score)
mu <- ddply(workfile_4_final, "Label", summarise, grp.mean=mean(max_score))
set.seed(42)
library(ggplot2)
library(extrafont)
#font_import()
ggplot(workfile_4_final, aes(max_score, fill=Label, color=Label)) +
  geom_histogram(aes(y=..density..), breaks=seq(-1,6,1), alpha=0.8, 
                 position="identity", lwd=0.2) +
  theme_bw() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Label),linetype="dashed")+
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

ggplot(workfile_4_genuine, aes(age, max_score,size=max_score,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='Genuine Score',title ="Genuine Match Scores vs. Age" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 

ggplot(workfile_4_imposter, aes(age, max_score,size=max_score,color=age)) +
  geom_point()+
  theme_bw() + 
  labs(x='Age', y='Genuine Score',title ="Genuine Match Scores vs. Age" )+
  theme(legend.position = "none",
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        text=element_text(size=14, family="Arial"),
        axis.text = element_text(size=14, family="Arial")) 

ggplot(data=workfile_4_genuine, aes(x=max_score, color=gender,fill=gender)) +
  xlim(-2,8)+
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='Genuine Score',y='Density',title ="Distributions of Genuine Match Scores\n based on Gender" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(data=workfile_4_genuine, aes(x=max_score, color=ethnicity,fill=ethnicity)) +
  xlim(-2,8)+
  geom_density(adjust=1.5,alpha=.6) +
  theme_bw() + 
  labs(x='Genuine Score',y='Density',title ="Distributions of Genuine Match Scores\n based on Ethnicity" )+
  theme(legend.background = element_rect(fill="transparent", size=1.5, linetype="solid"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        axis.text = element_text(size=14, family="Arial"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






















################analysis################

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
summary(model_age_output4)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.7775521  0.0123127 144.368  < 2e-16 ***
#   match        0.5890378  0.1333579   4.417 1.00e-05 ***
#   age         -0.0009889  0.0003420  -2.892  0.00383 ** 
#   max_id_age   0.0020197  0.0003246   6.223 4.93e-10 ***
#   match:age    0.0026000  0.0049630   0.524  0.60037    

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.2945420  0.0133770 171.529  < 2e-16 ***
#   match        1.0153423  0.0955540  10.626  < 2e-16 ***
#   age         -0.0068820  0.0003726 -18.472  < 2e-16 ***
#   max_id_age   0.0012915  0.0003680   3.509  0.00045 ***
#   match:age   -0.0027360  0.0036520  -0.749  0.45376  
#####################model with only gender#####
model_gender_output4 <- lm(max_score~match+gender+max_id_gender+gender:match, data=workfile_4_final)
coef_gender<-model_gender_output4$coefficients
summary(model_gender_output4)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.737601   0.006761 257.017  < 2e-16 ***
#   match         0.494380   0.077717   6.361 2.02e-10 ***
#   gender        0.008268   0.006768   1.222  0.22187    
# max_id_gender 0.098636   0.006817  14.469  < 2e-16 ***
#   match:gender  0.258964   0.099068   2.614  0.00895 ** 

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.071253   0.006531 317.163  < 2e-16 ***
#   match         0.787138   0.050435  15.607  < 2e-16 ***
#   gender        0.076785   0.007049  10.893  < 2e-16 ***
#   max_id_gender 0.079835   0.007046  11.331  < 2e-16 ***
#   match:gender  0.297140   0.068412   4.343 1.41e-05 ***

#####################model with only ethnicity#####
model_ethnicity_output4 <- lm(max_score~match+ethnicity+max_id_ethnicity+ethnicity:match, data=workfile_4_final)
coef_ethnicity<-model_ethnicity_output4$coefficients
summary(model_ethnicity_output4)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      1.734339   0.009481 182.930  < 2e-16 ***
#   match            0.498035   0.102157   4.875 1.09e-06 ***
#   ethnicity        0.077954   0.007977   9.772  < 2e-16 ***
#   max_id_ethnicity 0.011102   0.008066   1.376   0.1687    
# match:ethnicity  0.200693   0.115937   1.731   0.0834 .  


####thumb
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       2.230655   0.009552 233.534   <2e-16 ***
#   match             1.033462   0.070154  14.731   <2e-16 ***
#   ethnicity        -0.090543   0.008224 -11.010   <2e-16 ***
#   max_id_ethnicity -0.005624   0.008258  -0.681    0.496    
# match:ethnicity  -0.110552   0.080362  -1.376    0.169    
# ---


#####################model with age and gender (both with interaction with match) use this model
model_full <- lm(max_score~match+age+max_id_age+gender+max_id_gender+age:match+gender:match, data=workfile_4_final)
coef_age_gender_full<-model_full$coefficients
summary(model_full)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.7104813  0.0136940 124.907  < 2e-16 ***
#   match          0.4096939  0.1484999   2.759  0.00580 ** 
#   age           -0.0009839  0.0003411  -2.884  0.00392 ** 
#   max_id_age     0.0020540  0.0003237   6.346 2.24e-10 ***
#   gender         0.0078878  0.0067660   1.166  0.24370    
# max_id_gender  0.0989611  0.0068135  14.524  < 2e-16 ***
#   match:age      0.0033051  0.0049534   0.667  0.50462    
# match:gender   0.2625467  0.0990916   2.650  0.00806 ** 

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    2.1889815  0.0152280 143.747  < 2e-16 ***
#   match          0.8052215  0.1072337   7.509 6.08e-14 ***
#   age           -0.0064293  0.0003750 -17.147  < 2e-16 ***
#   max_id_age     0.0018945  0.0003704   5.115 3.15e-07 ***
#   gender         0.0602353  0.0070887   8.497  < 2e-16 ***
#   max_id_gender  0.0842498  0.0070847  11.892  < 2e-16 ***
#   match:age     -0.0007308  0.0036723  -0.199    0.842    
# match:gender   0.2964801  0.0687336   4.313 1.61e-05 ***



#####################model with age gender and ethnicity (both with interaction with match) use this model
model_4 <- lm(max_score~match+age+max_id_age+gender+max_id_gender+ethnicity+max_id_ethnicity+
                age:match+gender:match+ethnicity:match, data=workfile_4_final)
coef_model_4<-model_4$coefficients
summary(model_4)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       1.6230060  0.0168252  96.463  < 2e-16 ***
#   match             0.2060756  0.1793164   1.149  0.25047    
# age              -0.0008426  0.0003409  -2.471  0.01346 *  
#   max_id_age        0.0020701  0.0003233   6.403 1.54e-10 ***
#   gender            0.0147035  0.0067929   2.165  0.03043 *  
#   max_id_gender     0.1004323  0.0068288  14.707  < 2e-16 ***
#   ethnicity         0.0787202  0.0079989   9.841  < 2e-16 ***
#   max_id_ethnicity  0.0219838  0.0080685   2.725  0.00644 ** 
#   match:age         0.0037247  0.0049508   0.752  0.45185    
# match:gender      0.2803460  0.0993700   2.821  0.00479 ** 
#   match:ethnicity   0.2345408  0.1161018   2.020  0.04338 *  

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       2.2661496  0.0183458 123.524  < 2e-16 ***
#   match             0.8797398  0.1281865   6.863 6.84e-12 ***
#   age              -0.0066801  0.0003751 -17.810  < 2e-16 ***
#   max_id_age        0.0018896  0.0003705   5.100 3.41e-07 ***
#   gender            0.0515345  0.0071209   7.237 4.66e-13 ***
#   max_id_gender     0.0850254  0.0071099  11.959  < 2e-16 ***
#   ethnicity        -0.0915927  0.0082221 -11.140  < 2e-16 ***
#   max_id_ethnicity  0.0040607  0.0082489   0.492    0.623    
# match:age        -0.0009880  0.0036744  -0.269    0.788    
# match:gender      0.2911654  0.0689061   4.226 2.39e-05 ***
#   match:ethnicity  -0.0851134  0.0802144  -1.061    0.289    


################################################### ROC Curve #########################################################

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
AUCpool#

star<-c(0.25,0.50,0.75)
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
AUCgender0#######is 0.64 when gender1=0 for index

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
AUCgender1#######is 0.71 when gender1=1 for index

rocdata_gender<-data.frame(sensitivity_gender1,sensitivity_gender0,sensitivity_pool,fpr)


rocdata_gender_melt<-reshape2::melt(rocdata_gender, id.var='fpr')
colnames(rocdata_gender_melt)[2]<-"Gender"
rocdata_gender_melt$Gender<- ifelse(rocdata_gender_melt$Gender=="sensitivity_gender1",paste0("Male AUC=",AUCgender1),
                                    ifelse(rocdata_gender_melt$Gender=="sensitivity_gender0",paste0("Female AUC=",AUCgender0),paste0("Pooled AUC=",AUCpool)))
rocdata_gender_melt$point<-ifelse(rocdata_gender_melt$fpr%in%star,rocdata_gender_melt$value,NA)
rocdata_gender_melt$point<-ifelse(rocdata_gender_melt$Gender=="Pooled AUC=0.75",NA,rocdata_gender_melt$point)  


ggplot(rocdata_gender_melt, aes(x=fpr,type= Gender,col=Gender)) +
  labs(x="FPR", y="Sensitivity")+ 
  geom_line(aes(y=value,linetype=Gender),size=1.2)+
  scale_linetype_manual(values=c("dotted","longdash","solid"))+
  geom_point(aes(y=point,shape=Gender),size=3)+
  scale_shape_manual(values=c(15,16,NA))+##point shape
  ggtitle("Gender-Adjusted ROC Curves")+
  theme(#legend.background = element_rect(fill="transparent",size=0.5, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        legend.position = c(0.75,0.2),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )


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
AUCage18#######0.68 for index

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
#plot(x=fpr,y=sensitivity_age25,ylim = c(0,1),main="age1=25")
AUCage25<-round(sum(sensitivity_age25[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage25#0.69

sensitivityfun_age40<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 40,x12=40,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age40<-sensitivityfun_age40(fpr)
#plot(x=fpr,y=sensitivity_age40,ylim = c(0,1),main="age1=25")
AUCage40<-round(sum(sensitivity_age40[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage40#######0.7 for index

sensitivityfun_age60<-function(prob){
  sensitivity_1=vector()
  for (i in 1:length(prob)){
    sensitivity_1[i]<-roc_fn(p=prob[i],x11 = 60,x12=60,x21=0,x31=0,beta_0 = coef_age[1],beta_L = coef_age[2],beta_11 = coef_age[3],beta_L11 =coef_age[5],
                             beta_21 = 0,beta_L21 = 0,beta_12 = coef_age[4],sigma0 = sigma_0,sigma1 = sigma_1)
  }
  return(sensitivity_1)
}
sensitivity_age60<-sensitivityfun_age60(fpr)
#plot(x=fpr,y=sensitivity_age60,ylim = c(0,1),main="age1=25")
AUCage60<-round(sum(sensitivity_age60[1:length(fpr)]*diff(c(0, fpr[1:length(fpr)]))),2)
AUCage60#######0.71

rocdata_age<-data.frame(sensitivity_age18,sensitivity_age25,
                        sensitivity_age40,sensitivity_age60,sensitivity_pool,fpr)####omit age=20
rocdata_age_melt<-reshape2::melt(rocdata_age, id.var='fpr')
colnames(rocdata_age_melt)[2]<-"Age"
rocdata_age_melt$Age<- ifelse(rocdata_age_melt$Age=="sensitivity_age18",paste0("Age=18 AUC=",AUCage18),
                              ifelse(rocdata_age_melt$Age=="sensitivity_age25",paste0("Age=25 AUC=",AUCage25),
                                     ifelse(rocdata_age_melt$Age=="sensitivity_age40",paste0("Age=40 AUC=",AUCage40),
                                            ifelse(rocdata_age_melt$Age=="sensitivity_age60",paste0("Age=60 AUC=",AUCage60),
                                                   paste0("Pooled AUC=",AUCpool)) )))
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
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )

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
rocdata_ethnicity_melt$Ethnicity<- ifelse(rocdata_ethnicity_melt$Ethnicity=="sensitivity_ethnicity1",
                                          paste0("Caucasian AUC=",AUC_ethn1),
                                          ifelse(rocdata_ethnicity_melt$Ethnicity=="sensitivity_ethnicity0",
                                                 paste0("Non caucasian AUC=",AUC_ethn0),
                                                 paste0("Pooled AUC=",AUCpool)))
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
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )

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
rocdata_age_gender_melt$Demographics<- ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_40_1",paste0("Male Age=40 AUC=",AUC_40_1),
                                              ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_40_0",paste0("Female Age=40 AUC=",AUC_40_0),
                                                     ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_25_1",paste0("Male Age=25 AUC=",AUC_25_1),
                                                            ifelse(rocdata_age_gender_melt$Demographics=="sensitivity_25_0",paste0("Female Age=25 AUC=",AUC_25_0),
                                                                   paste0("Pooled AUC=",AUCpool))
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
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )


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

#######roc curve for ethnity=noncaucasian
rocdata_3demographics0<-data.frame(sensitivity_40_1_0,sensitivity_40_0_0,
                                   sensitivity_25_1_0,sensitivity_25_0_0,
                                   sensitivity_pool,fpr)
rocdata_3demographics0_melt<-reshape2::melt(rocdata_3demographics0, id.var='fpr')
colnames(rocdata_3demographics0_melt)[2]<-"Demographics"
rocdata_3demographics0_melt$Demographics<- ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_40_1_0",paste0("Male Non-caucasian Age=40 AUC=",AUC_40_1_0),
                                                  ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_40_0_0",paste0("Female Non-caucasian Age=40 AUC=",AUC_40_0_0),
                                                         ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_25_1_0",paste0("Male Non-caucasian Age=25 AUC=",AUC_25_1_0),
                                                                ifelse(rocdata_3demographics0_melt$Demographics=="sensitivity_25_0_0",paste0("Female Non-caucasian Age=25 AUC=",AUC_25_0_0),
                                                                       paste0("Pooled AUC=",AUCpool)))))
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
        legend.position = c(0.60,0.25),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )

#######roc curve for ethnity=caucasian
rocdata_3demographics1<-data.frame(sensitivity_40_1_1,sensitivity_40_0_1,
                                   sensitivity_25_1_1,sensitivity_25_0_1,
                                   sensitivity_pool,fpr)
rocdata_3demographics1_melt<-reshape2::melt(rocdata_3demographics1, id.var='fpr')
colnames(rocdata_3demographics1_melt)[2]<-"Demographics"
rocdata_3demographics1_melt$Demographics<- ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_40_1_1",paste0("Male Caucasian Age=40 AUC=",AUC_40_1_1),
                                                  ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_40_0_1",paste0("Female Caucasian Age=40 AUC=",AUC_40_0_1),
                                                         ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_25_1_1",paste0("Male Caucasian Age=25 AUC=",AUC_25_1_1),
                                                                ifelse(rocdata_3demographics1_melt$Demographics=="sensitivity_25_0_1",paste0("Female Caucasian Age=25 AUC=",AUC_25_0_1),
                                                                       paste0( "Pooled AUC=",AUCpool)))))
rocdata_3demographics1_melt$point<-ifelse(rocdata_3demographics1_melt$fpr%in%star,rocdata_3demographics1_melt$value,NA)
ggplot(rocdata_3demographics1_melt, aes(x=fpr, y=value, type= Demographics,col=Demographics))+
  labs(x="FPR", y="Sensitivity")+ geom_line(aes(linetype=Demographics), size=1.2)+
  scale_linetype_manual(values=c("dashed","dotted","longdash", "dotdash","solid"))+
  geom_point(aes(y=point,shape=Demographics),size=3)+
  scale_shape_manual(values=c(15,16,17,18,NA))+
  ggtitle("Gender, Age and Ethnicity Ajusted ROC Curves",subtitle = "Ethnicity=Caucasian")+
  theme(legend.background = element_rect(fill="transparent",size=12, linetype="solid"),
        text=element_text(size=14, family="Arial"),
        plot.title = element_text(size=14, family="Arial",hjust = 0.5),
        plot.subtitle = element_text(size=12, family="Arial",hjust = 1),
        legend.position = c(0.60,0.25),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10,family="Arial"),
        axis.text = element_text(size=10, family="Arial"),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank()
  )
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


######ROC variance part refer to file"variability_part_code"
